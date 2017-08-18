package com.metl.datamigrator

import com.metl.data._
import com.metl.h2._
import com.metl.metl2011._
import dispatch.Defaults._
import dispatch._
import net.liftweb.actor._
import net.liftweb.common._
import net.liftweb.util.Helpers._
import net.liftweb.util._

import scala.Option
import scala.xml._

object Main extends App with Logger {
  def generatePrivateListFunc(config:ServerConfiguration):Conversation=>Option[List[String]] = {
      config match {
        /*
        case cm2011:MeTL2011BackendAdaptor => (conv:Conversation) => {
          None // this is where the parse through the content will occur, so that we can determine who's made private or public actions in these conversations, so that we don't lose their content.
          val rootHost = cm2011.host
        }
        */
        case sa:ReadOnlyMeTL2011ZipAdaptor => {
          (conv:Conversation) => Some((sa.getPrivateAuthorsForConversation(conv.jid.toString) ::: sa.getHistory(conv.jid.toString).getAll.map(_.author)).distinct)
        }
        case _ => (conv:Conversation) => None
      }
    }
    /*
    val runningFromConsole:Boolean = args.toList.exists(_.split("=") match {
      case Array("runningFromConsole", value) => value.toLowerCase.trim.toBoolean == true
      case _ => false
    })
    */
    val start = new java.util.Date().getTime
    val maintainKeys = new MaintainKeys(start)
    protected def mark(msg:String): Unit = {
      info("[%sms] %s".format(new java.util.Date().getTime - start,msg))
    }
    val configurationFileLocation = System.getProperty("metlx.configurationFile")
    MeTL2011ServerConfiguration.initialize
    MeTL2015ServerConfiguration.initialize
    LocalH2ServerConfiguration.initialize
    ServerConfiguration.addServerConfigurator(ReadOnlyMeTL2011ZipAdaptorConfigurator)
    ServerConfiguration.loadServerConfigsFromFile(
      path = configurationFileLocation,
      onConversationDetailsUpdated = (c:Conversation) => {},
      messageBusCredentailsFunc = () => ("",""),
      conversationListenerCredentialsFunc = () => ("",""),
      httpCredentialsFunc = () => ("","")
    )
    val servers = ServerConfiguration.getServerConfigurations
    val configFile = XML.load(configurationFileLocation)
    val parallelism: Option[Option[Int]] = try {
      Some((configFile \\ "parallelism").headOption.map(_.text.toInt))
    } catch {
      case e:Exception => None
    }
    val importDescription = for {
      idNode <- configFile \\ "importDescription"
    } yield {
      idNode.text
    }
    val (exportXmlEnabled,exportXmlLocation) = {
      val exportXmlNode = configFile \\ "exportXml"
      (exportXmlNode \@ "enabled" match {
        case s:String if s == "true" => true
        case _ => false
      },exportXmlNode \@ "location")
    }
    val sortConversations:List[Conversation]=>List[Conversation] = {
      val priorityElems = configFile \\ "priorities" \ "priority"
      val doAllAfter = (configFile \\ "priorities" \ "@completeLoadAfterPriorities").headOption.map(_.text.toBoolean).getOrElse(true)
      val jidPriorities = priorityElems.flatMap(elem => (elem \ "@jid").headOption.map(_.text.toInt))
      val authorPriorities = priorityElems.flatMap(elem => (elem \ "@author").headOption.map(_.text))
      (in:List[Conversation]) => {
        val (jidMatches,nonJid) = in.sortWith((a,b) => a.lastAccessed > b.lastAccessed).partition(c => jidPriorities.contains(c.jid))
        val (authorMatches,nonAuthor) = nonJid.partition(c => authorPriorities.contains(c.author))
        jidMatches ::: authorMatches ::: nonAuthor.filter(_i => doAllAfter)
      }
    }
    val (targetServer,cookieKey,cookieValue):(Option[String],String,String) = {
      configFile \\ "targetServer" match {
        case cn: NodeSeq if cn.text != "" =>
          (
            Some(cn.text),
            (cn \\ "@cookieKey").headOption.map(_.text).getOrElse("JSESSIONID"),
            (cn \\ "@cookieValue").headOption.map(_.text).getOrElse({
              throw new Exception("please specify the target server's cookie in the configuration file")
            })
          )
        case _ => (None, "", "")
      }
    }
    if(targetServer.isDefined){
      maintainKeys ! AddKey(targetServer.get,cookieKey,cookieValue)
    }
    mark("servers: %s => %s".format(servers,targetServer))
    servers.filterNot(_ == EmptyBackendAdaptor).foreach(config => {
      mark("exporting server: %s".format(config))
      config.isReady
      val timezoneOverrides = (for {
        dtf <- configFile \\ "dateTimeFormat"
        format <- (dtf \ "@format").headOption.map(_.text).filterNot(_ == "")
        zoneId = (dtf \ "@zoneId").headOption.map(_.text).filterNot(_ == "")
      } yield {
        zoneId match {
          case Some(z) => Right((format,z))
          case None => Left(format)
        }
      }).toList
      val exportSerializer = new MigratorXmlSerializer(config.name,timezoneOverrides)
      val privateFunc = generatePrivateListFunc(config)
      def exportConversation(onBehalfOfUser:String,conversation:String):Box[NodeSeq] = {
        tryo((for (
          conv <- Some(config.detailsOfConversation(conversation));
          if (onBehalfOfUser == conv.author);
          histories = exportHistories(conv,privateFunc(conv));
          xml = {
            <export>
            {exportSerializer.fromConversation(conv)}
            <histories>{histories.map(h => exportSerializer.fromHistory(h))}</histories>
            </export>
          }
        ) yield {
          if( exportXmlEnabled )
            XML.save(exportXmlLocation + "/" + conv.jid.toString + ".xml",xml,"UTF-8")
          xml
        }).head)
      }
      def time[A](label:String,action: => A):A = {
        val s = new java.util.Date().getTime
        val res = action
        info(label + "(%s ms)".format(new java.util.Date().getTime - s))
        res
      }
      def exportHistories(conversation:Conversation,restrictToPrivateUsers:Option[List[String]]):List[History] = {
        var convHistory = time("getHistory(%s)".format(conversation.jid),config.getHistory(conversation.jid.toString).filter(m => {
          restrictToPrivateUsers.map(users => {
            m match {
              case q:MeTLQuiz => true
              case mcc:MeTLCanvasContent => mcc.privacy == Privacy.PUBLIC || users.contains(mcc.author)
              case ms:MeTLStanza => users.contains(ms.author)
            }
          }).getOrElse(true)
        }))
        val attendees = convHistory.getAttendances.map(_.author).distinct
        val participants = (attendees ::: restrictToPrivateUsers.getOrElse(List.empty[String])).distinct.filter(u => restrictToPrivateUsers.map(_.contains(u)).getOrElse(true))
        val histories = conversation.slides.flatMap(slide => {
          val slideJid = slide.id.toString
          val publicHistory = config.getHistory(slideJid)
          val privateHistories = participants.map(p => {
            config.getHistory(slideJid + p)
          })
          val allHistories = publicHistory :: privateHistories
          allHistories.flatMap(_.getAll).groupBy(_.author).foreach(authorStanzas => {
            if (!attendees.contains(authorStanzas._1)){
              val times = authorStanzas._2.map(_.timestamp)
              val entering = Attendance(EmptyBackendAdaptor,authorStanzas._1,times.min,slideJid,present = true,Nil)
              val leaving = Attendance(EmptyBackendAdaptor,authorStanzas._1,times.max,slideJid,present = true,Nil)
              convHistory.addAttendance(entering)
              convHistory.addAttendance(leaving)
            }
          })
          allHistories
        })
        convHistory :: histories
      }
      val rawConvs = config.searchForConversation("")//.take(25) // hopefully every conversation will be returned by this query?  Will probably have to write an explicit "getAllConversations" call into the backends.
      val convs = sortConversations(rawConvs)
      mark("fetched conversations: %s => %s".format(rawConvs.length, convs.length))
      val p = convs.par
      parallelism.foreach(paraCount => {
        p.tasksupport = new scala.collection.parallel.ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(paraCount.getOrElse(1)))
      })
      val completed = p.map(conversation => {
        val es = new java.util.Date().getTime
        time("exporting conversation",exportConversation(conversation.author,conversation.jid.toString)).map(xml => {
          val ee = new java.util.Date().getTime
          mark("exported conversation: %s (%s) %s".format(conversation.author, conversation.slides.length, conversation.title))
          val is = new java.util.Date().getTime

          var res:Either[Throwable,Elem] = Left(new Exception("initialiser"))
          if( targetServer.isDefined) {
            val svc = url("%s/conversationImport".format(targetServer.get)).POST << xml.toString <:< Map("Cookie" -> "%s=%s".format(cookieKey, cookieValue)) <<? Map(importDescription.map(id => ("importDescription", id)).toList: _*)

            val result = Http(svc OK as.xml.Elem).either
            res = result()
            //res = Left(new Exception("deliberately failing"))
          }
          else {
            res = Right(<exportOnly/>)
          }
          val ie = new java.util.Date().getTime
          val exportTime = ee - es
          val importTime = ie - is
          val total = ie - es
          res.right.map(crx => {
            mark("successful conversation push: %s (%s) %s => (%sB)".format(conversation.author, conversation.slides.length, conversation.title, crx.toString.length))
          }).left.map(e => {
            error("exception while pushing conversation: %s (%s) %s: %s".format(conversation.author, conversation.slides.length, conversation.title, e.getMessage), e)
          })
          (conversation.jid,conversation.created,res.isRight,exportTime,importTime,total)
        }) match {
          case Full(r) => r
          case Empty => {
            val now = new java.util.Date().getTime
            error("exception %s while generating export xml, resulting in empty".format(conversation.jid))
            (conversation.jid,conversation.created,false,es,now,now - es)
          }
          case ParamFailure(msg,exception,chain,param) => {
            val now = new java.util.Date().getTime
            error("exception %s while generating export xml: %s (%s)\r\n%s => %s".format(
              conversation.jid,
              msg,
              param,
              exception.map(_.getMessage),
              exception.map(_.getStackTraceString)
            ))
            (conversation.jid,conversation.created,false,es,now,now - es)
          }
          case Failure(msg,exception,chain) => {
              val now = new java.util.Date().getTime
              error("exception %s while generating export xml: %s\r\n%s => %s".format(
                conversation.jid,
                msg,
                exception.map(_.getMessage),
                exception.map(_.getStackTraceString)
              ))
              (conversation.jid,conversation.created,false,es,now,now - es)
          }
        }
      }).toList
      mark("completed pushing conversations: \r\n%s".format(completed.map(c => "CONV %s @ '%s' [%s (%s + %s)]: %s".format(c._1,c._2,c._6,c._4,c._5,c._3)).mkString("\r\n")))
      if(targetServer.isDefined) {
        maintainKeys ! RemoveKey(targetServer.get, cookieKey, cookieValue)
      }
      config.shutdown
    })
    maintainKeys.shutdown
    Schedule.shutdown()
    LAScheduler.shutdown()
    mark("finished reading.")
    System.exit(0)
}

case class AddKey(h:String,k:String,v:String)
case class RemoveKey(h:String,k:String,v:String)
class MaintainKeys(start:Long) extends LiftActor with Logger {
  def shutdown = {
    shouldCheck = false
  }
  protected var shouldCheck = true
  case object Ping
  var keys:List[Tuple3[String,String,String]] = Nil
  def repeat = {
    if (shouldCheck){
      Schedule.schedule(this,Ping,10 seconds)
    }
  }
  def messageHandler = {
    case AddKey(host,key,value) => {
      keys = (host,key,value) :: keys
      repeat
    }
    case RemoveKey(host,key,value) => {
      keys = keys.filterNot(k => k._1 == host && k._2 == key && k._3 == value)
      repeat
    }
    case Ping => {
      if (shouldCheck) {
        keys.foreach(k => {
          val svc = url("%s/authenticationState".format(k._1)).GET <:< Map("Cookie" -> "%s=%s".format(k._2, k._3))
          val result = Http(svc OK as.String).either.right.map(r => (XML.loadString(r) \\ "@id").exists(_.text.toString == "authData"))
          info("[%sms] maintainingKey: %s".format(new java.util.Date().getTime - start, result()))
        })
        repeat
      }
    }
  }
}