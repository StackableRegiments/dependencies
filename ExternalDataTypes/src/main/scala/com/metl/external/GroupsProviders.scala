package com.metl.external

import com.metl.PeriodicallyRefreshingVar

import scala.io.Source
import net.liftweb.common.Logger
import net.liftweb.util.Helpers._

abstract class PeriodicallyRefreshingGroupsProvider[T](override val storeId:String,override val name:String,refreshPeriod:TimeSpan) extends GroupsProvider(storeId,name) {
  protected val timespan = refreshPeriod
  protected var lastModified:Long = 0
  protected def startingValue:T
  protected var lastCache:T = startingValue
  protected var cache = new PeriodicallyRefreshingVar[Unit](timespan,() => {
    if (shouldCheck){
      val newCheck = new java.util.Date().getTime()
      lastCache = actuallyFetchGroups
      lastModified = newCheck
    }
  })
  protected def shouldCheck:Boolean
  protected def actuallyFetchGroups:T
  protected def parseStore(username:String,store:T):List[OrgUnit]
  override def getGroupsFor(userData:LiftAuthStateData):List[OrgUnit] = parseStore(userData.username,lastCache)
}

abstract class PeriodicallyRefreshingFileReadingGroupsProvider[T](override val storeId:String,override val name:String,path:String,refreshPeriod:TimeSpan) extends PeriodicallyRefreshingGroupsProvider[T](storeId,name,refreshPeriod) {
  override def shouldCheck = {
    val newCheck = new java.io.File(path).lastModified()
    newCheck > lastModified
  }
}

abstract class PerUserFlatFileGroupsProvider(override val storeId:String,override val name:String,path:String,refreshPeriod:TimeSpan) extends PeriodicallyRefreshingFileReadingGroupsProvider[Map[String,List[OrgUnit]]](storeId,name,path,refreshPeriod) {
  override def startingValue = Map.empty[String,List[OrgUnit]]
  override protected def parseStore(username:String,store:Map[String,List[OrgUnit]]):List[OrgUnit] = store.get(username).getOrElse(Nil)
}

class StLeoFlatFileGroupsProvider(override val storeId:String,override val name:String,path:String,refreshPeriod:TimeSpan, facultyWhoWantSubgroups:List[String] = List.empty[String]) extends PerUserFlatFileGroupsProvider(storeId,name,path,refreshPeriod) with Logger {
  info("created new stLeoFlatFileGroupsProvider(%s,%s)".format(path,refreshPeriod))
  override val canQuery:Boolean = false
  override def getAllOrgUnits:List[OrgUnit] = List()
  override def getOrgUnit(name:String):Option[OrgUnit] = None
  override def actuallyFetchGroups:Map[String,List[OrgUnit]] = {
    var rawData = Map.empty[String,List[OrgUnit]]
    Source.fromFile(path).getLines.foreach(line => {
      //sometimes it comes as a csv and other times as a tsv, so converting commas into tabs to begin with
      line.replace(",","\t").split("\t") match {
        case Array(facId,_facFirstName,_facSurname,facUsername,course,section,studentId,studentFirstName,studentSurname,studentUsername,studentStatus) => {
          val subgroups:List[OrgUnit] = facultyWhoWantSubgroups.find(f => f == facUsername).map(f => OrgUnit("ou","%s and %s".format(f,studentUsername))).toList
          studentStatus match {
            case "ACTIVE" => {
              val stuMember = Member(studentUsername,List("firstName" -> studentFirstName,"surname" -> studentSurname).map(t => Detail(t._1,t._2)),Some(ForeignRelationship(storeId,studentId)))
              rawData = rawData.updated(studentUsername,(List(OrgUnit("course",course,List(stuMember),List(GroupSet("section",section,List(stuMember)),GroupSet("ou","%s_%s".format(course,section),List(stuMember))))) ::: subgroups ::: rawData.get(studentUsername).toList.flatten).distinct)
            }
            case _ =>  {}
          }
          val facMember = Member(facUsername,List("firstName" -> _facFirstName,"surname" -> _facSurname).map(t => Detail(t._1,t._2)),Some(ForeignRelationship(storeId,facId)))
          rawData = rawData.updated(facUsername,(List(OrgUnit("course",course,List(facMember),List(GroupSet("section",section,List(facMember)),GroupSet("ou","%s_%s".format(course,section),List(facMember))))) ::: subgroups ::: rawData.get(facUsername).toList.flatten).distinct)
        }
        case _ => {}
      }
    })
    debug("loaded groupData for %s: %s".format(path,rawData))
    rawData
  }
}
