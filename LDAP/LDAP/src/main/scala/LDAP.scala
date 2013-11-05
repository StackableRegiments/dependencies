package com.metl.ldap

import com.metl.utils._

import org.apache.commons.io._
import javax.naming._
import javax.naming.directory._
import net.liftweb.common._
import net.liftweb.util.Helpers._

trait IMeTLLDAP {
  def authenticate(username:String,password:String):Option[Boolean]
	def simpleQuery(attrName:String,attrValue:String,returnType:String):List[String] 
	def getAuthcateFromId(id:String):List[String] 
	def getIdFromAuthcate(id:String):List[String]
	def getTeachersForSubject(unitCode:String):List[String] 
	def getStudentsForSubject(unitCode:String):List[String]
	
	def getEligibleGroups(id:String):Option[Seq[Tuple2[String,String]]] 
	def getInformationGroups(id:String):Option[Seq[Tuple2[String,String]]] 

  def ou(names: Seq[String]): Map[String, Seq[(String,String)]] 
  def info(names: Seq[String]): Map[String, Seq[(String,String)]]  
  def humanNames(names: Seq[String]): Map[String, String] 
}

object DisconnectedLDAP extends IMeTLLDAP {
  override def authenticate(username:String,password:String) = None
 	override def simpleQuery(attrName:String,attrValue:String,returnType:String):List[String] = List.empty[String]
	override def getAuthcateFromId(id:String):List[String] = List.empty[String]
	override def getIdFromAuthcate(id:String):List[String] = List.empty[String]
	override def getTeachersForSubject(unitCode:String):List[String] = List.empty[String]
	override def getStudentsForSubject(unitCode:String):List[String] = List.empty[String]
	
	override def getEligibleGroups(id:String):Option[Seq[Tuple2[String,String]]] = Some(Seq.empty[Tuple2[String,String]])
	override def getInformationGroups(id:String):Option[Seq[Tuple2[String,String]]] = Some(Seq.empty[Tuple2[String,String]])

  override def ou(names: Seq[String]): Map[String, Seq[(String,String)]] = Map(names.map(n => (n,Seq.empty[(String,String)])):_*)
  override def info(names: Seq[String]): Map[String, Seq[(String,String)]] = Map(names.map(n => (n,Seq.empty[(String,String)])):_*)
  override def humanNames(names: Seq[String]): Map[String, String] = Map(names.map(n => (n,n)):_*)
}

trait LDAPSearch {

  def withLDAP(searchTerm:String,action:(List[SearchResult])=>Unit):Unit
  def withLDAPUsingCredentials(user:String,password:String,searchTerm:String,action:(List[SearchResult])=>Unit,base:String,usernameIncludesBase:Boolean):Unit
}

class LDAPSearchService(directory:String,bindUser:String,password:String) extends LDAPSearch {

  def withLDAPUsingCredentials(user:String,pass:String,searchTerm:String, action:(List[SearchResult])=> Unit,base:String = "o=Monash University, c=AU",usernameIncludesBase:Boolean = true):Unit = Stopwatch.time("LDAP.withLDAPUsingCredentials", () => {
    var env = new java.util.Hashtable[String,String]()
    val constructedUsername = usernameIncludesBase match {
      case true => user
      case false => "%s, %s".format(user,base)
    }
		env.put(Context.INITIAL_CONTEXT_FACTORY,"com.sun.jndi.ldap.LdapCtxFactory")
		env.put(Context.PROVIDER_URL,directory)
		env.put(Context.SECURITY_AUTHENTICATION,"simple")
		env.put(Context.SECURITY_PRINCIPAL,constructedUsername)
		env.put(Context.SECURITY_CREDENTIALS,pass)
		try
			{
				val ctx = new InitialDirContext(env)
				val controls = new SearchControls
				controls.setSearchScope(SearchControls.SUBTREE_SCOPE)
				val results = ctx.search(base,searchTerm,controls)
				var resultEnumerations = List.empty[SearchResult]
				while (results.hasMore){
					resultEnumerations = results.next() :: resultEnumerations
				}
			results.close
			ctx.close
			action(resultEnumerations)
		}
		catch
		{ 
			case e:Throwable => {
				//println("ldap search threw: "+e.getMessage)
				//e.printStackTrace
			}
		}
	})

  def withLDAP(searchTerm:String, action:(List[SearchResult])=> Unit): Unit = Stopwatch.time("LDAP.withLDAP", () => {
    withLDAPUsingCredentials(bindUser,password,searchTerm,action)
	})
}

class LDAPService(env: { val ldapSearch: LDAPSearch }) {
  def withLDAP(searchTerm: String, action: (List[SearchResult]) => Unit): Unit = {
    env.ldapSearch.withLDAP(searchTerm, action)
  }
  def withLDAPUsingCredentials(username:String,password:String, searchTerm:String,action: (List[SearchResult]) => Unit,base:String,usernameIncludesBase:Boolean): Unit = {
    env.ldapSearch.withLDAPUsingCredentials(username,password,searchTerm,action,base,usernameIncludesBase) 
  }
}

class LDAPConfig(directory:String,bindUser:String,password:String) {
  lazy val ldapSearch = new LDAPSearchService(directory,bindUser,password)
  lazy val ldap = new LDAPService(this)
}

class LDAP(env : { val ldap: LDAPService }) extends IMeTLLDAP {
  val groups = Seq("ou","monashenrolledsubject","monashteachingcommitment","o","c")
  val infoGroups = Seq("sn","givenname","initials","mail","cn","jpegphoto","gender","personaltitle","employeenumber")

  override def authenticate(username:String,password:String):Option[Boolean] = Stopwatch.time("LDAP.authenticate", () => {
    //println("LDAP - auth begin")
    var output:Option[Boolean] = None
    val usernameCheckType = "uid"
    var recordOutput:Option[String] = None
    val func = (n:List[SearchResult]) => {
      //println("first pass raw result: %s".format(n))
			recordOutput = n.take(1).map(ne => ne.getName).headOption
      //println("first pass response: %s".format(recordOutput))
    }
    env.ldap.withLDAP("(%s=%s)".format(usernameCheckType,username),func)
    val func2 = (n:List[SearchResult]) => {
      //println("second pass raw result: %s".format(n))
      output = Some(n.length > 0)
      //println("second pass response: %s".format(output))
    }
    recordOutput.map(dn => {
      env.ldap.withLDAPUsingCredentials(dn,password,"(%s=%s)".format(usernameCheckType,username),func2,"o=Monash University, c=AU",false)
    })
    //println("LDAP - auth ended: %s".format(output))
    output
  })

	override def simpleQuery(attrName:String,attrValue:String,returnType:String):List[String] = Stopwatch.time("LDAP.simpleQuery", () => {
		var output = List.empty[String]
		val func = (n:List[SearchResult]) => {
			output = n.map(ne => tryo(namingEnumerationToSeq(returnType,ne.getAttributes().get(returnType).getAll).map(a => a._2)).openOr(List.empty[String]).toList).flatten
		}
		env.ldap.withLDAP("(%s=%s)".format(attrName,attrValue),func)
		output
	})

	override def getAuthcateFromId(id:String):List[String] = Stopwatch.time("LDAP.getAuthcateFromId", () => simpleQuery("employeenumber",id,"uid"))
	override def getIdFromAuthcate(id:String):List[String] = Stopwatch.time("LDAP.getIdFromAuthcate", () => simpleQuery("uid",id,"employeenumber"))
	override def getTeachersForSubject(unitCode:String):List[String] = Stopwatch.time("LDAP.getTeachersForSubject", () => simpleQuery("monashteachingcommitment",unitCode,"uid"))
	override def getStudentsForSubject(unitCode:String):List[String] = Stopwatch.time("LDAP.getStudentsForSubject", () => simpleQuery("monashenrolledsubject",unitCode,"uid"))
	
	override def getEligibleGroups(id:String):Option[Seq[Tuple2[String,String]]] = Stopwatch.time("LDAP.getEligibleGroups", () => {
		var output:Option[Seq[Tuple2[String,String]]] = None
		val func = (n:List[SearchResult]) => {
			output = n.headOption.map(net => {
				val ne = net.asInstanceOf[SearchResult]
				val attributes = ne.getAttributes()	
				val metacn = Option(attributes.get("monashmetacn")) match {
                  case Some(cn) => List(("monashmetacn", cn.toString))
                  case None => List.empty
                }
				metacn ::: List(("ou","Unrestricted"),("uid",id)) ::: groups.map(group => (group, attributes.get(group))).filter{case (name,attr) => attr != null}.map{ case (name:String,attrib:Attribute) => namingEnumerationToSeq(name,attrib.getAll)}.flatten.toList
			})
		}
		env.ldap.withLDAP("(uid=%s)".format(id),func)
		output
	})	

	override def getInformationGroups(id:String):Option[Seq[Tuple2[String,String]]] = Stopwatch.time("LDAP.getInformationGroups", () => {
		var output:Option[Seq[Tuple2[String,String]]] = None
        val otherRestrictions = Seq(("ou","Unrestricted"),("uid",id))
		val func = (n:List[SearchResult]) => {
			output = n.headOption.map(net => {
            val ne = net.asInstanceOf[SearchResult]
        infoGroups.map(group => (group,ne.getAttributes().get(group))).filter{case (name,attr) => attr != null}.map{case (name:String,attrib:Attribute) => namingEnumerationToSeq(name,attrib.getAll)}.flatten
			})
		}
		env.ldap.withLDAP("(uid=%s)".format(id),func)
		output
	})

    private def namingEnumerationToSeq(name:String,namingEnumeration:NamingEnumeration[_]):Seq[(String,String)]= Stopwatch.time("LDAP.namingEnumerationToSeq", () => {
        var mySeq = List.empty[(String,String)] 
        while (namingEnumeration.hasMoreElements()) {
            Option(namingEnumeration.nextElement).foreach( v => mySeq = (name, v.toString) :: mySeq )
        }
        mySeq
    })

    override def ou(names: Seq[String]): Map[String, Seq[(String,String)]] = Stopwatch.time("LDAP.ou", () => {
      Map(names.map{n=> (n,getEligibleGroups(n).getOrElse(List.empty[(String,String)]).asInstanceOf[Seq[(String,String)]])} : _*)
    })
 
    override def info(names: Seq[String]): Map[String, Seq[(String,String)]] = Stopwatch.time("LDAP.info", () => {
      Map(names.map{n=> (n,getInformationGroups(n).getOrElse(List.empty[(String,String)]).asInstanceOf[Seq[(String,String)]])} : _*)
    })
  
    override def humanNames(names: Seq[String]): Map[String, String] = Stopwatch.time("LDAP.humanNames", () => {
      Map(names.map{ n => (n, getInformationGroups(n).getOrElse(List.empty[(String,String)]).filter{ case (name, attr) => name.startsWith("cn") }.first._2)} : _*) 
    })
}
