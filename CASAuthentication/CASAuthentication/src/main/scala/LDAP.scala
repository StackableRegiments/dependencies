package com.metl.ldap

import com.metl.utils._

import org.apache.commons.io._
import javax.naming._
import javax.naming.directory._
import net.liftweb.common._
import net.liftweb.util.Helpers._

trait IMeTLLDAP {
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

trait LDAPSearch {

    def withLDAP(searchTerm:String,action:(List[SearchResult])=>Unit):Unit
}

class LDAPSearchService extends LDAPSearch {

  def withLDAP(searchTerm:String, action:(List[SearchResult])=> Unit): Unit = Stopwatch.time("LDAP.withLDAP", () => {
		var env = new java.util.Hashtable[String,String]()
		env.put(Context.INITIAL_CONTEXT_FACTORY,"com.sun.jndi.ldap.LdapCtxFactory")
		env.put(Context.PROVIDER_URL,"ldap://hybrid.monash.edu.au")
		env.put(Context.SECURITY_AUTHENTICATION,"simple")
		env.put(Context.SECURITY_PRINCIPAL,"uid=mdsmetl, o=Monash University, c=AU")
		env.put(Context.SECURITY_CREDENTIALS,"xaepejam")
		try
			{
				val ctx = new InitialDirContext(env)
				val controls = new SearchControls
				controls.setSearchScope(SearchControls.SUBTREE_SCOPE)
				val results = ctx.search("o=Monash University, c=AU",searchTerm,controls)
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
				println("ldap search threw: "+e.getMessage)
				e.printStackTrace
			}
		}
	})
}

class LDAPService(env: { val ldapSearch: LDAPSearch }) {
    def withLDAP(searchTerm: String, action: (List[SearchResult]) => Unit): Unit = {
        env.ldapSearch.withLDAP(searchTerm, action)
    }
}

object LDAPProdConfig {
    lazy val ldapSearch = new LDAPSearchService
    lazy val ldap = new LDAPService(this)
}

class LDAP(env : { val ldap: LDAPService }) extends IMeTLLDAP {
    val groups = Seq("ou","monashenrolledsubject","monashteachingcommitment","o","c")
    val infoGroups = Seq("sn","givenname","initials","mail","cn","jpegphoto","gender","personaltitle","employeenumber")

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
        while(namingEnumeration.hasMore())
              mySeq = (name,namingEnumeration.next.toString) :: mySeq
              mySeq
    })

    override def ou(names: Seq[String]): Map[String, Seq[(String,String)]] = Stopwatch.time("LDAP.ou", () => {
      Map(names.map{n=> (n,getEligibleGroups(n).getOrElse(List.empty[(String,String)]).asInstanceOf[Seq[(String,String)]])} : _*)
    })
 
    override def info(names: Seq[String]): Map[String, Seq[(String,String)]] = Stopwatch.time("LDAP.info", () => {
      Map(names.map{n=> (n,getInformationGroups(n).getOrElse(List.empty[(String,String)]).asInstanceOf[Seq[(String,String)]])} : _*)
    })
  
    override def humanNames(names: Seq[String]): Map[String, String] = Stopwatch.time("LDAP.humanNames", () => {
      Map(names.map{n=> (n, getInformationGroups(n).getOrElse(List.empty[(String,String)]).filter{case (name,attr) => name.startsWith("cn")}.first)._2} : _*)
    })
}
