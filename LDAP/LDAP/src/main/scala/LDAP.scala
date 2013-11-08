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
  def getValuePairs(attrName:String,attrValue:String,returnTypes:List[String]):List[Tuple2[String,Seq[Tuple2[String,String]]]]
}

object DisconnectedLDAP extends IMeTLLDAP {
  override def authenticate(username:String,password:String) = None
 	override def simpleQuery(attrName:String,attrValue:String,returnType:String):List[String] = List.empty[String]
  override def getValuePairs(attrName:String,attrValue:String,returnTypes:List[String]):List[Tuple2[String,Seq[Tuple2[String,String]]]] = List.empty[Tuple2[String,Seq[Tuple2[String,String]]]]
}

trait LDAPSearch {
  val bindBase:String
  def withLDAP(searchTerm:String,action:(List[SearchResult])=>Unit):Unit
  def withLDAPUsingCredentials(user:String,password:String,searchTerm:String,action:(List[SearchResult])=>Unit,base:String,usernameIncludesBase:Boolean):Unit
}

class LDAPSearchService(directory:String,incomingBindBase:String,bindUser:String,password:String) extends LDAPSearch {
  val bindBase = incomingBindBase 
  def withLDAPUsingCredentials(user:String,pass:String,searchTerm:String, action:(List[SearchResult])=> Unit,base:String = bindBase,usernameIncludesBase:Boolean = true):Unit = Stopwatch.time("LDAP.withLDAPUsingCredentials", () => {
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
    withLDAPUsingCredentials(bindUser,password,searchTerm,action,bindBase,false)
	})
}

class LDAPService(env: { val ldapSearch: LDAPSearch }) {
  val searchService = env.ldapSearch
  def withLDAP(searchTerm: String, action: (List[SearchResult]) => Unit): Unit = {
    env.ldapSearch.withLDAP(searchTerm, action)
  }
  def withLDAPUsingCredentials(username:String,password:String, searchTerm:String,action: (List[SearchResult]) => Unit,base:String,usernameIncludesBase:Boolean): Unit = {
    env.ldapSearch.withLDAPUsingCredentials(username,password,searchTerm,action,base,usernameIncludesBase) 
  }
}

class LDAPConfig(directory:String,bindUser:String,password:String,bindBase:String) {
  lazy val ldapSearch = new LDAPSearchService(directory,bindBase,bindUser,password)
  lazy val ldap = new LDAPService(this)
}

class LDAP(env : { val ldap: LDAPService }) extends IMeTLLDAP {
  protected val usernameCheckType = "uid"
  override def authenticate(username:String,password:String):Option[Boolean] = Stopwatch.time("LDAP.authenticate", () => {
    var recordOutput:Option[String] = None
    var output:Option[Boolean] = None
    val func = (n:List[SearchResult]) => {
			recordOutput = n.take(1).map(ne => ne.getName).headOption
    }
    env.ldap.withLDAP("(%s=%s)".format(usernameCheckType,username),func)
    val func2 = (n:List[SearchResult]) => {
      output = Some(n.length > 0)
    }
    recordOutput.map(dn => {
      env.ldap.withLDAPUsingCredentials(dn,password,"(%s=%s)".format(usernameCheckType,username),func2,env.ldap.searchService.bindBase,false)
    })
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
  override def getValuePairs(attrName:String,attrValue:String,returnTypes:List[String]):List[Tuple2[String,Seq[Tuple2[String,String]]]] = Stopwatch.time("LDAP.getValuePairs", () => {
  	var output:List[Tuple2[String,Seq[Tuple2[String,String]]]] = List.empty[Tuple2[String,Seq[Tuple2[String,String]]]]
		val func = (n:List[SearchResult]) => {
      output = n.map(net => {
        val ne = net.asInstanceOf[SearchResult]
        Tuple2(ne.getName,returnTypes.map(group => (group,ne.getAttributes().get(group))).filter{case (name,attr) => attr != null}.map{case (name:String,attrib:Attribute) => namingEnumerationToSeq(name,attrib.getAll)}.flatten)
			})
		}
		env.ldap.withLDAP("(%s=%s)".format(attrName,attrValue),func)
		output
  })	
  protected def namingEnumerationToSeq(name:String,namingEnumeration:NamingEnumeration[_]):Seq[(String,String)]= Stopwatch.time("LDAP.namingEnumerationToSeq", () => {
    var mySeq = List.empty[(String,String)] 
    while (namingEnumeration.hasMoreElements()) {
        Option(namingEnumeration.nextElement).foreach( v => mySeq = (name, v.toString) :: mySeq )
    }
    mySeq
  })
}
