package com.metl.model

import javax.naming._
import javax.naming.directory._

object LDAP {
  val groups = Seq("ou","monashenrolledsubject","monashteachingcommitment","o","c")
  val infoGroups = Seq("sn","givenname","initials","mail","cn","jpegphoto","gender","personaltitle","employeenumber")

	def simpleQuery(attrName:String,attrValue:String,returnType:String):List[String] = Stopwatch.time("LDAP.simpleQuery", () => {
		var output = List.empty[String]
		val func = (n:List[SearchResult]) => {
			output = n.map(ne => {
				val internalSeq = try {
					namingEnumerationToSeq(returnType,ne.getAttributes().get(returnType).getAll).map(a => a._2)
				} catch {
					case e:Throwable => List.empty[String]
				}
				internalSeq.toList
			}).flatten
		}
		withLDAP("(%s=%s)".format(attrName,attrValue),func)
		output
	})

	def getRawResults(attrName:String,attrValue:String):List[SearchResult] = Stopwatch.time("LDAP.getRawResults",() => {
		var output = List.empty[SearchResult]
		withLDAP("(%s=%s)".format(attrName,attrValue),(l:List[SearchResult])=>output = l)
		output
	})

	def authenticate(username:String,password:String):Boolean = Stopwatch.time("LDAP.authenticate", () => {
		try {
			var cn = ""
			val cnFetchFunc = (n:List[SearchResult]) => cn = n.head.getName
			var output = false
			val func = (n:List[SearchResult]) => output = n.head.getName == cn
			withLDAP("(%s=%s)".format("uid",username),cnFetchFunc)
			withLDAP("(%s=%s)".format("uid",username),func,cn,password,false)
			output
		} catch {
			case e:Throwable => {
				false
			}
		}
	})

	def getAuthcateFromId(id:String):List[String] = Stopwatch.time("LDAP.getAuthcateFromId", () => simpleQuery("employeenumber",id,"uid"))
	def getIdFromAuthcate(id:String):List[String] = Stopwatch.time("LDAP.getIdFromAuthcate", () => simpleQuery("uid",id,"employeenumber"))
	def getTeachersForSubject(unitCode:String):List[String] = Stopwatch.time("LDAP.getTeachersForSubject", () => simpleQuery("monashteachingcommitment",unitCode,"uid"))
	def getStudentsForSubject(unitCode:String):List[String] = Stopwatch.time("LDAP.getStudentsForSubject", () => simpleQuery("monashenrolledsubject",unitCode,"uid"))
	
	def getEligibleGroups(id:String):Option[Seq[Tuple2[String,String]]] = Stopwatch.time("LDAP.getEligibleGroups", () => {
		var output:Option[Seq[Tuple2[String,String]]] = None
		val func = (n:List[SearchResult]) => {
			output = n.headOption.map(net => {
				val ne = net.asInstanceOf[SearchResult]
				val attributes = ne.getAttributes()	
				val name = attributes.get("monashmetacn")
				List(("monashmetacn", name.toString),("ou","Unrestricted"),("uid",id)) ::: groups.map(group => (group, attributes.get(group))).filter{case (name,attr) => attr != null}.map{ case (name:String,attrib:Attribute) => namingEnumerationToSeq(name,attrib.getAll)}.flatten.toList
			})
		}
		withLDAP("(uid=%s)".format(id),func)
		output
	})	

	def getInformationGroups(id:String):Option[Seq[Tuple2[String,String]]] = Stopwatch.time("LDAP.getInformationGroups", () => {
		var output:Option[Seq[Tuple2[String,String]]] = None
    val otherRestrictions = Seq(("ou","Unrestricted"),("uid",id))
		val func = (n:List[SearchResult]) => {
			output = n.headOption.map(net => {
				val ne = net.asInstanceOf[SearchResult]
        infoGroups.map(group => (group,ne.getAttributes().get(group))).filter{case (name,attr) => attr != null}.map{case (name:String,attrib:Attribute) => namingEnumerationToSeq(name,attrib.getAll)}.flatten
			})
		}
		withLDAP("(uid=%s)".format(id),func)
		output
	})

	def withLDAP(searchTerm:String,action:(List[SearchResult])=>Unit, bindUser:String = "uid=mdsmetl",bindPassword:String = "xaepejam",printErrors:Boolean = true):Unit = Stopwatch.time("LDAP.withLDAP", () => {
		var env = new java.util.Hashtable[String,String]();
		env.put(Context.INITIAL_CONTEXT_FACTORY,"com.sun.jndi.ldap.LdapCtxFactory")
		env.put(Context.PROVIDER_URL,"ldap://hybrid.monash.edu.au")
		env.put(Context.SECURITY_AUTHENTICATION,"simple")
		env.put(Context.SECURITY_PRINCIPAL,bindUser+", o=Monash University, c=AU")
		env.put(Context.SECURITY_CREDENTIALS,bindPassword)
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
				if (printErrors) {
					println("ldap search threw: "+e.getMessage)
					e.printStackTrace
				}
			}
		}
	})
  private def namingEnumerationToSeq(name:String,namingEnumeration:NamingEnumeration[_]):Seq[(String,String)]= Stopwatch.time("LDAP.namingEnumerationToSeq", () => {
      var mySeq = List.empty[(String,String)] 
      while(namingEnumeration.hasMore())
			mySeq = (name,namingEnumeration.next.toString) :: mySeq
			mySeq
  })

  def ou(names: Seq[String]): Map[String, Seq[(String,String)]] = Stopwatch.time("LDAP.ou", () => {
    Map(names.map{n=> (n,getEligibleGroups(n).getOrElse(List.empty[(String,String)]).asInstanceOf[Seq[(String,String)]])} : _*)
  })
 
  def info(names: Seq[String]): Map[String, Seq[(String,String)]] = Stopwatch.time("LDAP.info", () => {
    Map(names.map{n=> (n,getInformationGroups(n).getOrElse(List.empty[(String,String)]).asInstanceOf[Seq[(String,String)]])} : _*)
  })
  
  def humanNames(names: Seq[String]): Map[String, String] = Stopwatch.time("LDAP.humanNames", () => {
    Map(names.map{n=> (n, getInformationGroups(n).getOrElse(List.empty[(String,String)]).filter{case (name,attr) => name.startsWith("cn")}.first)._2} : _*)
  })
}

