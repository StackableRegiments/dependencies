package com.metl.cas

import com.metl.ldap._
import com.metl.utils._

import org.scalatest._
import org.scalatest.fixture
import org.scalatest.fixture.ConfigMapFixture
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.mock.MockitoSugar
import org.scalatest.OptionValues._
import org.scalatest.prop.TableDrivenPropertyChecks._

import org.mockito.Mockito._
import org.mockito.Matchers.{eq => the, any, anyInt}

import com.metl.utils._ 

import javax.naming.directory.{BasicAttributes, BasicAttribute, SearchResult}

/*trait IMeTLLDAP {
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
*/
// some test data
/*
ou = ou: Office of the Deputy Vice-Chancellor (Education), Administration, Staff
monashenrolledsubject = null
monashteachingcommitment = null
o = null
c = null
ou = Unrestricted
uid = eecrole
employeenumber = null
sn = Role
givenname = EEC
initials = E
mail = EEC.Role@monash.edu
cn = EEC Role
jpegphoto = null
gender = null
personaltitle = ROLE
*/
class LDAPMockService extends LDAPSearch {

    def withLDAP(searchTerm:String,action:(List[SearchResult])=>Unit):Unit = {
        val attributes = new BasicAttributes 
        attributes.put(new BasicAttribute("monashenrolledsubject", null))
        attributes.put(new BasicAttribute("monashteachingcommitment", null))
        attributes.put(new BasicAttribute("o", null))
        attributes.put(new BasicAttribute("c", null))
        attributes.put(new BasicAttribute("ou", "Unrestricted"))
        attributes.put(new BasicAttribute("uid", "eecrole"))
        attributes.put(new BasicAttribute("employeenumber", null))
        attributes.put(new BasicAttribute("sn", "Role"))
        attributes.put(new BasicAttribute("givenname", "EEC"))
        attributes.put(new BasicAttribute("initials", "E"))
        attributes.put(new BasicAttribute("mail", "EEC.Role@monash.edu"))
        attributes.put(new BasicAttribute("cn", "EEC Role"))
        attributes.put(new BasicAttribute("jpegphoto", null))
        attributes.put(new BasicAttribute("gender", null))
        attributes.put(new BasicAttribute("personaltitle", "ROLE"))

        val searchResult = new SearchResult("o=Monash University, c=AU", null, attributes)
      
        action(List(searchResult))
    }
}

object LDAPTestConfig {
  lazy val ldapSearch = new LDAPMockService
  lazy val ldap = new LDAPService(this)
}

class LDAPSuite extends fixture.FunSuite with ConfigMapFixture with MockitoSugar with ShouldMatchers {
    
    def withLDAP(test: LDAP => Any) {
       val ldap = new LDAP(LDAPTestConfig) 
       test(ldap)
    }
    
    test("perform a simple query with requested return attribute") { () =>
        withLDAP { ldap =>
            val result = ldap.simpleQuery("sn", "Role", "uid") 
            result should contain ("eecrole")
        }
    }
}
