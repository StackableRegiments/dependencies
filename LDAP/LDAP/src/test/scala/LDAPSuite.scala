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

class LDAPMockService extends LDAPSearch {
    def withLDAPUsingCredentials(username:String,password:String,searchTerm:String,action:(List[SearchResult])=>Unit):Unit = {}

    def withLDAP(searchTerm:String,action:(List[SearchResult])=>Unit):Unit = {
        val attributes = new BasicAttributes 
        attributes.put(new BasicAttribute("monashenrolledsubject", "phs2022"))
        attributes.put(new BasicAttribute("monashenrolledsubject", "phs2011"))
        attributes.put(new BasicAttribute("monashteachingcommitment", "mth2021"))
        attributes.put(new BasicAttribute("o", null))
        attributes.put(new BasicAttribute("c", null))
        attributes.put(new BasicAttribute("uid", "eecrole"))
        attributes.put(new BasicAttribute("employeenumber", "1034236575"))
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

class LDAPMockMultiService extends LDAPSearch {

    def withLDAPUsingCredentials(username:String,password:String,searchTerm:String,action:(List[SearchResult])=>Unit):Unit = {}

    private def buildSearchResult(uid: String, cn: String, sn: String, givenname: String, 
      initials: String, mail: String, personaltitle: String, employeenumber: String,
        teaching:Seq[String], enrolment: Seq[String]) = {

        val attr = new BasicAttributes 
        attr.put(new BasicAttribute("uid", uid))
        attr.put(new BasicAttribute("cn", cn))
        attr.put(new BasicAttribute("sn", sn))
        attr.put(new BasicAttribute("givenname", givenname))
        attr.put(new BasicAttribute("initials", initials))
        attr.put(new BasicAttribute("mail", mail))
        attr.put(new BasicAttribute("personaltitle", personaltitle))
        attr.put(new BasicAttribute("employeenumber", employeenumber))

        teaching.foreach( t => attr.put(new BasicAttribute("monashteachingcommitment", t)))
        enrolment.foreach( e => attr.put(new BasicAttribute("monashenrolledsubject", e)))
        
        new SearchResult("o=Monash University, c=AU", null, attr)
    }

    def withLDAP(searchTerm:String,action:(List[SearchResult])=>Unit):Unit = {

        val result = searchTerm match {
          case "(uid=eecrole)" => List(buildSearchResult("eecrole", "EEC Role", "Role", "EEC", "E", "EEC.Role@monash.edu", "ROLE", "1034236575", Seq("mth2021"), Seq("phs2011")))
          case "(uid=designa)" => List(buildSearchResult("designa", "Design Testing-A", "Testing-A", "Design", "D", null, "ROLE", null, Seq.empty[String], Seq.empty[String]))
          case _ => List.empty[SearchResult] 
        }

        action(result)
    }
}

object LDAPTestMultiConfig {
  lazy val ldapSearch = new LDAPMockMultiService
  lazy val ldap = new LDAPService(this)
}

class LDAPSuite extends fixture.FunSuite with ConfigMapFixture with MockitoSugar with ShouldMatchers {
    
    def withLDAP(test: LDAP => Any) {
       val ldap = new LDAP(LDAPTestConfig) 
       test(ldap)
    }

    def withMultiLDAP(test: LDAP => Any) {
        val ldap = new LDAP(LDAPTestMultiConfig)
        test(ldap)
    }
    
    test("perform a simple query with requested return attribute") { () =>
        withLDAP { ldap =>
            val result = ldap.simpleQuery("sn", "Role", "uid") 

            result should have length (1)
            result should contain ("eecrole")
        }
    }

    test("perform simple query with null attribute") { () =>
        withLDAP { ldap =>
            val result = ldap.simpleQuery("uid", "eecrole", "jpegphoto")

            result should have length (0)
        }
    }

    test("perform simple query and return non-existent attribute") { () =>
        withLDAP { ldap =>
            val result = ldap.simpleQuery("uid", "eecrole", "lsjdflkjsd")

            result should have length (0)
        }
    }

    test("retrieve authcate from the id specified") { () =>
        withLDAP { ldap =>
            val result = ldap.getAuthcateFromId("1034236575") 

            result should have length (1)
            result should contain ("eecrole")
        }
    }

    test("retrieve id from authcate specified") { () =>
        withLDAP { ldap =>
            val result = ldap.getIdFromAuthcate("eecrole")

            result should have length (1)
            result should contain ("1034236575")
        }
    }

    test("retrieve teachers for subject") { () =>
        withLDAP { ldap =>
            val result = ldap.getTeachersForSubject("mth2021")

            result should have length (1)
            result should contain ("eecrole")
        }
    }

    test("retrieve students for subject") { () =>
        withLDAP { ldap =>
            val result = ldap.getStudentsForSubject("phs2022")

            result should have length (1)
            result should contain ("eecrole")
        }
    }

    test("retrieve eligible groups for uid") { () =>
        withLDAP { ldap =>
            val result = ldap.getEligibleGroups("eecrole")

            val expected = List(("ou", "Unrestricted"), ("uid", "eecrole"), ("monashenrolledsubject", "phs2011"), ("monashteachingcommitment", "mth2021"))
            result should be (Some(expected))
        }
    }

    test("retrieve information groups for uid") { () =>
        withLDAP { ldap =>
            val result = ldap.getInformationGroups("eecrole")
            
            val expected = List(("sn", "Role"), ("givenname", "EEC"), ("initials", "E"), ("mail", "EEC.Role@monash.edu"), 
                ("cn", "EEC Role"), ("personaltitle", "ROLE"), ("employeenumber", "1034236575"))
            result should be (Some(expected))
        }
    }

    test("return ou for given names") { () =>
        withLDAP { ldap =>
            val result = ldap.ou(Seq("eecrole"))

            val expected = Map("eecrole" -> Seq(("ou", "Unrestricted"), ("uid", "eecrole"), ("monashenrolledsubject", "phs2011"), ("monashteachingcommitment", "mth2021")))
            result should be (expected)
        }
    }

    test("return ou for multiple given names") { () =>
        withMultiLDAP { ldap =>
            val result = ldap.ou(Seq("eecrole", "designa"))

            val expected = Map("eecrole" -> Seq(("ou", "Unrestricted"), ("uid", "eecrole"), ("monashenrolledsubject", "phs2011"), ("monashteachingcommitment", "mth2021")),
                               "designa" -> Seq(("ou", "Unrestricted"), ("uid", "designa"))) 
            result should be (expected)
        }
    }
    

    test("return empty ou results for empty names") { () =>
        withLDAP { ldap =>
            val result = ldap.ou(Seq.empty[String])

            result should be (Map.empty[String, Seq[(String, String)]])
        }
    }

    test("return info for given names") { () =>
        withLDAP { ldap =>
            val result = ldap.info(Seq("eecrole"))

            val expected = Map("eecrole" -> Seq(("sn", "Role"), ("givenname", "EEC"), ("initials", "E"), ("mail", "EEC.Role@monash.edu"), 
                ("cn", "EEC Role"), ("personaltitle", "ROLE"), ("employeenumber", "1034236575")))
            result should be (expected)
        }
    }

    test("return empty info results for empty names") { () =>
        withLDAP { ldap =>
            val result = ldap.info(Seq.empty[String])

            result should be (Map.empty[String, Seq[(String, String)]])
        }
    }

    test("return info for multiple given names") { () => 
        withMultiLDAP { ldap =>
            val result = ldap.info(Seq("eecrole", "designa"))

            val expected = Map("eecrole" -> Seq(("sn", "Role"), ("givenname", "EEC"), ("initials", "E"), ("mail", "EEC.Role@monash.edu"), ("cn", "EEC Role"), ("personaltitle", "ROLE"), 
                                                ("employeenumber", "1034236575")),
                               "designa" -> Seq(("sn", "Testing-A"), ("givenname", "Design"), ("initials", "D"), ("cn", "Design Testing-A"), ("personaltitle", "ROLE")))
            result should be (expected)
        }
    }

    test("return humanNames for given names") { () =>
        withLDAP { ldap =>
            val result = ldap.humanNames(Seq("eecrole"))

            val expected = Map("eecrole" -> "EEC Role")
            result should be (expected)
        }
    }

    test("return humanNames for multiple given names") { () =>
        withMultiLDAP { ldap =>
            val result = ldap.humanNames(Seq("eecrole", "designa"))

            val expected = Map("eecrole" -> "EEC Role", "designa" -> "Design Testing-A")
            result should be (expected)
        }
    }

    test("return empty humanNames results for empty names") { () =>
        withLDAP { ldap =>
            val result = ldap.humanNames(Seq.empty[String])

            result should be (Map.empty[String, Seq[(String, String)]])
        }
    }

}
