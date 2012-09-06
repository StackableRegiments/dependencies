package bootstrap.liftweb

import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.rest._
import _root_.net.liftweb.http.provider._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import Helpers._
import _root_.net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, StandardDBVendor}
import _root_.java.sql.{Connection, DriverManager}
import _root_.com.metl.model._
import java.io.FileNotFoundException
import js.jquery.JQuery14Artifacts
import com.metl.snippet._
import com.metl.model._
import com.mongodb._
import net.liftweb.mongodb._
import com.metl.model.Globals._

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
case class ParamInfo(paramName: String)

class Boot {
  def boot {
    if (!DB.jndiJdbcConnAvailable_?) {
      val vendor =
        new StandardDBVendor(Props.get("db.driver") openOr "org.h2.Driver",
          Props.get("db.url") openOr
            "jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE",
          Props.get("db.user"), Props.get("db.password")){
          override def maxPoolSize = 2000
          override def doNotExpandBeyond = 10000
          override def allowTemporaryPoolExpansion = true
        }

      LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

      DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
    }
    LiftRules.addToPackages("com.metl")
    
    LiftRules.allowParallelSnippets.session.set(true)
    LiftRules.maxConcurrentRequests.session.set((r:Req)=>1000)

		// Setup mongo connection
		DBFormats.establishMongoConnection

		// Setup RESTful endpoints
		LiftRules.statelessDispatchTable.prepend(MeTLRestHelper)
		LiftRules.dispatch.append(MeTLStatefulRestHelper)

		// Build SiteMap
		def sitemap() = SiteMap(
      Menu("Home") / "index",
      Menu("Help") / "help" >> Hidden,
      Menu(Loc("Static", Link(List("static"), true, "/static/index"), "Static Content", Hidden))
		)
    LiftRules.setSiteMapFunc(sitemap _)
		// Show the spinny image when an Ajax call starts 
    LiftRules.ajaxStart = Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    // Make the spinny image go away when it ends 
    LiftRules.ajaxEnd = Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    LiftRules.early.append(makeUtf8)
    LiftRules.loggedInTest = Full(() => true)
    LiftRules.jsArtifacts = JQuery14Artifacts
    S.addAround(DB.buildLoanWrapper)

		//changing longPoll timeout to 25 seconds
		LiftRules.cometRequestTimeout = Full(25)
  }
  // Force the request to be UTF-8
  private def makeUtf8(req: HTTPRequest) {
    req.setCharacterEncoding("UTF-8")
  }
}

