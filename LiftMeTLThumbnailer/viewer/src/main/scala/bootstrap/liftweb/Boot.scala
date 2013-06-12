package bootstrap.liftweb

import com.metl.data._

import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.provider._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import Helpers._
import _root_.net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, StandardDBVendor}
import _root_.java.sql.{Connection, DriverManager}
import _root_.com.metl.model._


/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  */
class Boot {
  def boot {
    if (!DB.jndiJdbcConnAvailable_?) {
      val vendor =
        new StandardDBVendor(Props.get("db.driver") openOr "org.h2.Driver",
          Props.get("db.url") openOr
            "jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE",
          Props.get("db.user"), Props.get("db.password"))

      LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

      DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
    }

    // where to search snippet
    LiftRules.addToPackages("com.metl")
    Schemifier.schemify(true, Schemifier.infoF _, User)

    WebMeTLServerConfiguration.initializeWebMeTLSystem

    // attempting to setup authentication using CAS, when running in prod mode or staging mode
    if (!Globals.isDevMode) {
      LiftRules.dispatch.prepend {
        case Req("probe" :: _,_,_) => () => Full(PlainTextResponse("OK",List.empty[Tuple2[String,String]], 200))
        case Req("serverStatus" :: _,_,_) => () => Full(PlainTextResponse("OK",List.empty[Tuple2[String,String]], 200))
      }
    }

    // preload all the server configurations to spin up their XMPP actors
    ServerConfiguration.default

    // Build SiteMap
    def sitemap() = SiteMap(
      Menu("Home") / "index",
      // Menu with special Link
      Menu(Loc("Static", Link(List("static"), true, "/static/index"),
        "Static Content")))

    LiftRules.setSiteMapFunc(() => User.sitemapMutator(sitemap()))

    LiftRules.statelessDispatchTable.append {
      case Req("application" :: "snapshot" :: Nil,_,_) => () => {
        val server = S.param("server").openOr("")
        val slide = S.param("slide").openOr("")
        //                      val width = S.param("width").openOr("")
        //                      val height = S.param("height").openOr("")
        Full(HttpResponder.snapshot(server,slide,"small"))
      }
      case Req(server :: "slide" :: jid :: size :: Nil,_,_) => () => Full(HttpResponder.snapshot(server,jid,size))
    }

    /*
     * Show the spinny image when an Ajax call starts
     */
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    /*
     * Make the spinny image go away when it ends
     */
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    LiftRules.early.append(makeUtf8)

    LiftRules.loggedInTest = Full(() => User.loggedIn_?)

    S.addAround(DB.buildLoanWrapper)
  }

  /**
    * Force the request to be UTF-8
    */
  private def makeUtf8(req: HTTPRequest) {
    req.setCharacterEncoding("UTF-8")
  }
}
