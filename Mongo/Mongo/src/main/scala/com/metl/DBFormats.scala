package psych.model

import org.bson.types.ObjectId
import net.liftweb.mongodb._
import net.liftweb.common._
import net.liftweb.record.field._
import net.liftweb.mongodb.record._
import net.liftweb.mongodb.record.field._
import net.liftweb.json.JsonDSL._
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonParser._
import net.liftweb.http.js.JE._
import com.mongodb.{BasicDBObject,BasicDBList,Mongo,MongoOptions,ServerAddress}
import java.util.Date
import scala.collection._
import scala.collection.JavaConversions._
import scala.io._
import net.liftweb.common._
import net.liftweb.util.Helpers._
import net.liftweb.http.SHtml._

object DBServerOptions {
	val db = "psychPreview"
	//val host = "adm-sql02-v01.adm.monash.edu"
	val host = "127.0.0.1"
	val port = 27017
	val socketTimeout = 10000
	val keepAlive = true
	def initializeDefaultDB = {
		val srvr = new ServerAddress(host,port)
		val mo = new MongoOptions
		mo.socketTimeout = socketTimeout
		mo.socketKeepAlive = keepAlive
		MongoDB.defineDb(DefaultMongoIdentifier, new Mongo(srvr,mo),db)
	}
}

class CustomMongoIdentifier(override val jndiName:String) extends MongoIdentifier

case class CustomMongo(db:String, host:String = DBServerOptions.host,port:Int = DBServerOptions.port,socketTimeout:Int = DBServerOptions.socketTimeout,socketKeepAlive:Boolean = DBServerOptions.keepAlive){
	mo.socketTimeout = socketTimeout
	mo.socketKeepAlive = socketKeepAlive
	MongoDB.defineDb(identifier, mongo, db)
	lazy val server = new ServerAddress(host,port)
	lazy val mo = new MongoOptions
	lazy val mongo = new Mongo(server,mo)
	lazy val identifier = new CustomMongoIdentifier("%s@%s:%s.mongoDbIdentifier".format(db,host,port))
}

object DBUtil {
	private val adminMongo = CustomMongo("admin")
	private val backupMongo = CustomMongo("psychBackup")
	private val prodMongo = CustomMongo("psych")
	private val previewMongo = CustomMongo("psychPreview")
	private val displayTables = List("activitydatas","cachedcalculations","coursedatas","mplsubmissions","pearsonuserdataoverrides","unitadminsuperusers","unitdatas","userdatas","userinformations","weekdatas")
	private val siteViews = "siteviews"
	private def copyDB(source:CustomMongo,target:CustomMongo,shouldDropSiteViews:Boolean = false) = {
		dropDataTables(target)
		if (shouldDropSiteViews) 
			dropTable(target,siteViews)
		val command = new BasicDBObject("copydb",1)
		command.append("fromdb",source.db)
		command.append("todb",target.db)
		MongoDB.useSession(adminMongo.identifier)(db => db.command(command))
	}
	private def dropDB(target:CustomMongo) = {
		MongoDB.useSession(target.identifier)(db => db.command(new BasicDBObject("dropDatabase",1)))
	}
	private def dropDataTables(target:CustomMongo) = {
		displayTables.foreach(dt => dropTable(target,dt))
	}
	private def dropTable(target:CustomMongo,table:String) = {
		MongoDB.useCollection(target.identifier,table)(coll => coll.drop)
	}
	def copyProdToPreview = copyDB(prodMongo,previewMongo)
	def restoreBackup = copyDB(backupMongo,prodMongo)
	def backupDB = copyDB(prodMongo,backupMongo,true)
	def copyPreviewToProd = copyDB(previewMongo,prodMongo)
}
