package com.metl.model

import org.bson.types.ObjectId
import net.liftweb.mongodb._
import net.liftweb.common._
import net.liftweb.record.field._
import net.liftweb.mongodb.record._
import net.liftweb.mongodb.record.field._
import java.util.Date
import net.liftweb.util._
import scala.collection.JavaConversions._
import net.liftweb.json.JsonDSL._
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonParser._
import net.liftweb.http.js.JE._
import java.net.URL
import com.mongodb._
import com.mongodb.gridfs._
import net.liftweb.util.Helpers
import net.liftweb.util.Helpers._
import org.apache.commons.io.IOUtils
import collection.JavaConversions._
import xml._
import com.metl.model.Globals._
import net.liftweb.http.SHtml._
import net.liftweb.http.js.JsCmds._
import net.liftweb.mongodb.MongoDB
import net.liftweb.json.Serialization

import java.io.{InputStream,OutputStream}

object DBFormats {

	val host = "127.0.0.1"
	val port = 27017
	val db = "sfxmetl"

	def createObjectId = Stopwatch.time("DBFormats.createObjectId", () => new ObjectId)

	private def getDb = Stopwatch.time("DBFormats.getDb", () => new Mongo(host,port).getDB(db))
	private def getColl(coll:String) = Stopwatch.time("DBFormats.getColl", () => getDb.getCollection(coll))

	def establishMongoConnection = Stopwatch.time("DBFormats.establishMongoConnection", () => {
		val mo = new MongoOptions
		mo.socketTimeout = 10000
		mo.socketKeepAlive = true
		MongoDB.defineDb(DefaultMongoIdentifier, new Mongo(new ServerAddress(host,port), mo), db)
	})
	def withColl[A](collection:String,func:(DBCollection) => A):Box[A] = Stopwatch.time("DBFormats.withColl", () => {
		val coll = getColl(collection)
		if (coll != null){
			val output = func(coll)
			if (output != null)	{
				Full(output)
			} else {
				Empty
			}
		} else {
			Empty
		}	
	})
	def saveFile(filename:String,input:InputStream):String = Stopwatch.time("DBFormats.saveFile/inputStream", () => {
		val fs = new GridFS(getDb)
		val inputFile = fs.createFile(input)
		inputFile.setContentType("octet/stream")
		inputFile.setFilename(filename)
		inputFile.save
		inputFile.getId.toString
	})
	def saveFile(filename:String,bytes:Array[Byte]):String = Stopwatch.time("DBFormats.saveFile/byte[]", () => {
		val fs = new GridFS(getDb)
		val inputFile = fs.createFile(bytes)
		inputFile.setContentType("octet/stream")
		inputFile.setFilename(filename)
		inputFile.save
		inputFile.getId.toString
	})
	def retrieveFile(query:DBObject,output:OutputStream):Unit = Stopwatch.time("DBFormats.retrieveFile/outputStream", () => {
		val fs = new GridFS(getDb)
		val file = fs.findOne(query)
		file.writeTo(output)
	})
	def retrieveFile(objectId:String):Array[Byte] = Stopwatch.time("DBFormats.retrieveFile/byte[]", () => {
		val fs = new GridFS(getDb)
		val file = fs.findOne(new ObjectId(objectId))
		IOUtils.toByteArray(file.getInputStream)
	})
}

object User extends User with MongoMetaRecord[User] {
	// 1 hour of sessionValidity
	val sessionValidity = 3600000L

	def fromUsername(username:String) = Stopwatch.time("User.fromUsername", () => {
		find("authcate",username) match {
			case Full(u) => u
			case other => createRecord.authcate(username)
		}
	})
}

class User extends MongoRecord[User] with MongoId[User] {
	def meta = User
	object authcate extends StringField(this,32)
	object sessionKey extends StringField(this,24)
	object sessionLastUpdated extends LongField(this)

	def checkSession(password:String):Boolean = Stopwatch.time("User.checkSession", () => {
		if ((new Date().getTime - sessionLastUpdated.is) < User.sessionValidity){
			password == sessionKey.is
		} else false
	})
	def updateSession(password:String):User = Stopwatch.time("User.updateSession", () => {
		this.sessionKey(password)
		this.sessionLastUpdated(new Date().getTime)
		this.save
		this
	})
}
