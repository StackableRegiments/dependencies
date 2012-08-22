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
import net.liftweb.http.SHtml._
import net.liftweb.http.js.JsCmds._
import net.liftweb.mongodb.MongoDB
import net.liftweb.json.Serialization

import java.io.{InputStream,OutputStream}

class LocalMongoInterface(host:String,port:Int,db:String) {

	def createObjectId = Stopwatch.time("LocalMongoInterface.createObjectId", () => new ObjectId)

	private def getDb = Stopwatch.time("LocalMongoInterface.getDb", () => new Mongo(host,port).getDB(db))
	private def getColl(coll:String) = Stopwatch.time("LocalMongoInterface.getColl", () => getDb.getCollection(coll))

	def establishMongoConnection = Stopwatch.time("LocalMongoInterface.establishMongoConnection", () => {
		val mo = new MongoOptions
		mo.socketTimeout = 10000
		mo.socketKeepAlive = true
		MongoDB.defineDb(DefaultMongoIdentifier, new Mongo(new ServerAddress(host,port), mo), db)
	})
	def withColl[A](collection:String,func:(DBCollection) => A):Box[A] = Stopwatch.time("LocalMongoInterface.withColl", () => {
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
	def saveFile(filename:String,input:InputStream):String = Stopwatch.time("LocalMongoInterface.saveFile/inputStream", () => {
		val fs = new GridFS(getDb)
		val inputFile = fs.createFile(input)
		inputFile.setContentType("octet/stream")
		inputFile.setFilename(filename)
		inputFile.save
		inputFile.getId.toString
	})
	def saveFile(filename:String,bytes:Array[Byte]):String = Stopwatch.time("LocalMongoInterface.saveFile/byte[]", () => {
		val fs = new GridFS(getDb)
		val inputFile = fs.createFile(bytes)
		inputFile.setContentType("octet/stream")
		inputFile.setFilename(filename)
		inputFile.save
		inputFile.getId.toString
	})
	def retrieveFile(query:DBObject,output:OutputStream):Unit = Stopwatch.time("LocalMongoInterface.retrieveFile/outputStream", () => {
		val fs = new GridFS(getDb)
		val file = fs.findOne(query)
		file.writeTo(output)
	})
	def retrieveFile(objectId:String):Array[Byte] = Stopwatch.time("LocalMongoInterface.retrieveFile/byte[]", () => {
		val fs = new GridFS(getDb)
		val file = fs.findOne(new ObjectId(objectId))
		IOUtils.toByteArray(file.getInputStream)
	})
		private def fetch(zone:String,room:String):List[DBObject] = Stopwatch.time("LocalMongoInterface.fetchHistory(%s,%s)".format(zone,room),()=>{
		withColl("history",(coll) => {
			val query = new BasicDBObject("zone",zone)
			query.put("room",room)
			coll.find(query).toArray.toArray.toList.asInstanceOf[List[DBObject]]
		}).openOr(List.empty[DBObject])	
	})
	def fetchAsJson(zone:String,room:String):String = Stopwatch.time("LocalMongoInterface.fetchHistoryAsJson(%s,%s)".format(zone,room),() => {
		/*
		fetch(zone,room).map(r => Serializer.toJson(r)) match {
			case Nil => "[]"
			case nl:List[String] => "["+nl.mkString(",")+"]"
			case other => "unknown %s".format(other)
		}
*/
		""
	})
	def fetchAsXml(zone:String,room:String):Node = Stopwatch.time("LocalMongoInterface.fetchHistoryAsXml(%s,%s)".format(zone,room),() => {
		/*
		fetch(zone,room).map(r => Serializer.toXml(r)) match {
			case Nil => Text("no elements found")
			case nl:List[Node] => Elem(null,"resultItems",null,TopScope,Group(nl))
			case other => Text(other.toString)
		}
		*/
		null.asInstanceOf[Node]
	})

	def simpleQuery(collection:String,key:String,value:String) = {
		withColl(collection,(coll) => coll.findOne(new BasicDBObject(key,value)))
	}
	def insertDBObject(coll:String,obj:DBObject) = {
		withColl(coll,(collection) => {
			obj.put("timestamp",new java.util.Date())
			collection.insert(obj)
		})
	}
	def saveXml(coll:String,node:Node) = {
		insertDBObject(coll,toDBObject(node))
	}
	def saveXml(coll:String,nodeSeq:NodeSeq) = {
		toDBArray(nodeSeq).foreach(o => insertDBObject(coll,o.asInstanceOf[DBObject]))
	}
	def toDBArray(nodeseq:NodeSeq):Array[Object] = {
		toDBArray(nodeseq.theSeq)
	}
	def toDBArray(nodes:Seq[Node]):Array[Object] = {
		nodes.toList.map(n => toDBObject(n)).toArray
	}
	def toDBObject(node:Node):DBObject = {
		parseXml(node).asInstanceOf[DBObject]
	}
	def parseXml(node:Node):Object = {
		node match {
			case e:Elem => {
				val output = new BasicDBObject
				val internalObj = new BasicDBObject
				possiblyAttachAttributes(internalObj,e.attributes)
				possiblyAttachChild("children",internalObj,e.child:_*)
				output.put(e.label, internalObj)
				output
			}
 			case scala.xml.Group(nodes) => toDBArray(nodes)
			case scala.xml.PCData(data) => data
			case scala.xml.Comment(text) => text
			case scala.xml.Text(string) => string
			case _ => null
		}
	}
	private def possiblyAttachChild(elementName:String,obj:BasicDBObject,children:Node*):Unit = {
		children.map(parseXml _).filterNot(i => i == null) match {
			case List() => {}
			case l:List[Object] if (l.length == 1) => obj.put(elementName,l(0))
			case other => obj.put(elementName,other)
		}
	}
	private def possiblyAttachAttributes(obj:BasicDBObject,attrib:MetaData):Unit = {
		attrib match {
			case p:PrefixedAttribute => {
				obj.put(p.prefixedKey,toDBArray(p.value))
				possiblyAttachAttributes(obj,p.next)
			}
			case u:UnprefixedAttribute => {
				obj.put(u.key,toDBArray(u.value))
				possiblyAttachAttributes(obj,u.next)
			}
			case _ => {}
		}
	}
}
