package com.metl.orientDB

import com.metl.data._
import com.metl.utils._
import com.metl.persisted._

import com.orientechnologies.orient._
import com.orientechnologies.orient.core.db.document._
import com.orientechnologies.orient.core.db._
import com.orientechnologies.orient.core.record.impl._
//import com.orientechnologies.orient.core.db.`object`._
import com.orientechnologies.orient.core.sql.query._

import java.util.Date

import com.orientechnologies.orient.core.metadata.schema.OType

class OrientDBSerializationHelper {
	def fetchFieldByName(doc:ODocument,fieldName:String) = {
		val fieldType = doc.fieldType(fieldName) match {
			case o:OType => o
			case _ => OType.STRING
		}
		doc.field(fieldName,fieldType)
	}
	def updateFieldByName(doc:ODocument,fieldName:String,fieldValue:Any):ODocument = {
		val (fieldType,adjustedValue) = fieldValue match {
			case v:Array[Byte] => (OType.BINARY,v)
			case v:Boolean => (OType.BOOLEAN,v)
			case v:Byte => (OType.BYTE,v)
			case v:Date => (OType.DATETIME,v)
			case v:Double => (OType.DOUBLE,v)
			case v:Float => (OType.FLOAT,v)
			case v:Long => (OType.LONG,v)
			case v:String => (OType.STRING,v)
			case v => (OType.STRING,v.toString)
		}
		doc.field(fieldName,adjustedValue,fieldType)
		doc
	}
}

class OrientDBInterface(configName:String,dbUri:String = "local:/temp/db",dbUser:String = "root",dbPass:String = "ThisIsA_TEST") extends PersistenceInterface{
	var db: ODatabaseDocumentTx = null

	def withDb[T](transactionAction:(ODatabaseDocumentTx)=>T):Option[T] = withDb[Unit,T](d => {},(d,e) => transactionAction(d))
	def withDb[A,T](preTransactionAction:(ODatabaseDocumentTx) => A, transactionAction:(ODatabaseDocumentTx,A)=> T):Option[T] = {
		if (db == null){
			db = new ODatabaseDocumentTx(dbUri)
		}
		var res:Option[T] = None
		if (db.isClosed){
			if (!db.exists){
				db.create()
				//db.getEntityManager.registerEntityClasses("com.metl.orientDB.dbformats")
			} else {
				db.open(dbUser,dbPass)
			}
		}
		try {
			ODatabaseRecordThreadLocal.INSTANCE.set(db)
			val pre = preTransactionAction(db)
			db.begin
			res = Some(transactionAction(db,pre))
			db.commit
		} catch {
			case e:Throwable => {
				db.rollback
				println(e.getMessage)
				e.printStackTrace
			}
		}
		db.close
		return res
	}

	def newDoc(docType:String):Option[ODocument] = withDb[ODocument,ODocument](d => new ODocument(docType),(d,e) => e)
	def fetchByType(typeName:String):List[ODocument] = {
		withDb[List[ODocument]](d => {
			val iter = d.browseClass(typeName)
			var res = List.empty[ODocument]
			while (iter.hasNext){
				res = res ::: List(iter.next)
			}
			res
		}).getOrElse(List.empty[ODocument])
	}

	def query(sql:String) = new OSQLAsynchQuery(sql)

	//stanzas table
	def storeStanza(jid:String,stanza:MeTLStanza):Boolean = Stopwatch.time("H2Interface.storeStanza", () => {
		false
	})

	def getHistory(jid:String):History = Stopwatch.time("H2Interface.getHistory",() => {
		History.empty
	})

	//conversations table
	def searchForConversation(query:String):List[Conversation] = List.empty[Conversation]
	def conversationFor(slide:Int):Int = 0
	def detailsOfConversation(jid:Int):Conversation = Conversation.empty
	def createConversation(title:String,author:String):Conversation = Conversation.empty
	def deleteConversation(jid:String):Conversation = Conversation.empty
	def renameConversation(jid:String,newTitle:String):Conversation = Conversation.empty
	def changePermissionsOfConversation(jid:String,newPermissions:Permissions):Conversation = Conversation.empty
	def updateSubjectOfConversation(jid:String,newSubject:String):Conversation = Conversation.empty
	def addSlideAtIndexOfConversation(jid:String,index:Int):Conversation = Conversation.empty
	def reorderSlidesOfConversation(jid:String,newSlides:List[Slide]):Conversation = Conversation.empty

	//resources table will have to be: STRING(jid),STRING(id),BINARY(data)

	//resources table
	def getResource(identity:String):Array[Byte] = Stopwatch.time("H2Interface.getResource", () => {
		Array.empty[Byte]
	})
	def postResource(jid:String,userProposedId:String,data:Array[Byte]):String = Stopwatch.time("H2Interface.postResource", () => {
		""
	})
}
