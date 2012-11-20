package com.metl.orientDB

import com.metl.data._
import com.metl.utils._
import com.metl.persisted._

import com.orientechnologies.orient._
import com.orientechnologies.orient.core.db.document._
import com.orientechnologies.orient.core.db._
//import com.orientechnologies.orient.core.db.`object`._
import com.orientechnologies.orient.core.sql.query._

class OrientDBInterface(configName:String,serializer:Serializer,dbUri:String = "local:/temp/db",dbUser:String = "root",dbPass:String = "ThisIsA_TEST") extends PersistenceInterface{
	var db: ODatabaseDocumentTx = null
	//db.getEntityManager.registerEntityClasses("com.metl.orientDB.formats")

	def withDb[T](transactionAction:(ODatabaseDocumentTx)=>T):Option[T] = withDb[Unit,T](d => {},(d,e) => transactionAction(d))
	def withDb[A,T](preTransactionAction:(ODatabaseDocumentTx) => A, transactionAction:(ODatabaseDocumentTx,A)=> T):Option[T] = {
		if (db == null){
			db = new ODatabaseDocumentTx(dbUri)
		}
		var res:Option[T] = None
		if (db.isClosed){
			if (!db.exists){
				db.create()
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

	def query(sql:String) = new OSQLAsynchQuery(sql)

	//stanzas table
	def storeStanza(jid:String,stanza:MeTLStanza):Boolean = Stopwatch.time("H2Interface.storeStanza", () => {
		false
		
/*
		val serialized = serializer.fromStanza(stanza)
		val s = db.newInstance(serialized.typeDescriptor)
		db.jid(jid)
		db.attach(serialized)
		val saved = db.save(serialized)
		db.detach(serialized)
*/
	})

	def getHistory(jid:String):History = Stopwatch.time("H2Interface.getHistory",() => {
/*
		val newHistory = History(jid)
		List(new OInk,new OText,new OImage,new ODirtyInk,new ODirtyText,new ODirtyImage,new OMoveDelta,new OSubmission,new OCommand,new OQuiz,new OQuizResponse).foreach(t => {
			type T = t
			db.queryBySql[T]("select from %s where jid=%s".format(t.classDescriptor,jid)).foreach(s => newHistory.addStanza(s))	
			//db.queryBySql[t.class]("select from %s where jid=%s".format(t.classDescriptor,jid)).foreach(s => newHistory.addStanza(s))	
		})
		newHistory
*/
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
		//retrieve(RESOURCES, "SELECT * FROM %s WHERE IDENTITY = %s LIMIT 1".format(RESOURCES,identity))
		Array.empty[Byte]
	})
	def postResource(jid:String,userProposedId:String,data:Array[Byte]):String = Stopwatch.time("H2Interface.postResource", () => {
		//store(RESOURCES,null)
		""
	})
/*
	protected def store[T](table:String,dbStoreable:ODocument):Boolean = Stopwatch.time("H2Interface.store", () => {
		db.newInstance(dbStoreable.typeDescriptor)
		db.attach(dbStoreable)
		val saved = db.save(dbStoreable)
		db.detach(dbStoreable)

		db.create(table)
		dbStoreable.save()
		false
	})
	protected def retrieve[T](table:String,query:String):List[ODocument] = Stopwatch.time("H2Interface.retrieve", () => {
		db.queryBySql[table]
		List.empty[ODocument]
	})
*/
}
