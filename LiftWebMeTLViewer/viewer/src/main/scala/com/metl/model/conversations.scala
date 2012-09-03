package com.metl.model

import com.metl.data._
import com.metl.utils._

import net.liftweb._
import http._
import common._
import util._
import Helpers._
import java.util.Date
import scala.xml._
import scala.collection.immutable.IntMap
import java.text.SimpleDateFormat
import net.liftweb.actor.LiftActor
import scala.collection.mutable.{HashMap, SynchronizedMap}
import net.liftweb.util.ActorPing

case object RefreshConversation

class CachedConversation(id:CachedConversationIdentifier) extends LiftActor {
	val jid = id.jid
	val server = id.server
	println("CachedConversation using server: %s".format(server))

	private val acceptableStaleTime = 10000
	private val refreshTime = 30 seconds

	private var conversation = fetchConversation

	private var lastRequested:Long = 0L
	ActorPing.schedule(this,RefreshConversation,refreshTime)
	
	private def fetchConversation = Stopwatch.time("CachedConversation_%s@%s fetchingConversation".format(jid,server.host),()=> tryo(server.detailsOfConversation(jid.toString)).openOr(Conversation.empty))

	override def messageHandler = {
		case RefreshConversation => {
			if ((new Date().getTime - lastRequested) > acceptableStaleTime){
				ConversationCache.remove(jid,server)
			}
			else {
				conversation = fetchConversation
				ActorPing.schedule(this,RefreshConversation,refreshTime)
			}
		}
		case _ => {}
	}
	def getConversation:Conversation = {
		lastRequested = new Date().getTime
		conversation
	}
}

case class CachedConversationIdentifier(jid:Int,server:ServerConfiguration)

object ConversationCache {
	private val conversations = new HashMap[CachedConversationIdentifier,CachedConversation] with SynchronizedMap[CachedConversationIdentifier,CachedConversation]{
		override def default(where:CachedConversationIdentifier) = {
			val newCachedConversation = new CachedConversation(where)
			this += (where -> newCachedConversation)
			newCachedConversation
		}
	}
	def remove(jid:Int,server:ServerConfiguration):Unit = {
		Stopwatch.time("ConversationCache expiring: %s@%s".format(jid,server.name),()=> conversations.remove(CachedConversationIdentifier(jid,server)))
	}
	def getConversation(jid:Int,server:ServerConfiguration) = {
		println("ConversationCache using server: %s".format(server))
		Stopwatch.time("ConversationCache fetching conversation: %s@%s".format(jid,server.name),()=> conversations(CachedConversationIdentifier(jid,server)).getConversation)
	}
}
