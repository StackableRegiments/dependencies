package com.metl.model

import scala.xml._
import net.liftweb._
import actor._
import common._
import http._
import util._
import util.TimeHelpers
import Helpers.TimeSpan
import java.util.Date

abstract class RoomProvider {
	def get(jid:String):MeTLRoom
	def addLocalMeTLStanza(s:MeTLStanza):Unit
	def removeMeTLRoom(room:String):Unit
	def exists(room:String):Boolean
}

object EmptyRoomProvider extends RoomProvider {
	override def get(jid:String) = EmptyRoom
	override def addLocalMeTLStanza(s:MeTLStanza) = {}
	override def removeMeTLRoom(room:String) = {}
	override def exists(room:String) = false
}

class HistoryCachingRoomProvider(configName:String) extends RoomProvider {
	private lazy val metlRooms = new SynchronizedWriteMap[String,MeTLRoom](scala.collection.mutable.HashMap.empty[String,MeTLRoom],true,(k:String) => createNewMeTLRoom(k))
	override def exists(room:String):Boolean = Stopwatch.time("Rooms.exists", () => metlRooms.keys.exists(k => k == room))
	override def get(room:String) = Stopwatch.time("Rooms.get", () => metlRooms.getOrElseUpdate(room, createNewMeTLRoom(room)))
	private def createNewMeTLRoom(room:String) = Stopwatch.time("Rooms.createNewMeTLRoom(%s)".format(room), () => new HistoryCachingRoom(configName,room))
	override def removeMeTLRoom(room:String) = Stopwatch.time("Rooms.removeMeTLRoom(%s)".format(room), () => metlRooms.remove(room))
	override def addLocalMeTLStanza(s:MeTLStanza) = Stopwatch.time("Rooms.addLocalMeTLStanza", () => {
		s match {
			case c:MeTLCanvasContent => {
				val host = c.server.name
				val room = c.slide
				if (exists(room))
					get(room) ! ServerToLocalMeTLStanza(s)
			}
			case _ => println("metlStanza from server: %s".format(s))
		}
	})
}

case class ServerToLocalMeTLStanza(stanza:MeTLStanza)
case class LocalToServerMeTLStanza(stanza:MeTLStanza)
abstract class RoomStateInformation
case class RoomJoinAcknowledged(server:String,room:String) extends RoomStateInformation
case class RoomLeaveAcknowledged(server:String,room:String) extends RoomStateInformation
case class JoinRoom(username:String,cometId:String,actor:LiftActor)
case class LeaveRoom(username:String,cometId:String,actor:LiftActor)

case object HealthyWelcomeFromRoom
case object Ping

abstract class MeTLRoom(configName:String,location:String) extends LiftActor with ListenerManager {
	lazy val config = ServerConfiguration.configForName(configName)
	protected val messageBus = config.getMessageBus(location)
	def getHistory:History
	private val pollInterval = new TimeSpan(120000)
	private var joinedUsers = List.empty[Tuple2[String,LiftActor]]
	def createUpdate = HealthyWelcomeFromRoom
	private var lastInterest:Long = new Date().getTime
	private var interestTimeout:Long = 60000
	private def heartbeat = ActorPing.schedule(this,Ping,pollInterval)
	heartbeat
	override def lowPriority = {
		case j:JoinRoom => Stopwatch.time("MeTLRoom.lowPriority.JoinRoom", () => addConnection(j)) 
		case l:LeaveRoom => Stopwatch.time("MeTLRoom.lowPriority.LeaveRoom", () => removeConnection(l))
		case sl@ServerToLocalMeTLStanza(s) => Stopwatch.time("MeTLRoom.lowPriority.ServerToLocalMeTLStanza", () => sendToChildren(s))
		case Ping => Stopwatch.time("MeTLRoom.ping", () => {
			if (possiblyCloseRoom){
				
			} else {
				heartbeat
			}
		})
		case ls@LocalToServerMeTLStanza(s) => Stopwatch.time("MeTLRoom.lowPriority.LocalToServerMeTLStanza", () => sendStanzaToServer(s))
		case _ => println("MeTLRoom recieved unknown message")
	}
	protected def sendToChildren(a:MeTLStanza):Unit = Stopwatch.time("MeTLRoom.sendToChildren",() => joinedUsers.foreach(j => j._2 ! a))
	protected def sendStanzaToServer(s:MeTLStanza):Unit = Stopwatch.time("MeTLRoom.sendStanzaToServer", () => messageBus.sendStanzaToRoom(s))
	private def formatConnection(username:String,uniqueId:String):String = "%s_%s".format(username,uniqueId)
	private def addConnection(j:JoinRoom):Unit = Stopwatch.time("MeTLRoom.addConnection(%s)".format(j),() => {
		joinedUsers = ((formatConnection(j.username,j.cometId),j.actor) :: joinedUsers).distinct
		j.actor ! RoomJoinAcknowledged(configName,location)
	})
	private def removeConnection(l:LeaveRoom):Unit = Stopwatch.time("MeTLRoom.removeConnection(%s)".format(l), () => {
		joinedUsers = joinedUsers.filterNot(i => i._1 == formatConnection(l.username,l.cometId))
		l.actor ! RoomLeaveAcknowledged(configName,location)
	})
	private def possiblyCloseRoom:Boolean = Stopwatch.time("MeTLRoom.possiblyCloseRoom", () => {
		if (joinedUsers.length == 0 && recentInterest) {
			config.getRoomProvider.removeMeTLRoom(location)
			true
		} else {
			false
		}
	})
	protected def showInterest:Unit = lastInterest = new Date().getTime
	private def recentInterest:Boolean = Stopwatch.time("MeTLRoom.recentInterest", () => {
		(new Date().getTime - lastInterest) < interestTimeout
	})
}

object EmptyRoom extends MeTLRoom("empty","empty") {
	override def getHistory = History.empty
	override protected def sendToChildren(s:MeTLStanza) = {}
	override protected def sendStanzaToServer(s:MeTLStanza) = {}
}

class NoCacheRoom(configName:String,location:String) extends MeTLRoom(configName,location) {
	override def getHistory = config.getHistory(location)
}

class HistoryCachingRoom(configName:String,location:String) extends MeTLRoom(configName,location) {
	val history = Stopwatch.time("MeTLRoom %s fetching history".format(location),() => {
		showInterest
		config.getHistory(location)
	})
	override def getHistory:History = history
	override protected def sendToChildren(s:MeTLStanza):Unit = Stopwatch.time("MeTLRoom.sendToChildren",() => {
		history.addStanza(s)
		super.sendToChildren(s)
	})
}
