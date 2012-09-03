package com.metl.model

import net.liftweb.util.Helpers._
import java.util.Date
import net.liftweb.actor.LiftActor
import scala.collection.mutable.{HashMap, SynchronizedMap}
import net.liftweb.util.ActorPing
import org.apache.commons.codec.digest.DigestUtils
import com.metl.renderer.SlideRenderer

case object RefreshHistory
case class UpdateHistoryWithQuizResponse(quizResponse:MeTLQuizResponse)

case class CachedBinary(data:Array[Byte],createTime:Long) {
	lazy val checksum = DigestUtils.shaHex(data)
}

class CachedHistory(id:CachedHistoryIdentifier) extends LiftActor {
	val jid = id.jid
	val server = id.server

	private val acceptableStaleTime = 10000
	private val refreshTime = 30 seconds

	private var history:History = fetchHistory

	private var snapshot:Map[SnapshotSize.Value,Array[Byte]] = makeSnapshots

	private var lastRequested:Long = 0L
	ActorPing.schedule(this,RefreshHistory,refreshTime)
	
	private def fetchHistory = Stopwatch.time("CachedHistory_%s@%s fetchingHistory".format(jid,server.host),()=> tryo(server.getHistory(jid)).openOr(History.empty))

	private def makeSnapshot(res:SnapshotResolution) = Stopwatch.time("CachedHistory_%s@%s makingSnapshot".format(jid,server.host), ()=> SlideRenderer.render(history,res.width,res.height))

	private def makeSnapshots = Globals.snapshotSizes.map(p => (p._1 -> makeSnapshot(p._2)))

	override def messageHandler = {
		case UpdateHistoryWithQuizResponse(response) => {
			history.addQuizResponse(response)
		}
		case RefreshHistory => {
			if ((new Date().getTime - lastRequested) > acceptableStaleTime){
				HistoryCache.remove(jid,server)
			}
			else {
				history = fetchHistory
				snapshot = makeSnapshots
				ActorPing.schedule(this,RefreshHistory,refreshTime)
			}
		}
		case _ => {}
	}
	def getHistory:History = {
		lastRequested = new Date().getTime
		history
	}
	def getSnapshot(size:SnapshotSize.Value):CachedBinary = {
		lastRequested = new Date().getTime
		CachedBinary(snapshot(size), lastRequested)
	}
	def getQuizImage(id:String):CachedBinary = {
		lastRequested = new Date().getTime
		CachedBinary(history.getQuizzes.find(q => q.id == id).map(q => q.imageBytes.openOr(Array.empty[Byte])).getOrElse(Array.empty[Byte]), lastRequested)
	}
}

case class CachedHistoryIdentifier(jid:String,server:ServerConfiguration)

object HistoryCache {
	private val histories = new HashMap[CachedHistoryIdentifier,CachedHistory] with SynchronizedMap[CachedHistoryIdentifier,CachedHistory]{
		override def default(where:CachedHistoryIdentifier) = {
			val newCachedHistory = new CachedHistory(where)
			this += (where -> newCachedHistory)
			newCachedHistory
		}
	}
	def remove(jid:String,server:ServerConfiguration):Unit = {
		Stopwatch.time("HistoryCache expiring: %s@%s".format(jid,server.name),()=> histories.remove(CachedHistoryIdentifier(jid,server)))
	}

	def getHistory(jid:String,server:ServerConfiguration) = {
		Stopwatch.time("HistoryCache fetching history: %s@%s".format(jid,server.name),()=> histories(CachedHistoryIdentifier(jid,server)).getHistory)
	}
	def getSnapshot(jid:String,server:ServerConfiguration,size:SnapshotSize.Value) = {
		Stopwatch.time("HistoryCache fetching snapshot: %s@%s size %s".format(jid,server.name,size),()=> histories(CachedHistoryIdentifier(jid,server)).getSnapshot(size))
	}
	def getQuizImage(jid:String,server:ServerConfiguration,id:String) = {
		Stopwatch.time("HistoryCache fetching quizImage: %s@%s %s".format(jid,server.name,id),()=> histories(CachedHistoryIdentifier(jid,server)).getQuizImage(id))
	}

	def addQuizResponseToHistory(jid:String,server:ServerConfiguration,quizResponse:MeTLQuizResponse) = {
		val id = CachedHistoryIdentifier(jid,server)
		if (histories.contains(id))
			histories(id) ! UpdateHistoryWithQuizResponse(quizResponse)
	}
}
