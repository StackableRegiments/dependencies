package com.metl.model

import net.liftweb.common._
import scala.xml._
import com.mongodb._

object History {
	private def fetch(zone:String,room:String):List[DBObject] = Stopwatch.time("History.fetchHistory(%s,%s)".format(zone,room),()=>{
		DBFormats.withColl("history",(coll) => {
			val query = new BasicDBObject("zone",zone)
			query.put("room",room)
			coll.find(query).toArray.toArray.toList.asInstanceOf[List[DBObject]]
		}).openOr(List.empty[DBObject])	
	})
	def fetchAsJson(zone:String,room:String):String = Stopwatch.time("History.fetchHistoryAsJson(%s,%s)".format(zone,room),() => {
		fetch(zone,room).map(r => Serializer.toJson(r)) match {
			case Nil => "[]"
			case nl:List[String] => "["+nl.mkString(",")+"]"
			case other => "unknown %s".format(other)
		}
	})
	def fetchAsXml(zone:String,room:String):Node = Stopwatch.time("History.fetchHistoryAsXml(%s,%s)".format(zone,room),() => {
		fetch(zone,room).map(r => Serializer.toXml(r)) match {
			case Nil => Text("no elements found")
			case nl:List[Node] => Elem(null,"resultItems",null,TopScope,Group(nl))
			case other => Text(other.toString)
		}
	})
}
