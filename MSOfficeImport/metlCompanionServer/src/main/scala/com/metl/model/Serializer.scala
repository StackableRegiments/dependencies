package com.metl.model

import net.liftweb.common._
import com.mongodb._

import scala.xml._

object Serializer {
	def toJson(obj:DBObject):String = Stopwatch.time("Serializer.toJson", () => {
		obj.toString
	})
	def toXml(obj:DBObject):Node = Stopwatch.time("Serializer.toXml", () => {
		Elem(null,"resultItem",null,TopScope,toXmlGroup(obj))
	})	
	private def toXmlGroup(obj:DBObject):Node = Stopwatch.time("Serializer.toXmlGroup", () => {
		Group(obj.toMap.entrySet.toArray.toList.map(kv => (kv.asInstanceOf[java.util.Map.Entry[String,Object]].getKey,kv.asInstanceOf[java.util.Map.Entry[String,Object]].getValue)).map(a => toXmlElem(a)))
	})
	private def toXmlElem(value:Object):Node = Stopwatch.time("Serializer.toXmlElem", () => {
		value match {
			case (k:String,v:Object) => Elem(null,k,null,TopScope,toXmlElem(v))
			case s:String => Text(s)
			case dl:BasicDBList => Group(dl.toArray.map(i => toXmlElem(("item",i))))
			case v:DBObject => toXmlGroup(v)
			case other => Text(other.toString)
		}
	})
}
