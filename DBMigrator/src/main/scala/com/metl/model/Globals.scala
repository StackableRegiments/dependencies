package com.metl.model

import net.liftweb.common.Logger
import net.liftweb.util.Props
import net.liftweb.util.Helpers._

import scala.xml.{Elem, NodeSeq, XML}

case class PropertyNotFoundException(key: String) extends Exception(key) {
  override def getMessage: String = "Property not found: " + key
}

trait PropertyReader extends Logger {
  def readProperty(key: String, default: Option[String] = None): String = traceIt("readProperty",key,{
    Props.get(key).getOrElse(default.getOrElse(throw PropertyNotFoundException(key)))
  })

  protected def traceIt[A](label:String,param:String,in: => A):A = {
    val res = in
    trace("%s(%s) : %s".format(label,param,in))
    res
  }

  def readNodes(node: NodeSeq, tag: String): Seq[NodeSeq] = traceIt("readNodes",tag,node \\ tag)
  def readNode(node: NodeSeq, tag: String): NodeSeq = traceIt("readNode",tag,readNodes(node, tag).headOption.getOrElse(NodeSeq.Empty))
  def readText(node: NodeSeq, tag: String): Option[String] = traceIt("readText",tag,readNodes(node, tag).headOption.map(_.text))
  def readInt(node:NodeSeq,tag:String):Option[Int] = traceIt("readInt",tag,readNodes(node,tag).headOption.map(_.text.toInt))
  def readLong(node:NodeSeq,tag:String):Option[Long] = traceIt("readLong",tag,readNodes(node,tag).headOption.map(_.text.toLong))
  def readBool(node:NodeSeq,tag:String):Option[Boolean] = traceIt("readBool",tag,readNodes(node,tag).headOption.map(_.text.toBoolean))
  def readTimespan(node:NodeSeq,tag:String):Option[TimeSpan] = traceIt("readTimespan",tag,readNodes(node,tag).headOption.map(v => TimeSpanParser.parse(v.text)))
  def readMandatoryText(node: NodeSeq, tag: String): String = traceIt("readMandatoryText",tag,readNodes(node, tag).headOption.map(_.text match {
    case s: String if s.trim.isEmpty => throw new Exception("mandatory field (%s) not supplied in expected node %s".format(tag, node))
    case other                       => other.trim
  }).getOrElse({
    throw new Exception("mandatory field (%s) not supplied in expected node %s".format(tag, node))
  }))
  def readAttribute(node:NodeSeq,attrName:String):String = traceIt("readAttribute",attrName,node match {
    case e:Elem => e.attribute(attrName).map(a => a.text).getOrElse("")
    case _ => ""
  })
  def readMandatoryAttribute(node:NodeSeq,attrName:String):String = traceIt("readMandatoryAttribute",attrName,readAttribute(node,attrName) match {
    case s: String if s.trim.isEmpty => throw new Exception("mandatory attr (%s) not supplied in expected node %s".format(attrName, node))
    case other                       => other.trim
  })
}

object Globals extends PropertyReader with Logger {
  val configurationFileLocation: String = System.getProperty("metlx.configurationFile")
  List(configurationFileLocation).filter(prop => prop match {
    case null => true
    case "" => true
    case _ => false
  }) match {
    case Nil => {}
    case any => {
      val e = new Exception("properties not provided, server cannot start")
      error("please ensure that the following properties are set on the command-line when starting the WAR: %s".format(any),e)
      throw e
    }
  }

  val propFile: Elem = XML.load(configurationFileLocation)
  val h2ThreadPoolMultiplier: Int = readInt(propFile,"h2ThreadPoolMultiplier").getOrElse(8)
}
