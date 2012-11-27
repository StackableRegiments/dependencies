package com.metl.test

import scala.xml._

import com.metl.data._
import Privacy._

trait QueryXml {

    @throws(classOf[IllegalArgumentException])
    def queryXml[T](label: String)(implicit mf: ClassManifest[T], content: NodeSeq):T = {

        lazy val xml = XmlUtils

        val dispatch = Map(
          classOf[Privacy] -> ((node: NodeSeq, elem: String) => xml.getPrivacyByName(node, elem)),
          classOf[Color] -> ((node: NodeSeq, elem: String) => xml.getColorByName(node, elem)),
          classOf[String] -> ((node: NodeSeq, elem: String) => xml.getStringByName(node, elem)),
          classOf[Boolean] -> ((node: NodeSeq, elem: String) => xml.getBooleanByName(node, elem)),
          classOf[Double] -> ((node: NodeSeq, elem: String) => xml.getDoubleByName(node, elem)),
          classOf[Long] -> ((node: NodeSeq, elem: String) => xml.getLongByName(node, elem)),
          classOf[Int] -> ((node: NodeSeq, elem: String) => xml.getIntByName(node, elem))
        )

        (() => dispatch.find(_._1 isAssignableFrom mf.erasure).map(_._2))() match {
            case Some(x) => x(content, label).asInstanceOf[T]
            case None => throw new IllegalArgumentException("Type not supported") 
        }
    }
}
