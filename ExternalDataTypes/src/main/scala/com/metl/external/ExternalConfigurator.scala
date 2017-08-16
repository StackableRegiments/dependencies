package com.metl.external

import net.liftweb.common.Logger

import scala.xml.NodeSeq

abstract class ExternalConfigurator[T] extends Logger {
  def configureFromXml(in:NodeSeq):Either[Exception,List[T]]
}