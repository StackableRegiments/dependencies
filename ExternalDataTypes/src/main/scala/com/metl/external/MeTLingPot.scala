package com.metl.external

import net.liftweb.json.JField

abstract class ExternalMeTLingPotAdaptorConfigurator extends ExternalConfigurator[MeTLingPotAdaptor]

case class KVP(`type`:String,name:String)
case class MeTLingPotItem(source:String,timestamp:Long,actor:KVP,action:KVP,context:Option[KVP],target:Option[KVP],value:Option[String])

trait MeTLingPotAdaptor {
  def postItems(items:List[MeTLingPotItem]):Either[Exception,Boolean]
  def search(after:Long,before:Long,queries:Map[String,List[String]]):Either[Exception,List[MeTLingPotItem]]
  def init:Unit = {}
  def shutdown:Unit = {}
  def description:List[JField] = { List.empty[JField] }
}
