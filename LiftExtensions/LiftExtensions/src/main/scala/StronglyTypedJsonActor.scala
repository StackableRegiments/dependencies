package com.metl.liftExtensions

import com.metl.utils._

import net.liftweb._
import common._
import http._
import util._
import Helpers._
import HttpHelpers._
import actor._
import scala.xml._
import SHtml._

import js._
import JsCmds._
import JE._

import json.JsonAST._

object ClientUpdate{
  def unapply(json:JValue) = json match{
    case JObject(List(JField("command",JString(command)), JField("params",args))) => Some(command,args)
    case _ => None
  }
}
object JNum{
  def unapply(json:JValue) = json match{
    case JInt(x) => Some(x.toDouble)
    case JDouble(x) => Some(x)
    case _ => None
  }
}
case class ClientSideFunctionDefinition(name:String,args:List[String],serverSideFunc:List[Any]=>JValue,returnResultFunction:Box[String])
abstract class StronglyTypedJsonActor extends CometActor with CometListener {
	protected val functionDefinitions:List[ClientSideFunctionDefinition]
	case class ClientSideFunction(name:String,args:List[String],serverSideFunc:List[Any]=>JValue,returnResultFunction:Box[String]){
		private def deconstructArgs(funcArgs:JValue):List[Any] = funcArgs match {
			case JArray(listOfFields) => listOfFields
			case JString(s) => List(s)
			case JNum(d) => List(d)
			case JNull => List.empty[String]
			case obj:JValue => List(obj)
			case _ => List.empty[String]
		}
		private def matchesRequirements(funcArgs:JValue):Boolean = {
			args.length match {
				case 0 => funcArgs == Nil
				case 1 => funcArgs match {
					case u:JArray => false
					case u:JValue => true
					case _ => false
				}
				case other => funcArgs match {	
					case u:JArray => u.arr.length == other
					case _ => false
				}
			}
		}
		private val jsonifiedArgs:JsExp = args match {
			case Nil => JNull
			case List(arg) => JsRaw(arg)
			case l:List[String] if (l.length > 1) => JsRaw("[%s]".format(l.mkString(",")))
			case _ => JNull
		}
		val jsCreationFunc = Script(Function(name,args,jsonSend(name,jsonifiedArgs)))
		def matchingFunc(input:Any):JsCmd = input match {
			case funcArgs:JValue if matchesRequirements(funcArgs) => {
				val output = Stopwatch.time("MeTLActor.ClientSideFunction.%s.serverSideFunc".format(name),() => {
					serverSideFunc(deconstructArgs(funcArgs))
				})
				returnResultFunction.map(rrf => {
					partialUpdate(Call(rrf,output))
				})
				Noop.asInstanceOf[JsCmd]
			}
			case _ => Noop 
		}
	}
	val strongFuncs = Map(functionDefinitions.map(fd => (fd.name,ClientSideFunction(fd.name,fd.args,fd.serverSideFunc,fd.returnResultFunction))):_*)
  val functions = NodeSeq.fromSeq(strongFuncs.values.map(_.jsCreationFunc).toList)
	override def fixedRender = {
		println("setting up functions: %s".format(functions))
		Stopwatch.time("StronglyTypedJsonActor.fixedRender", () => functions)
	}
	override def receiveJson = {
		case ClientUpdate(commandName,commandParams) => strongFuncs(commandName).matchingFunc(commandParams)
    case other => println("receiveJson: %s".format(other))
  }
}
