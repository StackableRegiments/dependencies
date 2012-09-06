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

import js.JsCmds
import js.JsCmds._
import net.liftweb.http.js.jquery.JqJsCmds._

import js._
import JsCmds._
import JE._

import json.JsonAST._

class ClientMessageBroker(messageContainerId:String,messageTemplate:NodeSeq,messageSelector:String,labelSelector:String,contentSelector:String,closeSelector:String,onMessageProcessed:(JsCmd)=>Unit) {
	private var visibleMessages = List.empty[ClientMessage]
	private def removeMessage(cm:ClientMessage) = visibleMessages = visibleMessages.filterNot(m => m == cm)
	private def addMessage(cm:ClientMessage) = cm :: visibleMessages
	private def clearAllMessages = visibleMessages.map(vm => vm.done)
	def repeatVisibleMessages = {
		visibleMessages.foreach(vm => processMessage(vm))
	}
	def processMessage(cm:ClientMessage):Unit = {
		cm match {
			case s:SpamMessage => {
				val message = SpamMessage(s.content,s.title,(cmi) => {removeMessage(cmi);s.removalFunc(cmi);},s.cancellable,messageTemplate,messageSelector,labelSelector,contentSelector,closeSelector,s.uniqueId)
				val removalFunction = visibleMessages.find(m => m.uniqueId == message.uniqueId).map(em => em.done).getOrElse(Noop)
				onMessageProcessed(removalFunction & PrependHtml(messageContainerId,message.renderMessage))
				addMessage(message)
			}
			case i:InteractableMessage => {
				val message = InteractableMessage(i.scope,i.title,(cmi) => {removeMessage(cmi);i.removalFunc(cmi);},i.cancellable,messageTemplate,messageSelector,labelSelector,contentSelector,closeSelector,i.uniqueId)
				val removalFunction = visibleMessages.find(m => m.uniqueId == message.uniqueId).map(em => em.done).getOrElse(Noop)
				onMessageProcessed(removalFunction & PrependHtml(messageContainerId,message.renderMessage))
				addMessage(message)
			}
			case Clear => {
				onMessageProcessed(visibleMessages.foldLeft(Noop)((acc,item) => acc & item.done))
			}
		}
	}
}

abstract class ClientMessage(id:String, incomingTitle:Box[String] = Empty,removalFunc:(ClientMessage)=>Unit,template:NodeSeq,messageSelector:String,labelSelector:String,contentSelector:String,closeSelector:String){
  var title:Box[String] = Empty
  def entitled(t:String) = {
    title = Full(t) 
    this
  }
	var uniqueId:String = id
	def identifiedBy(t:String) = {
		uniqueId = t
		this
	}
	val content:NodeSeq
	val cancellable:Boolean = false
	val contentNode:NodeSeq = content
	val removeFromPageJs = Hide(id) & Replace(id,NodeSeq.Empty)
	def renderMessage:NodeSeq = {
		onDone(()=>removalFunc(this))
		((labelSelector+" *") #> title.openOr("Response") &
		contentSelector #> contentNode &
		(messageSelector+" [id+]") #> id &
		closeSelector #> ((n:NodeSeq) => if (cancellable) a(()=>done,n) else NodeSeq.Empty)
		).apply(template)
	}
	private type Doable = ()=>Unit
  private var doThese = List.empty[Doable]
  def done ={
    doThese.foreach(doThis =>doThis())
		removeFromPageJs
  }
  def onDone(doThis:Doable){
    doThese = doThis :: doThese
  }
}
case object Clear extends ClientMessage("clearSingleton",Empty,(cm)=>{},NodeSeq.Empty,"","","",""){
	override val content = NodeSeq.Empty
	override def renderMessage = NodeSeq.Empty
}
case class InteractableMessage(scope:InteractableMessage=>NodeSeq,incomingTitle:Box[String] = Empty,removalFunc:(ClientMessage)=>Unit = (cm) => {},override val cancellable:Boolean=true,template:NodeSeq = NodeSeq.Empty,messageSelector:String="",labelSelector:String="",contentSelector:String="",closeSelector:String="",id:String = nextFuncName) extends ClientMessage(id,incomingTitle,removalFunc,template,messageSelector,labelSelector,contentSelector,closeSelector){
  override val content = scope(this)
	override val contentNode = ajaxForm(content)
}

case class SimpleTextAreaInteractableMessage(messageTitle:String,body:String,defaultValue:String,onChanged:(String)=>Boolean, customError:Box[()=>Unit] = Empty) extends InteractableMessage((i)=>{
	var newText = defaultValue
	var answerProvided = false
	<div>
		<div>{body}</div>
		<div>
			<span>
				{text(newText,(input:String) => newText = input)}
			</span>
			<span>
				{a(()=>{
					if (!answerProvided && onChanged(newText)){
						answerProvided = true
						i.done
					} else {
						customError.map(ce => ce())
					}
					Noop
				},Text("Submit")) }
			</span>
		</div>
	</div>
},Full(messageTitle))

case class SimpleMultipleButtonInteractableMessage(messageTitle:String,body:String,buttons:Map[String,()=>Boolean], customError:Box[()=>Unit] = Empty, vertical:Boolean = true) extends InteractableMessage((i)=>{
	var answerProvided = false
	<div>
		<div>{body}</div>
		<div>
			{
				buttons.toList.map(bd => {
					val buttonName = bd._1
					val buttonAction = bd._2
					val internalButton = a(()=>{
							if (!answerProvided && buttonAction()){
								answerProvided = true
								i.done
							} else {
								customError.map(ce => ce())
								Noop
							}
						},Text(buttonName))
					if (vertical){ 
						<div>{internalButton}</div>
					} else {
						<span>{internalButton}</span>
					}
				})
			}
		</div>
	</div>	
},Full(messageTitle))

case class SimpleRadioButtonInteractableMessage(messageTitle:String,body:String,radioOptions:Map[String,()=>Boolean],defaultOption:Box[String] = Empty, customError:Box[()=>Unit] = Empty) extends InteractableMessage((i)=>{
	var answerProvided = false
	var chosenOption = defaultOption.map(dOpt => (dOpt,radioOptions(dOpt))).openOr(radioOptions.toList(0))
	<div>
		<div>{body}</div>
		<div>
			{
				radio(radioOptions.toList.map(optTuple => optTuple._1),Full(chosenOption._1),(chosen:String) => chosenOption = (chosen,radioOptions(chosen))).toForm
			}		
			<div>
				{a(()=>{
					if (!answerProvided && chosenOption._2()){
						answerProvided = true
						i.done
					} else {
						customError.map(ce => ce())
						Noop
					}
					Noop
				},Text("Submit")) }
			</div>
		</div>
	</div>	
},Full(messageTitle))

case class SimpleDropdownInteractableMessage(messageTitle:String,body:String,dropdownOptions:Map[String,()=>Boolean],defaultOption:Box[String] = Empty,customError:Box[()=>Unit] = Empty) extends InteractableMessage((i)=>{
	var answerProvided = false
	var chosenOption = defaultOption.map(dOpt => (dOpt,dropdownOptions(dOpt))).openOr(dropdownOptions.toList(0))
	<div>
		<div>{body}</div>
		<div>
			{
				select(dropdownOptions.toList.map(optTuple => (optTuple._1,optTuple._1)),Full(chosenOption._1),(chosen:String) => chosenOption = (chosen,dropdownOptions(chosen)))
			}		
			<div>
				{a(()=>{
					if (!answerProvided && chosenOption._2()){
						answerProvided = true
						i.done
					} else {
						customError.map(ce => ce())
						Noop
					}
					Noop
				},Text("Submit")) }
			</div>
		</div>
	</div>	
},Full(messageTitle))

case class SpamMessage(content:NodeSeq,incomingTitle:Box[String] = Empty,removalFunc:(ClientMessage)=>Unit = (cm) => {},override val cancellable:Boolean=true,template:NodeSeq = NodeSeq.Empty,messageSelector:String="",labelSelector:String="",contentSelector:String="",closeSelector:String="",id:String = nextFuncName) extends ClientMessage(id,incomingTitle,removalFunc,template,messageSelector,labelSelector,contentSelector,closeSelector){
	override val contentNode = a(() => done,content)
}
