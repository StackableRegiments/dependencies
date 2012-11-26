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

class ClientMessageBroker(messageTemplate:NodeSeq,messageSelector:String,labelSelector:String,contentSelector:String,closeSelector:String,onMessageArrival:(ClientMessage)=>Unit,onMessageRemoval:(ClientMessage)=>Unit) {
	private var visibleMessages = List.empty[ClientMessage]
	private def removeMessageFromVisible(cm:ClientMessage) = visibleMessages = visibleMessages.filterNot(m => m == cm)
	private def addMessageToVisible(cm:ClientMessage) = {
		visibleMessages = cm :: visibleMessages
	}
	private def clearAllMessages = visibleMessages.map(vm => vm.done)
	def repeatVisibleMessages = {
		visibleMessages.foreach(vm => processMessage(vm))
	}
	def removeMessage(cm:ClientMessage):Unit = {
		visibleMessages.find(m => m.uniqueId == cm.uniqueId).map(m => {
			onMessageRemoval(m)
			removeMessageFromVisible(m)
		})
	}
	def processMessage(cm:ClientMessage):Unit = {
		cm match {
			case s:SpamMessage => {
				val message = SpamMessage(s.content,s.role,s.title,(cmi) => {
					removeMessage(cmi);
					s.removalFunc(cmi);
				},s.cancellable,messageTemplate,messageSelector,labelSelector,contentSelector,closeSelector,s.uniqueId)
				removeMessage(message)
				onMessageArrival(message)
				addMessageToVisible(message)
			}
			case i:InteractableMessage => {
				val message = InteractableMessage(i.scope,i.role,i.title,(cmi) => {
					removeMessage(cmi);
					i.removalFunc(cmi);
				},i.cancellable,messageTemplate,messageSelector,labelSelector,contentSelector,closeSelector,i.uniqueId)
				removeMessage(message)
				onMessageArrival(message)
				addMessageToVisible(message)
			}
			case Clear => {
				visibleMessages.foreach(removeMessage)	
			}
		}
	}
}

abstract class ClientMessage(id:String, incomingRole:Box[String] = Empty, incomingTitle:Box[String] = Empty,removalFunc:(ClientMessage)=>Unit,template:NodeSeq,messageSelector:String,labelSelector:String,contentSelector:String,closeSelector:String){
  var title:Box[String] = incomingTitle
  def entitled(t:String) = {
    title = Full(t) 
    this
  }
	var uniqueId:String = id
	def identifiedBy(t:String) = {
		uniqueId = t
		this
	}
	val role:Box[String] = incomingRole
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
    doThese.foreach(doThis => {
			doThis()
		})
		removeFromPageJs
  }
  def onDone(doThis:Doable){
    doThese = doThis :: doThese
  }
}
case object Clear extends ClientMessage("clearSingleton",Empty,Empty,(cm)=>{},NodeSeq.Empty,"","","",""){
	override val content = NodeSeq.Empty
	override def renderMessage = NodeSeq.Empty
}
case class InteractableMessage(scope:InteractableMessage=>NodeSeq,override val role:Box[String] = Empty,incomingTitle:Box[String] = Empty,removalFunc:(ClientMessage)=>Unit = (cm) => {},override val cancellable:Boolean=true,template:NodeSeq = NodeSeq.Empty,messageSelector:String="",labelSelector:String="",contentSelector:String="",closeSelector:String="",id:String = nextFuncName) extends ClientMessage(id,role,incomingTitle,removalFunc,template,messageSelector,labelSelector,contentSelector,closeSelector){
  override val content = scope(this)
	override val contentNode = ajaxForm(content)
}

case class SimpleTextAreaInteractableMessage(messageTitle:String,body:String,defaultValue:String,onChanged:(String)=>Boolean, customError:Box[()=>Unit] = Empty, override val role:Box[String] = Empty) extends InteractableMessage((i)=>{
	var answerProvided = false
	<div>
		<div>{body}</div>
		<div>
			<span>
				{text(defaultValue,(input:String) => {
					if (!answerProvided && onChanged(input)){
						answerProvided = true
						i.done
					} else {
						customError.map(ce => ce())
					}
				},("class","simpleTextAreaInteractableMessageTextarea"))}
			</span>
			<span>
				{submit("Submit", ()=>Noop)}
			</span>
		</div>
	</div>
},role,Full(messageTitle))

case class SimpleMultipleButtonInteractableMessage(messageTitle:String,body:String,buttons:Map[String,()=>Boolean], customError:Box[()=>Unit] = Empty, vertical:Boolean = true,override val role:Box[String] = Empty) extends InteractableMessage((i)=>{
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
						<div class="simpleMultipleButtonInteractableMessageButton">{internalButton}</div>
					} else {
						<span class="simpleMultipleButtonInteractableMessageButton">{internalButton}</span>
					}
				})
			}
		</div>
	</div>	
},role,Full(messageTitle))

case class SimpleRadioButtonInteractableMessage(messageTitle:String,body:String,radioOptions:Map[String,()=>Boolean],defaultOption:Box[String] = Empty, customError:Box[()=>Unit] = Empty,override val role:Box[String] = Empty) extends InteractableMessage((i)=>{
	var answerProvided = false
	<div>
		<div>{body}</div>
		<div>
			{
				radio(radioOptions.toList.map(optTuple => optTuple._1),defaultOption,(chosen:String) => {
					if (!answerProvided && radioOptions(chosen)()){
						answerProvided = true
						i.done
					} else {
						customError.map(ce => ce())
					}
				},("class","simpleRadioButtonInteractableMessageButton")).toForm
			}		
			<div>
				{submit("Submit", ()=> Noop) }
			</div>
		</div>
	</div>	
},role,Full(messageTitle))

case class SimpleDropdownInteractableMessage(messageTitle:String,body:String,dropdownOptions:Map[String,()=>Boolean],defaultOption:Box[String] = Empty,customError:Box[()=>Unit] = Empty,override val role:Box[String] = Empty) extends InteractableMessage((i)=>{
	var answerProvided = false
	<div>
		<div>{body}</div>
		<div>
			{
				select(dropdownOptions.toList.map(optTuple => (optTuple._1,optTuple._1)),defaultOption,(chosen:String) => {
					if (!answerProvided && dropdownOptions(chosen)()){
						answerProvided = true
						i.done
					} else {
						customError.map(ce => ce())
					}
				},("class","simpleDropdownInteractableMessageDropdown"))
			}		
			<div>
				{submit("Submit",()=> Noop) }
			</div>
		</div>
	</div>	
},role,Full(messageTitle))

case class SpamMessage(content:NodeSeq,override val role:Box[String] = Empty,incomingTitle:Box[String] = Empty,removalFunc:(ClientMessage)=>Unit = (cm) => {},override val cancellable:Boolean=true,template:NodeSeq = NodeSeq.Empty,messageSelector:String="",labelSelector:String="",contentSelector:String="",closeSelector:String="",id:String = nextFuncName) extends ClientMessage(id,role,incomingTitle,removalFunc,template,messageSelector,labelSelector,contentSelector,closeSelector){
	override val contentNode = a(() => done,content)
}
