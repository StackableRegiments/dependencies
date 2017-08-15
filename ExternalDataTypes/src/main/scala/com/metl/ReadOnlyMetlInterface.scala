package com.metl

trait ReadOnlyMetlInterface {
  def boardFor():String
  def boardFor(conversationJid:Int):String
  def boardFor(conversationJid:Int,slideId:Int):String
  def noBoard:String
  def remotePluginConversationChooser(ltiToken:String):String
}
