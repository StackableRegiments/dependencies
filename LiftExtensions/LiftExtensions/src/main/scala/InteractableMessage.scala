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

case class InteractableMessage(title:String,body:NodeSeq)

