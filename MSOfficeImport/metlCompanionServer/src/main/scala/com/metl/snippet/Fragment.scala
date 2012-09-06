package com.metl.snippet

import net.liftweb.http._
import js.JsCmds
import js.JsCmds._
import net.liftweb.http.SHtml._
import net.liftweb.common._
import S._
import net.liftweb.util._
import Helpers._
import scala.xml._
import collection.mutable.ListBuffer
import com.metl.model._
import com.metl.model.Globals._

class Fragment{
  def render(x:NodeSeq) = {
    S.skipDocType=true
    S.skipXmlHeader=true
    x
  }
}
