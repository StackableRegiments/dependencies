package com.metl.snippet

import com.metl.data._
import com.metl.utils._


import net.liftweb._
import http._
import SHtml._
import common._
import util._
import Helpers._
import scala.xml._
import com.metl.comet._
import com.metl.model._
import Globals._

class MeTL {
  def specific = {
    val name = "%s".format(Globals.currentUser.is)
    val clazz = "lift:comet?type=MeTLActor&amp;name=%s".format(name)
    <span class={clazz}>
    </span>
  }
}
