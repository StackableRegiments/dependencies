package com.metl.liftExtensions

import com.metl.utils._
import com.metl.data._

import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.util.Helpers._
import net.liftweb.util._

object ServerConfigurationHydrator{
	def loadFromProps:String = {
		Props.get("serverConfiguration") match {
			case Full(s) => {
				println(s)
				s.toString
			}
			case _ => "not found"
		}
	}
}

