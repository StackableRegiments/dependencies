package com.metl.model

import net.liftweb.common._

object Resources {
	def upload(filename:String,data:Box[Array[Byte]]):String = Stopwatch.time("Resources.upload", () => {
		data.map(d => {
			DBFormats.saveFile(filename,d)
		}).openOr("no data supplied")
	})
	def download(objectId:String):Array[Byte] = Stopwatch.time("Resources.download", () => {
		DBFormats.retrieveFile(objectId)	
	})
}
