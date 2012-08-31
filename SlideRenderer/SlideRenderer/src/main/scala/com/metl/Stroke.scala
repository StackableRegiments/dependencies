package com.metl.renderer

import java.awt.Shape
import java.awt.geom._
import net.liftweb.util.Helpers._
import com.metl.model._

case class Vector2(x:Double,y:Double) {
	def +(v:Vector2) = new Vector2(x+v.x,y+v.y)
	def -(v:Vector2) = new Vector2(x-v.x,y-v.y)
	def *(n:Double) = new Vector2(x*n,y*n)

	def length = Math.sqrt(x*x+y*y)
	def normalized ={
		val l = 1.0/length
		new Vector2(x*l,y*l)
	}

	def leftRot90 = new Vector2(y,-x)
	def rightRot90 = new Vector2(-y,x)

	override def toString = "[%f,%f]".format(x,y)
}

class Stroke(points:List[Point],thickness:Double) extends Path2D.Double {
	makeShape

	private def offsetAt(point:Point) = point.thickness/256*thickness

	private def leftPoint(start:Vector2,end:Vector2,dist:Double) ={
		val actual = end-start
		val perp = actual.leftRot90
		val norm = perp.normalized
		val point = norm*dist
		point+end
	}

	private def rightPoint(start:Vector2,end:Vector2,dist:Double) ={
		val actual = end-start
		val perp = actual.rightRot90
		val norm = perp.normalized
		val point = norm*dist
		point+end
	}

	private def addSegment(start:Point,end:Point) ={
		val vStart = Vector2(start.x,start.y)
		val vEnd = Vector2(end.x,end.y)

		val v1 = leftPoint(vStart,vEnd,offsetAt(end))
		val v2 = rightPoint(vStart,vEnd,offsetAt(end))   // XXX optimise, invert vEnd->v1
		val v3 = leftPoint(vEnd,vStart,offsetAt(start))
		val v4 = rightPoint(vEnd,vStart,offsetAt(start)) // XXX optimise, invert vStart->v3

		moveTo(v1.x,v1.y)
		lineTo(v2.x,v2.y)
		lineTo(v3.x,v3.y)
		lineTo(v4.x,v4.y)
		lineTo(v1.x,v1.y)
	}

	private def addPoint(p:Point) ={
		val offset = offsetAt(p)
		append(new Ellipse2D.Double(p.x-offset,p.y-offset,offset*2,offset*2),false)
	}

	private def makeShape ={
		val (first,rest) = points.splitAt(1)
		addPoint(first.head)
		rest.foldLeft(first.head)((prev,current) => {
			addPoint(current)
			addSegment(prev,current)
			current
		})
	}
}
