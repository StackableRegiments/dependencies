package com.metl.renderer

import com.metl.data._
import com.metl.utils._

import java.awt.{Color=>AWTColor,List=>AWTList,_}
import java.awt.image._
import java.awt.font._
import java.awt.geom.AffineTransform
import java.text._
import javax.imageio.ImageIO
import java.io.ByteArrayInputStream
import com.bric.geom.BasicVectorizer
import net.liftweb.util.Helpers._

case class Dimensions(left:Double,top:Double,right:Double,bottom:Double,width:Double,height:Double)

object SlideRenderer {

	private val JAVA_DEFAULT_DPI = 72.0
	private val WINDOWS_DEFAULT_DPI = 96.0

	private def makeBlankImage(width:Int,height:Int) = Stopwatch.time("SlideRenderer.makeBlankImage", () => {
		val blankImage = new BufferedImage(width,height,BufferedImage.TYPE_3BYTE_BGR)
		val g = blankImage.createGraphics.asInstanceOf[Graphics2D]
		g.setPaint(AWTColor.white)
		g.fill(new Rectangle(0,0,width,height))
		blankImage
	})

	private def imageToByteArray(image:BufferedImage):Array[Byte] = Stopwatch.time("SlideRenderer.imageToByteArray", () => {
		val stream = new java.io.ByteArrayOutputStream
		ImageIO.write(image, "jpg", stream)
		stream.toByteArray
	})
		
	//We assume it came from windows, and that any headful dev env is Windows.
	private def correctFontSizeForOsDpi(size:Double):Double = {
		if(java.awt.GraphicsEnvironment.isHeadless) 
			Math.round(size * JAVA_DEFAULT_DPI / WINDOWS_DEFAULT_DPI)
		else size
	}

	def toAwtColor(c:Color,overrideAlpha:Int = -1):AWTColor = {
		if (overrideAlpha < 0)
			new AWTColor(c.red,c.green,c.blue,c.alpha)
		else
			new AWTColor(c.red,c.green,c.blue,Math.max(0,Math.min(255,overrideAlpha)))
	}
	private val emptyImage:Image = new BufferedImage(1,1,BufferedImage.TYPE_4BYTE_ABGR)
	private def getImageFor(metlImage:MeTLImage):Image = Stopwatch.time("SlideRenderer.getImageFor", () => {
		metlImage.imageBytes.map(ib => {
			val stream = new ByteArrayInputStream(ib)
			val image = ImageIO.read(stream).asInstanceOf[Image]
			stream.close()
			image
		}).openOr(emptyImage)
	})
	private val defaultObserver:Graphics2D = {
		val tempImage = new BufferedImage(1,1,BufferedImage.TYPE_3BYTE_BGR)
		tempImage.createGraphics.asInstanceOf[Graphics2D]
	}
	def measureImage(metlImage:MeTLImage):Dimensions = measureImage(metlImage,defaultObserver)
	private def measureImage(metlImage:MeTLImage,g:Graphics2D):Dimensions = Stopwatch.time("SlideRenderer.measureImage", () => {
		val x = metlImage.x
		val y = metlImage.y
		val errorSize = Dimensions(x,y,x,y,0.0,0.0)
		try {
			val image:Image = getImageFor(metlImage)
			(metlImage.height,metlImage.width) match {
				case (h:Double,w:Double) if (h.isNaN || w.isNaN) => {
					val imageObserver = new Canvas(g.getDeviceConfiguration)
					val internalHeight = h match {
						case d:Double if (d.isNaN) => {
							val observedHeight = image.getHeight(imageObserver)
							observedHeight * metlImage.scaleFactorY
						}
						case d:Double => d
						case _ => 0.0
					}
					val internalWidth = w match {
						case d:Double if (d.isNaN) => {
							val observedWidth = image.getWidth(imageObserver)
							observedWidth * metlImage.scaleFactorX
						}
						case d:Double => d
						case _ => 0.0
					}
					Dimensions(x,y,x+internalWidth,y+internalHeight,internalWidth,internalHeight)
				}
				case (h:Double,w:Double) => Dimensions(x,y,x+w,y+h,w,h)
				case _ => errorSize	
			}
		} catch {
			case e:Throwable => {
				e.printStackTrace
				println("failed to measure image: %s with exception %s".format(metlImage,e.getMessage))
				errorSize
			}
		}
	})
	private def renderImage(metlImage:MeTLImage,g:Graphics2D):Unit = Stopwatch.time("SlideRenderer.renderImage", () => {
		try {
			val image:Image = getImageFor(metlImage)
			val dimensions = measureImage(metlImage,g)
			val (finalWidth,finalHeight) = (dimensions.width,dimensions.height)
			image match {
				case i:Image if (finalHeight == 0.0 || finalWidth == 0.0) => {}
				case i:Image => g.drawImage(image,metlImage.left.toInt,metlImage.top.toInt,finalWidth.toInt,finalHeight.toInt,null)
				case _ => {}
			}
		} catch {
			case e:Throwable => {
				e.printStackTrace
				println("failed to render image: %s with exception %s".format(metlImage, e.getMessage))
			}
		}
	})

	private def renderInk(metlInk:MeTLInk,g:Graphics2D) = Stopwatch.time("SlideRenderer.renderInk", () => {
		try {
			val HIGHLIGHTER_ALPHA  = 55
			val PRESSURE = 0.22
			val color = metlInk.isHighlighter match {
				case true => toAwtColor(metlInk.color, HIGHLIGHTER_ALPHA)
				case false => toAwtColor(metlInk.color)
			}
			g.setPaint(color)
			g.fill(new Stroke(metlInk.points,metlInk.thickness))
		} catch {
			case e:Throwable => {
				e.printStackTrace
				println("failed to render ink: %s with exception %s".format(metlInk, e.getMessage))
			}
		}
	})

	case class PreparedTextLine(text:String,layout:TextLayout,x:Float,y:Float,width:Float,height:Float,color:Color)
	def measureText(metlText:MeTLText):Dimensions = measureText(metlText,defaultObserver)
	private def measureText(metlText:MeTLText,g:Graphics2D):Dimensions = Stopwatch.time("SlideRenderer.measureText", () => {
		val (l,r,t,b) = measureTextLines(metlText,g).foldLeft((metlText.x,metlText.y,metlText.x,metlText.y))((internalAcc,internalItem) => {
			val newLeft = Math.min(internalAcc._1,internalItem.x)
			val newRight = Math.max(internalAcc._2,internalItem.x+internalItem.width)
			val newTop = Math.min(internalAcc._3,internalItem.y)
			val newBottom = Math.max(internalAcc._4,internalItem.y+internalItem.height)
			(newLeft,newRight,newTop,newBottom)
		})
		Dimensions(l,t,r,b,r-l,b-t)
	})
	private def measureTextLines(metlText:MeTLText,g:Graphics2D):List[PreparedTextLine] = Stopwatch.time("SlideRenderer.measureTextLines", () => {
		val frc = g.getFontRenderContext()

		val font = new Font(metlText.family, metlText.weight match{
			case "Normal" => metlText.style match {
				case s:String if s.contains("Italic") => Font.ITALIC
				case _ => Font.PLAIN
			}
			case "Bold" => metlText.style match {
				case s:String if s.contains("Italic") => Font.BOLD + Font.ITALIC
				case _ => Font.BOLD
			}
			case _ => {
				"renderText: I don't know what to do with font weight '%s'".format(metlText.weight)
				Font.PLAIN
			}
		}, correctFontSizeForOsDpi(metlText.size).toInt)

		val metrics = g.getFontMetrics(font)
		val blankLineHeight = metrics.getHeight

		metlText.text match {
			case t:String if (t.length > 0) => {
				def drawLines(preparedLines:List[PreparedTextLine],lines:List[String],y:Float):List[PreparedTextLine] ={
					if (lines.length > 0) {
						val line = lines.head
						if (line.length > 0){
							val styledText = new AttributedString(line)
							val stubLayout = new TextLayout(line,font,frc)

							styledText.addAttribute(TextAttribute.FONT, font)
							if(metlText.decoration.contains("Underline"))
								styledText.addAttribute(TextAttribute.UNDERLINE, TextAttribute.UNDERLINE_ON, 0, line.length)
							if(metlText.decoration.contains("Strikethrough"))
								styledText.addAttribute(TextAttribute.STRIKETHROUGH, TextAttribute.STRIKETHROUGH_ON, 0, line.length)
							val (nextPreparedLines,nextY) = metlText.width match{
								case w:Double if (w.isNaN || w < 0) => {
									val textLayout = new TextLayout(styledText.getIterator,frc)
									val nextY = y+blankLineHeight
									val newLine = PreparedTextLine(line,textLayout,metlText.x.toFloat,nextY,textLayout.getBounds.getWidth.toFloat,blankLineHeight,metlText.color)
									(List(newLine),nextY)
								}
								case _ =>{
									val styledTextIterator = styledText.getIterator()
									val measurer = new LineBreakMeasurer(styledTextIterator, frc)
									def renderLine(internalPreparedLines:List[PreparedTextLine],lineY:Float,wraps:Int = 0):Tuple2[List[PreparedTextLine],Float] = {
										if (measurer.getPosition() < t.length()){
											val textLayout = measurer.nextLayout(metlText.width.toFloat)
											if (textLayout != null){
												val currentY = lineY + textLayout.getAscent()
												val totalHeight = textLayout.getDescent + textLayout.getLeading

												val newLines = PreparedTextLine(line,textLayout,metlText.x.toFloat,currentY,textLayout.getBounds.getWidth.toFloat,totalHeight,metlText.color) :: internalPreparedLines
												renderLine(newLines,currentY+totalHeight, wraps + 1)
											}
											else (internalPreparedLines,lineY + blankLineHeight)
										}
										else (internalPreparedLines,lineY + blankLineHeight)
									}
									val inter = renderLine(List.empty[PreparedTextLine],y)
									inter
								}
							}
							drawLines(nextPreparedLines ::: preparedLines,lines.drop(1),nextY)
						}
						else drawLines(preparedLines,lines.drop(1),y + blankLineHeight)
					}
					else
						preparedLines
				}
				drawLines(List.empty[PreparedTextLine],metlText.text.split("\n").toList,metlText.y.toFloat)
			}
			case _ => List.empty[PreparedTextLine]
		}
	})

	private def renderText(lines:List[PreparedTextLine],g:Graphics2D) = Stopwatch.time("SlideRenderer.renderText", () => {
		try {
			lines.foreach(line => {
				g.setPaint(toAwtColor(line.color))
				line.layout.draw(g,line.x,line.y)
			})
		} catch {
			case e:Throwable => {
				e.printStackTrace
				println("failed to render text: %s with exception %s".format(lines, e.getMessage))
			}
		}
	})

	private val ratioConst = 0.75
	
	private def filterAccordingToTarget[T](target:String,mccl:List[T]):List[T] = mccl.filter(mcc => {
		mcc match {
			case m:MeTLCanvasContent => m.target.trim.toLowerCase == target.trim.toLowerCase
			case _ => false
		}	
	}).toList
	def renderMultiple(h:History,requestedSizes:List[Tuple3[String,Int,Int]],target:String= "presentationSpace"):Map[String,Tuple3[Int,Int,Array[Byte]]] = Stopwatch.time("SlideRenderer.renderMultiple", () => {
		h.shouldRender match {
			case true => {
				val (texts,highlighters,inks,images) = h.getRenderableGrouped	
				val dimensions = measureItems(h,texts,highlighters,inks,images,target)
				Map(requestedSizes.map(rs => {
					val name = rs._1
					val width = rs._2
					val height = rs._3
					(name,(width,height,renderImage(h,dimensions,width,height,target)))
				}):_*)
			}
			case false => {
				Map(requestedSizes.map(rs => {
					val name = rs._1
					val width = rs._2
					val height = rs._3
					(name,(width,height,imageToByteArray(makeBlankImage(width,height))))
				}):_*)
			}
		}
	})
	def measureHistory(h:History, target:String = "presentationSpace"):Dimensions = Stopwatch.time("SlideRenderer.measureHistory", () => {
		h.shouldRender match {
			case true => {
				val (texts,highlighters,inks,images) = h.getRenderableGrouped	
				measureItems(h,texts,highlighters,inks,images)
			}
			case false => Dimensions(0.0,0.0,0.0,0.0,0.0,0.0)
		}
	})
	def measureItems(h:History,texts:List[MeTLText],highlighters:List[MeTLInk],inks:List[MeTLInk],images:List[MeTLImage], target:String = "presentationSpace"):Dimensions = Stopwatch.time("SlideRenderer.measureItems", () => {
		val nativeScaleTextBoxes = filterAccordingToTarget[MeTLText](target,texts).map(t => measureText(t))
		val td = nativeScaleTextBoxes.foldLeft(Dimensions(h.getLeft,h.getTop,h.getRight,h.getBottom,0.0,0.0))((acc,item) => {
			val newLeft = Math.min(acc.left,item.left)
			val newTop = Math.min(acc.top,item.top)
			val newRight = Math.max(acc.right,item.right)
			val newBottom = Math.max(acc.bottom,item.bottom)
			Dimensions(newLeft,newTop,newRight,newBottom,0.0,0.0)
		})
		Dimensions(td.left,td.top,td.right,td.bottom,td.right - td.left,td.bottom - td.top)
	})
	def renderImage(h:History,historyDimensions:Dimensions,width:Int,height:Int,target:String):Array[Byte] = Stopwatch.time("SlideRenderer.renderImage", () => {
		val contentWidth = historyDimensions.width
		val contentHeight = historyDimensions.height
		val contentXOffset = historyDimensions.left * -1
		val contentYOffset = historyDimensions.top * -1
		val historyRatio = tryo(contentHeight/contentWidth).openOr(ratioConst)

		val (renderWidth,renderHeight,scaleFactor) = (historyRatio >= ratioConst) match {
			case true => {
				val initialWidth = Math.max(1.0,width)
				var initialHeight = initialWidth*historyRatio
				val (renderWidth,renderHeight) = 
					(initialHeight > height) match {
						case true => (initialWidth*(height/initialHeight),height)
						case false => (initialWidth,initialHeight)
					}
				(renderWidth,renderHeight,renderWidth/contentWidth)
			}
			case false => {
				val initialHeight = Math.max(1.0,height)
				var initialWidth = initialHeight/historyRatio
				val (renderWidth,renderHeight) = 
						(initialWidth > width) match {
						case true => (width,initialHeight*(width/initialWidth))
						case false => (initialWidth,initialHeight)
					}
				(renderWidth,renderHeight,renderHeight/contentHeight)
			}
		}
		val unscaledImage = new BufferedImage(width,height,BufferedImage.TYPE_3BYTE_BGR)
		val g = unscaledImage.createGraphics.asInstanceOf[Graphics2D]
		g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
		g.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
		g.setPaint(AWTColor.white)
		g.fill(new Rectangle(0,0,width,height))
		val scaleApplier = scaleFactor
		val scaledHistory = (scaleFactor != h.xScale || scaleFactor != h.yScale || h.xOffset != 0 || h.yOffset != 0) match {
			case true => {
				println("scaling history to: %s (+%s,%s)".format(scaleFactor,h.xOffset,h.yOffset))
				h.adjustToVisual(contentXOffset,contentYOffset,scaleApplier,scaleApplier)
			}
			case false => h
		}
		val (scaledTexts,scaledHighlighters,scaledInks,scaledImages) = scaledHistory.getRenderableGrouped	
		filterAccordingToTarget[MeTLImage](target,scaledImages).foreach(img => renderImage(img,g))
		filterAccordingToTarget[MeTLInk](target,scaledHighlighters).foreach(renderInk(_,g))
		filterAccordingToTarget[MeTLText](target,scaledTexts).foreach(t => renderText(measureTextLines(t,g),g))
		filterAccordingToTarget[MeTLInk](target,scaledInks).foreach(renderInk(_,g))
		imageToByteArray(unscaledImage)
	})
	def render(h:History,intWidth:Int,intHeight:Int,target:String = "presentationSpace"):Array[Byte] = Stopwatch.time("SlideRenderer.render", () => {
		renderMultiple(h,List(("single",intWidth,intHeight)),target)("single")._3
/*
		val width = intWidth.toDouble
		val height = intHeight.toDouble
		val (texts,highlighters,inks,images) = h.getRenderableGrouped	
		h.shouldRender match {
			case true => {
				val nativeScaleTextBoxes = filterAccordingToTarget[MeTLText](target,texts).map(t => measureText(t))
				val td = nativeScaleTextBoxes.foldLeft(Dimensions(h.getLeft,h.getTop,h.getRight,h.getBottom,0.0,0.0))((acc,item) => {
					val newLeft = Math.min(acc.left,item.left)
					val newTop = Math.min(acc.top,item.top)
					val newRight = Math.max(acc.right,item.right)
					val newBottom = Math.max(acc.bottom,item.bottom)
					Dimensions(newLeft,newTop,newRight,newBottom,0.0,0.0)
				})
				val dimensions = Dimensions(td.left,td.top,td.right,td.bottom,td.right - td.left,td.bottom - td.top)
				val contentWidth = dimensions.width
				val contentHeight = dimensions.height
				val contentXOffset = dimensions.left * -1
				val contentYOffset = dimensions.top * -1
				val historyRatio = tryo(contentHeight/contentWidth).openOr(ratioConst)

				val (renderWidth,renderHeight,scaleFactor) = (historyRatio >= ratioConst) match {
					case true => {
						val initialWidth = Math.max(1.0,width)
						var initialHeight = initialWidth*historyRatio
						val (renderWidth,renderHeight) = 
							(initialHeight > height) match {
								case true => (initialWidth*(height/initialHeight),height)
								case false => (initialWidth,initialHeight)
							}
						(renderWidth,renderHeight,renderWidth/contentWidth)
					}
					case false => {
						val initialHeight = Math.max(1.0,height)
						var initialWidth = initialHeight/historyRatio
						val (renderWidth,renderHeight) = 
								(initialWidth > width) match {
								case true => (width,initialHeight*(width/initialWidth))
								case false => (initialWidth,initialHeight)
							}
						(renderWidth,renderHeight,renderHeight/contentHeight)
					}
				}
				val unscaledImage = new BufferedImage(width.toInt,height.toInt,BufferedImage.TYPE_3BYTE_BGR)
				val g = unscaledImage.createGraphics.asInstanceOf[Graphics2D]
				g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
				g.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
				g.setPaint(AWTColor.white)
				g.fill(new Rectangle(0,0,renderWidth.toInt,renderHeight.toInt))//dimensions.width.toInt,dimensions.height.toInt))
				val scaleApplier = scaleFactor
				val scaledHistory = (scaleFactor != h.xScale || scaleFactor != h.yScale || h.xOffset != 0 || h.yOffset != 0) match {
					case true => {
						println("scaling history to: %s (+%s,%s)".format(scaleFactor,h.xOffset,h.yOffset))
						h.adjustToVisual(contentXOffset,contentYOffset,scaleApplier,scaleApplier)
					}
					case false => h
				}
				val (scaledTexts,scaledHighlighters,scaledInks,scaledImages) = scaledHistory.getRenderableGrouped	
				filterAccordingToTarget[MeTLImage](target,scaledImages).foreach(img => renderImage(img,g))
				filterAccordingToTarget[MeTLInk](target,scaledHighlighters).foreach(renderInk(_,g))
				filterAccordingToTarget[MeTLText](target,scaledTexts).foreach(t => renderText(measureTextLines(t,g),g))
				filterAccordingToTarget[MeTLInk](target,scaledInks).foreach(renderInk(_,g))
				imageToByteArray(unscaledImage)
			}
			case false => imageToByteArray(makeBlankImage(width.toInt,height.toInt))
		}
*/
	})
}
