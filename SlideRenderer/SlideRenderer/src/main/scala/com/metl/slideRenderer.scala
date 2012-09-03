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
	private def renderImage(metlImage:MeTLImage,g:Graphics2D):Unit = Stopwatch.time("SlideRenderer.renderImage", () => {
		metlImage match {
			case m:MeTLImage => {
				val image:Image = getImageFor(metlImage)
				val (finalHeight,finalWidth) = (metlImage.height,metlImage.width) match {
					case (h:Double,w:Double) if (h.isNaN || w.isNaN) => {
						val imageObserver = new Canvas(g.getDeviceConfiguration)
						val internalHeight = h match {
							case d:Double if (d.isNaN) => {
								val observedHeight = image.getHeight(imageObserver)
								//println("observed height: %s".format(observedHeight))
								observedHeight * metlImage.scaleFactor
							}
							case d:Double => d
							case _ => 0.0
						}
						val internalWidth = w match {
							case d:Double if (d.isNaN) => {
								val observedWidth = image.getWidth(imageObserver)
								//println("observed width: %s".format(observedWidth))
								observedWidth * metlImage.scaleFactor
							}
							case d:Double => d
							case _ => 0.0
						}
						(internalHeight,internalWidth)
					}
					case (h:Double,d:Double) => (h,d)
					case _ => (0.0,0.0)	
				}
				//println("rendered image: %s with height (%s) and width (%S)".format(m,finalHeight,finalWidth))
				image match {
					case i:Image if (finalHeight == 0.0 || finalWidth == 0.0) => {}
					case i:Image => g.drawImage(image,m.left.toInt,m.top.toInt,finalWidth.toInt,finalHeight.toInt,null)
					case _ => {}
				}
			}
			case _ => {}
		}
	})

	private def renderInk(metlInk:MeTLInk,g:Graphics2D) = Stopwatch.time("SlideRenderer.renderInk", () => {
		val HIGHLIGHTER_ALPHA  = 55
		val PRESSURE = 0.22
		val color = metlInk.isHighlighter match {
			case true => toAwtColor(metlInk.color, HIGHLIGHTER_ALPHA)
			case false => toAwtColor(metlInk.color)
		}
		g.setPaint(color)
		g.fill(new Stroke(metlInk.points,metlInk.thickness))
	})

	case class PreparedTextLine(text:String,layout:TextLayout,x:Float,y:Float,width:Float,height:Float,color:Color)

	private def measureText(metlText:MeTLText,g:Graphics2D):List[PreparedTextLine] = Stopwatch.time("SlideRenderer.measureText", () => {
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
		lines.foreach(line => {
			g.setPaint(toAwtColor(line.color))
			line.layout.draw(g,line.x,line.y)
		})
	})

	private val ratioConst = 0.75

	def render(h:History,intWidth:Int,intHeight:Int):Array[Byte] = Stopwatch.time("SlideRenderer.render", () => {
		val width = intWidth.toDouble
		val height = intHeight.toDouble

		h.shouldRender match {
			case true => {
				val tempImage = new BufferedImage(1,1,BufferedImage.TYPE_3BYTE_BGR)
				val tempG = tempImage.createGraphics.asInstanceOf[Graphics2D]
				val nativeScalePreparedTextLines = h.getTexts.map(t => measureText(t,tempG))

				val (right,bottom) = nativeScalePreparedTextLines.foldLeft((h.getRight,h.getBottom))((acc,item) => {
					item.foldLeft(acc)((internalAcc,internalItem) => {
						val newRight = Math.max(internalAcc._1,internalItem.x+internalItem.width)
						val newBottom = Math.max(internalAcc._2,internalItem.y+internalItem.height)
						(newRight,newBottom)
					})
				})

				val historyRatio = tryo(bottom/right).openOr(ratioConst)

				//println("---")
				//println("requested: W:%s,H:%s".format(width,height))
				//println("history ratio: %s (bottom:%s,right:%s)".format(historyRatio,bottom,right))	

				val (renderWidth,renderHeight,scaleFactor) = (historyRatio >= ratioConst) match {
					case true => {
						val initialWidth = Math.max(1.0,width)
						var initialHeight = initialWidth*historyRatio
						val (renderWidth,renderHeight) = 
							//(initialWidth*(height/initialHeight),height)
							(initialHeight > height) match {
								case true => (initialWidth*(height/initialHeight),height)
								case false => (initialWidth,initialHeight)
							}
						(renderWidth,renderHeight,renderWidth/right)
					}
					case false => {
						val initialHeight = Math.max(1.0,height)
						var initialWidth = initialHeight/historyRatio
						val (renderWidth,renderHeight) = 
							//(width,initialHeight*(width/initialWidth))
								(initialWidth > width) match {
								case true => (width,initialHeight*(width/initialWidth))
								case false => (initialWidth,initialHeight)
							}
						(renderWidth,renderHeight,renderHeight/bottom)
					}
				}
				//println("render: F:%s (RW:%s,RH:%s)".format(scaleFactor,renderWidth,renderHeight))

				val unscaledImage = new BufferedImage(width.toInt,height.toInt,BufferedImage.TYPE_3BYTE_BGR)
				val g = unscaledImage.createGraphics.asInstanceOf[Graphics2D]
				g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
				g.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
				g.setPaint(AWTColor.white)
				g.fill(new Rectangle(0,0,width.toInt,height.toInt))

				val scaledHistory = (scaleFactor != h.getScaleFactor) match {
					case true => h.scale(scaleFactor)
					case false => h
				}

				//g.transform(AffineTransform.getTranslateInstance((width-renderWidth)/2,(height-renderHeight)/2))

				scaledHistory.getImages.foreach(renderImage(_,g))
				scaledHistory.getHighlighters.foreach(renderInk(_,g))
				scaledHistory.getTexts.foreach(t => renderText(measureText(t,g),g))
				scaledHistory.getInks.foreach(renderInk(_,g))

				//println("---")
				imageToByteArray(unscaledImage)
			}

			case false => imageToByteArray(makeBlankImage(width.toInt,height.toInt))
		}
	})
}
