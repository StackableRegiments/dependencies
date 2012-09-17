package com.metl.model
// TODO: Change namespace to MSOfficeImport specific

import net.liftweb.common._
import scala.xml._
import scala.collection.JavaConverters._
import net.liftweb.util._
import Helpers._

import javax.imageio.ImageIO
import java.io._
import org.apache.poi.xslf.usermodel._

import java.awt.{Dimension, RenderingHints}
import java.awt.image.BufferedImage
import java.awt.geom._

/*
object ExportFidelity extends Enumeration {
    val ImageScale1X, ImageScale2X, Shapes = Value

    def fromString(requestedFidelity: String) = requestedFidelity.toLowerCase match {
      case "image_scale1x" => Some(ImageScale1x)
      case "image_scale2x" => Some(ImageScale2x)
      case "shapes" => Some(Shapes)
      case _ => None
    }
}
*/

// OCDriver will handle the file depending on it's type. Office2003 or Office2007 format
// OCConverter will export the file depending on the converter requested via export fidelity
/*
trait OCType {
  def driver: OCDriver
  def converter: OCConverter
}
*/

object OfficeConverter {
    def apply(fileName: String/*, exportFidelity: ExportFidelity*/) = {
      val converter = new OfficeConverter
      converter.fileName = fileName
      converter.convert(fileName)
    }
}

class OfficeConverter {
    private var fileName = ""

	val b64 = new sun.misc.BASE64Encoder
	def convert(file:String):Node = Stopwatch.time("OfficeConverter.convert",() => {
		file.split('.').reverse.take(1)(0) match {
			case "ppt" => convertDoc(file)
			case "pptx" => convertDocX(file)
		}
	})
    def convertAndDump(file:String):Node = Stopwatch.time("OfficeConverter.convertAndDump",() => {
        val xml = convert(file)
        try {
            val xmlOutput = new FileOutputStream(file.split('.')(0) + ".xml")
            val printer = new PrintStream(xmlOutput)
            printer.print(xml.toString)
            printer.close
        }
        xml
    })
    def convertImgX(file:String, scale: Int) {
        val is = new FileInputStream(file)
		val ppt = new XMLSlideShow(is)
        is.close

		val pageSize = ppt.getPageSize
		val pageWidth = (pageSize.width * scale).toInt
		val pageHeight = (pageSize.height * scale).toInt

		val slides = ppt.getSlides.toList

        slides.view.zipWithIndex foreach {
            case (slide, index) => {
              
              val img = new BufferedImage(pageWidth, pageHeight, BufferedImage.TYPE_INT_RGB)
              val graphics = img.createGraphics
              graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
              graphics.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
              graphics.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC)
              graphics.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON)

              graphics.setPaint(java.awt.Color.white)
              graphics.fill(new Rectangle2D.Float(0, 0, pageWidth, pageHeight))

              graphics.scale((pageWidth / pageSize.width).toDouble, (pageHeight / pageSize.height).toDouble)

              slide.draw(graphics)

              val fname = file.replaceAll("\\.pptx", "-" + (index + 1) + ".png")
              val out = new FileOutputStream(fname)
              ImageIO.write(img, "png", out)
              out.close
            }
        }
    }
    def convertImg(file:String, scale: Int) {
        val is = new FileInputStream(file)
        val ppt = new org.apache.poi.hslf.usermodel.SlideShow(is)
        is.close

        val pageSize = ppt.getPageSize
        val pageWidth = (pageSize.width * scale).toInt
        val pageHeight = (pageSize.height * scale).toInt

        val slides = ppt.getSlides.toList
        
        slides.view.zipWithIndex foreach {
            case (slide, index) => {
              
              val img = new BufferedImage(pageWidth, pageHeight, BufferedImage.TYPE_INT_RGB)
              val graphics = img.createGraphics
              graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
              graphics.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
              graphics.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC)
              graphics.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON)

              graphics.setPaint(java.awt.Color.white)
              graphics.fill(new Rectangle2D.Float(0, 0, pageWidth, pageHeight))

              graphics.scale((pageWidth / pageSize.width).toDouble, (pageHeight / pageSize.height).toDouble)

              slide.draw(graphics)

              val fname = file.replaceAll("\\.ppt", "-" + (index + 1) + ".png")
              val out = new FileOutputStream(fname)
              ImageIO.write(img, "png", out)
              out.close
            }
        }
    }

	def convertDoc(file:String):Node = convertDoc(new FileInputStream(file))
	def convertDoc(inputStream:InputStream):Node = Stopwatch.time("OfficeConverter.convertDoc",() => {
		val ppt = new org.apache.poi.hslf.usermodel.SlideShow(inputStream)
		val pageSize = ppt.getPageSize
		val slideWidth = pageSize.width
		val slideHeight = pageSize.height
		val slides = ppt.getSlides.toList
        val notes = ppt.getNotes.toList
		<presentation>
			<powerpointVersion>2003</powerpointVersion>
			<height>{slideHeight}</height>
			<width>{slideWidth}</width>
			<slides>{slides.map(s => docXItemToXml(s))}</slides>
            {notes.map(n => docXItemToXml(n))}
		</presentation>
	})
	def convertDocX(file:String):Node = convertDocX(new FileInputStream(file))
	def convertDocX(inputStream:InputStream):Node = Stopwatch.time("OfficeConverter.convertDocX",() => {
		val ppt = new XMLSlideShow(inputStream)
		val pageSize = ppt.getPageSize
		val slideWidth = pageSize.width
		val slideHeight = pageSize.height
		val slides = ppt.getSlides.toList
		<presentation>
			<powerpointVersion>2007</powerpointVersion>
			<height>{slideHeight}</height>
			<width>{slideWidth}</width>
			<slides>{slides.map(s => docXItemToXml(s))}</slides>
		</presentation>
	})
	private def getNameFromEnum[A <: {def name():String}](enum:Object):String = {
		if (enum != null){
			enum.asInstanceOf[A].name
		} else {
			""
		}
	}
	private def docXItemToXml(input:Object):Node = {
		input match {
			case slide:org.apache.poi.hslf.model.Slide => {
              <slide>{slide.getShapes.toList.map(shape => docXItemToXml(shape))}</slide>
            }
			case slide:XSLFSlide => {
              <slide>
                {tryo(slide.getShapes.map(s => docXItemToXml(s))).openOr(Text(""))}
                {tryo(docXItemToXml(slide.getNotes)).openOr(Text(""))}
              </slide>
            }
			case rect:java.awt.geom.Rectangle2D.Double => {
				<rect>
					<x>{rect.x}</x>
					<y>{rect.y}</y>
					<height>{rect.height}</height>
					<width>{rect.width}</width>
				</rect>
			}
			case rect:java.awt.geom.Rectangle2D.Float => {
				<rect>
					<x>{rect.x}</x>
					<y>{rect.y}</y>
					<height>{rect.height}</height>
					<width>{rect.width}</width>
				</rect>
			}
			case rect:java.awt.Rectangle => {
				<rect>
					<x>{rect.x}</x>
					<y>{rect.y}</y>
					<height>{rect.height}</height>
					<width>{rect.width}</width>
				</rect>
			}
			case color:java.awt.Color => {
				val transparency = color.getTransparency match {
					case java.awt.Transparency.BITMASK => "bitmask"
					case java.awt.Transparency.OPAQUE => "opaque"
					case java.awt.Transparency.TRANSLUCENT => "translucent"
					case other => "unknown: "+other
				}
				<color>
					<r>{color.getRed}</r>
					<g>{color.getGreen}</g>
					<b>{color.getBlue}</b>
					<a>{color.getAlpha}</a>
					<transparency>{transparency}</transparency>
				</color>	
			}
			case shape:org.apache.poi.hslf.model.SimpleShape => {
				val internal = shape match {
					case picture:org.apache.poi.hslf.model.Picture => <picture>{docXItemToXml(picture.getPictureData)}</picture>
					case text:org.apache.poi.hslf.model.TextShape => <text>{tryo(text.getTextRun.getRichTextRuns.toList.map(rtr => docXItemToXml(rtr))).openOr(Text(""))}</text>
					case line:org.apache.poi.hslf.model.Line => <line>Not implemented</line>
					case other => <error>{"Not implemented: "+other}</error>
				}
				val shadowProp = "not implemented"
				<shape>
					<id>{shape.getShapeId}</id>
					<fillColor>{docXItemToXml(shape.getFillColor)}</fillColor>
					<anchor>{docXItemToXml(shape.getAnchor)}</anchor>
					<flipHorizontal>{shape.getFlipHorizontal}</flipHorizontal>
					<flipVertical>{shape.getFlipVertical}</flipVertical>
					<rotation>{shape.getRotation}</rotation>
					<shapeShadow>{shadowProp}</shapeShadow>
					<content>{internal}</content>
				</shape>
			}
			case shape:XSLFSimpleShape => {
				val internal = shape match {
					case picture:XSLFPictureShape => <pictureShape>{docXItemToXml(picture.getPictureData)}</pictureShape>
					case textbox:XSLFTextShape => {
						<textShape>
							<textParagraphs>{textbox.getTextParagraphs.toArray.toList.map(t => docXItemToXml(t))}</textParagraphs>
						</textShape>
					}
					case background:XSLFBackground => {
						<background>
							<fillColor>{docXItemToXml(background.getFillColor)}</fillColor>
						</background>
					}
					case connector:XSLFConnectorShape => {
						<connectorShape>
							<error>Not implemented</error>
						</connectorShape>
					}
					case shadow:XSLFShadow => {
						<shadow>
							<fillColor>{shadow.getFillColor}</fillColor>
							<angle>{shadow.getAngle}</angle>
							<blur>{shadow.getBlur}</blur>
							<distance>{shadow.getDistance}</distance>
						</shadow>
					}
					case other => <error>Not implemented: {other}</error>
				}
				// not implemented
				//val shadowProp = docXItemToXml(shape.getShadow)
				val shadowProp = "not implemented"
				<shape>
					<id>{shape.getShapeId}</id>
					<fillColor>{docXItemToXml(shape.getFillColor)}</fillColor>
					<anchor>{docXItemToXml(shape.getAnchor)}</anchor>
					<flipHorizontal>{shape.getFlipHorizontal}</flipHorizontal>
					<flipVertical>{shape.getFlipVertical}</flipVertical>
					<rotation>{shape.getRotation}</rotation>
					<lineCap>{getNameFromEnum[LineCap](shape.getLineCap)}</lineCap>
					<lineColor>{docXItemToXml(shape.getLineColor)}</lineColor>
					<lineDash>{getNameFromEnum[LineDash](shape.getLineDash)}</lineDash>
					<lineHeadDecoration>{getNameFromEnum[LineDecoration](shape.getLineHeadDecoration)}</lineHeadDecoration>
					<lineHeadLength>{getNameFromEnum[LineEndLength](shape.getLineHeadLength)}</lineHeadLength>
					<lineHeadWidth>{getNameFromEnum[LineEndWidth](shape.getLineHeadWidth)}</lineHeadWidth>
					<lineTailDecoration>{getNameFromEnum[LineDecoration](shape.getLineHeadDecoration)}</lineTailDecoration>
					<lineTailLength>{getNameFromEnum[LineEndLength](shape.getLineTailLength)}</lineTailLength>
					<lineTailWidth>{getNameFromEnum[LineEndWidth](shape.getLineTailWidth)}</lineTailWidth>
					<lineWidth>{shape.getLineWidth}</lineWidth>
					<shapeShadow>{shadowProp}</shapeShadow>
					<content>{internal}</content>
				</shape>
			}
			case pictureData:org.apache.poi.hslf.usermodel.PictureData => {
				val pictureType = pictureData.getType match {
					case org.apache.poi.hslf.model.Picture.DIB => "dib"
					case org.apache.poi.hslf.model.Picture.EMF => "emf"
					case org.apache.poi.hslf.model.Picture.JPEG => "jpeg"
					case org.apache.poi.hslf.model.Picture.PICT => "pict"
					case org.apache.poi.hslf.model.Picture.PNG => "png"
					case org.apache.poi.hslf.model.Picture.WMF => "wmf"
					case other => "unknown: "+other	
				}
				<pictureData>
					<type>{pictureType}</type>
					<bytes>{b64.encode(pictureData.getData).replace("\n","").replace("\r","").replace(" ","")}</bytes>
				</pictureData>
			}
			case pictureData:XSLFPictureData => {
				val pictureType = pictureData.getPictureType match {
					case XSLFPictureData.PICTURE_TYPE_BMP => "bmp"
					case XSLFPictureData.PICTURE_TYPE_DIB => "dib"
					case XSLFPictureData.PICTURE_TYPE_EMF => "emf"
					case XSLFPictureData.PICTURE_TYPE_EPS => "eps"
					case XSLFPictureData.PICTURE_TYPE_GIF => "gif"
					case XSLFPictureData.PICTURE_TYPE_JPEG => "jpeg"
					case XSLFPictureData.PICTURE_TYPE_PICT => "pict"
					case XSLFPictureData.PICTURE_TYPE_PNG => "png"
					case XSLFPictureData.PICTURE_TYPE_TIFF => "tiff"
					case XSLFPictureData.PICTURE_TYPE_WMF => "wmf"
					case XSLFPictureData.PICTURE_TYPE_WPG => "wpg"
					case other => "unknown: "+other	
				}
				<pictureData>
					<filename>{pictureData.getFileName}</filename>
					<type>{pictureType}</type>
					<bytes>{b64.encode(pictureData.getData).replace("\n","").replace("\r","").replace(" ","")}</bytes>
				</pictureData>
			}
			case paragraph:org.apache.poi.hslf.model.TextRun => {
				<paragraph>
					<textRuns>{tryo(paragraph.getRichTextRuns.toList.map(rtr => docXItemToXml(rtr))).openOr(Text(""))}</textRuns>
				</paragraph>	
			}
			case paragraph:XSLFTextParagraph => {
				val bullet = paragraph.isBullet match {
					case true => {
						<bullet>
							<font>{paragraph.getBulletFont}</font>
							<char>{paragraph.getBulletCharacter}</char>
							<color>{docXItemToXml(paragraph.getBulletFontColor)}</color>
							<size>{paragraph.getBulletFontSize}</size>
						</bullet>
					}
					case false => <bullet />
				}
				<paragraph>
					<spaceBefore>{paragraph.getSpaceBefore}</spaceBefore>
					<spaceAfter>{paragraph.getSpaceAfter}</spaceAfter>
					<indent>{paragraph.getIndent}</indent>
					<leftMargin>{paragraph.getLeftMargin}</leftMargin>
					<level>{paragraph.getLevel}</level>
					<textAlign>{getNameFromEnum[TextAlign](paragraph.getTextAlign)}</textAlign>
					{bullet}
					<textRuns>{tryo(paragraph.getTextRuns.toArray.toList.map(t => docXItemToXml(t))).openOr(Text(""))}</textRuns>
				</paragraph>
			}
			case richTextRun:org.apache.poi.hslf.usermodel.RichTextRun => {
				val bullet = richTextRun.isBullet match {
					case true => {
						<bullet>
							<font>{richTextRun.getBulletFont}</font>
							<char>{richTextRun.getBulletChar}</char>
							<color>{docXItemToXml(richTextRun.getBulletColor)}</color>
							<size>{richTextRun.getBulletSize}</size>
						</bullet>
					}
					case false => <bullet />
				}
				<textRun>
					<spaceBefore>{richTextRun.getSpaceBefore}</spaceBefore>
					<spaceAfter>{richTextRun.getSpaceAfter}</spaceAfter>
					<indent>{richTextRun.getIndentLevel}</indent>
					<textOffset>{richTextRun.getTextOffset}</textOffset>
					<startIndex>{richTextRun.getStartIndex}</startIndex>
					<endIndex>{richTextRun.getEndIndex}</endIndex>
					<textAlign>{richTextRun.getAlignment}</textAlign>
					{bullet}
					<fontFamily>{richTextRun.getFontName}</fontFamily>
					<fontSize>{richTextRun.getFontSize}</fontSize>
					<bold>{richTextRun.isBold}</bold>
					<italic>{richTextRun.isItalic}</italic>
					<strikethrough>{richTextRun.isStrikethrough}</strikethrough>
					<superscriptValue>{richTextRun.getSuperscript}</superscriptValue>
					<underline>{richTextRun.isUnderlined}</underline>
					<text>{richTextRun.getText}</text>
				</textRun>	
			}
			case textRun:XSLFTextRun => {
				val hyperlink = textRun.getHyperlink match {
					case h:XSLFHyperlink => <hyperlink>{textRun.getHyperlink.getTargetURI.toString}</hyperlink>
					case _ => <hyperlink />
				}
				<textRun>
					<characterSpacing>{textRun.getCharacterSpacing}</characterSpacing>
					<fontFamily>{textRun.getFontFamily}</fontFamily>
					<fontSize>{textRun.getFontSize}</fontSize>
					<bold>{textRun.isBold}</bold>
					<italic>{textRun.isItalic}</italic>
					<strikethrough>{textRun.isStrikethrough}</strikethrough>
					<subscript>{textRun.isSubscript}</subscript>
					<superscript>{textRun.isSuperscript}</superscript>
					<underline>{textRun.isUnderline}</underline>
					<textCap>{getNameFromEnum[TextCap](textRun.getTextCap)}</textCap>
					<text>{textRun.getText}</text>
					{hyperlink}
				</textRun>
			}
			case frame:XSLFGraphicFrame => {
				val anchor = <anchor>{docXItemToXml(frame.getAnchor)}</anchor>
				val id = frame.getShapeId
				// not implemented
				//val shapeType = getNameFromEnum[XSLFShapeType](frame.getShapeType)
				val shapeType = frame.getShapeName
				<graphicFrame>
					<id>{id}</id>
					{anchor}
					<flipHorizontal>{frame.getFlipHorizontal}</flipHorizontal>
					<flipVertical>{frame.getFlipVertical}</flipVertical>
					<shapeType>{shapeType}</shapeType>
					<rotation>{frame.getRotation}</rotation>
				</graphicFrame>
			}
			case group:XSLFGroupShape => {
				<groupShape>
					<id>{group.getShapeId}</id>
					<anchor>{docXItemToXml(group.getAnchor)}</anchor>
					<interiorAnchor>{docXItemToXml(group.getInteriorAnchor)}</interiorAnchor>
					<flipHorizontal>{group.getFlipHorizontal}</flipHorizontal>
					<flipVertical>{group.getFlipVertical}</flipVertical>
					<rotation>{group.getRotation}</rotation>
					<content>{group.getShapes.toList.map(s => docXItemToXml(s))}</content>
				</groupShape>
			}
            case notes:org.apache.poi.hslf.model.Notes => {
                <notes>
                    <content>{notes.getTextRuns.toList.map(t => docXItemToXml(t))}</content>
                </notes>
            }
            case notes:XSLFNotes => {
                <notes>
                  <content>{extractText(notes.getCommonSlideData)}</content>
                </notes>
            } 
			case null => Text("")
			case other => <error>Not yet implemented: {other}</error>
		}
	}

    private def extractText(data: XSLFCommonSlideData): String = {

       val text = new StringBuffer
	   for (textBody <- data.getDrawingText.asScala) {
	      for (p <- textBody.getParagraphs) {
            text.append(p.getText)
            text.append("\n")
	      }
	   }
       text.toString.trim
	}
}
