package com.metl.external

case class ExternalGrade(author:String,timestamp:Long,id:String,location:String,name:String,description:String,gradeType:ExternalGradeValueType.Value = ExternalGradeValueType.Numeric,visible:Boolean = false,foreignRelationship:Option[Tuple2[String,String]] = None,gradeReferenceUrl:Option[String] = None,numericMaximum:Option[Double] = Some(100.0),numericMinimum:Option[Double] = Some(0.0))

object ExternalGradeValueType extends Enumeration {
  type ExternalGradeValueType = Value
  val Numeric,Boolean,Text = Value
  def parse(input:String):ExternalGradeValueType = {
    input.toLowerCase.trim match {
      case "numeric" => ExternalGradeValueType.Numeric
      case "boolean" => ExternalGradeValueType.Boolean
      case "text" => ExternalGradeValueType.Text
      case _ => ExternalGradeValueType.Numeric
    }
  }
  def print(input:ExternalGradeValueType.Value):String = {
    input match {
      case ExternalGradeValueType.Numeric => "numeric"
      case ExternalGradeValueType.Boolean => "boolean"
      case ExternalGradeValueType.Text => "text"
      case _ => "numeric"
    }
  }
}

trait ExternalGradeValue {
  def getType:ExternalGradeValueType.Value
  def getNumericGrade:Option[Double] = None
  def getTextGrade:Option[String] = None
  def getBooleanGrade:Option[Boolean] = None
  def getComment:Option[String] = None
  def getPrivateComment:Option[String] = None
  def getGradedUser:String
  def getGradeId:String
}
object ExternalNumericGradeValue {
  def empty = ExternalNumericGradeValue("",0L,"","",0.0)
}
case class ExternalNumericGradeValue(author:String,timestamp:Long,gradeId:String,gradedUser:String,gradeValue:Double,gradeComment:Option[String] = None,gradePrivateComment:Option[String] = None) extends ExternalGradeValue {
  override def getType:ExternalGradeValueType.Value = ExternalGradeValueType.Numeric
  override def getNumericGrade:Option[Double] = Some(gradeValue)
  override def getGradeId:String = gradeId
  override def getGradedUser:String = gradedUser
  override def getComment:Option[String] = gradeComment
  override def getPrivateComment:Option[String] = gradePrivateComment
}

object ExternalBooleanGradeValue {
  def empty = ExternalBooleanGradeValue("",0L,"","",gradeValue = false)
}
case class ExternalBooleanGradeValue(author:String,timestamp:Long,gradeId:String,gradedUser:String,gradeValue:Boolean,gradeComment:Option[String] = None,gradePrivateComment:Option[String] = None) extends ExternalGradeValue {
  override def getType:ExternalGradeValueType.Value = ExternalGradeValueType.Boolean
  override def getBooleanGrade:Option[Boolean] = Some(gradeValue)
  override def getGradeId:String = gradeId
  override def getGradedUser:String = gradedUser
  override def getComment:Option[String] = gradeComment
  override def getPrivateComment:Option[String] = gradePrivateComment
}

object ExternalTextGradeValue {
  def empty = ExternalTextGradeValue("",0L,"","","")
}
case class ExternalTextGradeValue(author:String,timestamp:Long,gradeId:String,gradedUser:String,gradeValue:String,gradeComment:Option[String] = None,gradePrivateComment:Option[String] = None) extends ExternalGradeValue {
  override def getType:ExternalGradeValueType.Value = ExternalGradeValueType.Text
  override def getTextGrade:Option[String] = Some(gradeValue)
  override def getGradeId:String = gradeId
  override def getGradedUser:String = gradedUser
  override def getComment:Option[String] = gradeComment
  override def getPrivateComment:Option[String] = gradePrivateComment
}
