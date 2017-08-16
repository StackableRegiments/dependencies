package com.metl.external

import net.liftweb.common.Logger

abstract class ExternalGradebookConfigurator extends ExternalConfigurator[ExternalGradebook]

abstract class ExternalGradebook(val id:String,val name:String) extends TryE with Logger {
  def getGradeContexts(username:String):Either[Exception,List[OrgUnit]] = Left(notImplemented)
  def getGradeContextClasslist(username:String,orgUnitId:String):Either[Exception,List[Map[String,String]]] = Left(notImplemented)
  def getGradesFromContext(context:String,username:String):Either[Exception,List[ExternalGrade]] = Left(notImplemented)
  def getGradeInContext(context:String,username:String,gradeId:String):Either[Exception,ExternalGrade] = Left(notImplemented)
  def createGradeInContext(context:String,username:String,grade:ExternalGrade):Either[Exception,ExternalGrade] = Left(notImplemented)
  def updateGradeInContext(context:String,username:String,grade:ExternalGrade):Either[Exception,ExternalGrade] = Left(notImplemented)
  def getGradeValuesForGrade(context:String,username:String,gradeId:String):Either[Exception,List[ExternalGradeValue]] = Left(notImplemented)
  def updateGradeValuesForGrade(context:String,username:String,gradeId:String,grades:List[ExternalGradeValue]):Either[Exception,List[ExternalGradeValue]] = Left(notImplemented)
}

trait TryE {
  val notImplemented = new Exception("not yet implemented")
  def trye[A](in: => A):Either[Exception,A] = {
    try {
      Right(in)
    } catch {
      case e:Exception => Left(e)
    }
  }
}