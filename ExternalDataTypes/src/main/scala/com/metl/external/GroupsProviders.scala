package com.metl.external

import com.metl.PeriodicallyRefreshingVar

import net.liftweb.util.Helpers._

abstract class PeriodicallyRefreshingGroupsProvider[T](override val storeId:String,override val name:String,refreshPeriod:TimeSpan) extends GroupsProvider(storeId,name) {
  protected val timespan = refreshPeriod
  protected var lastModified:Long = 0
  protected def startingValue:T
  protected var lastCache:T = startingValue
  protected var cache = new PeriodicallyRefreshingVar[Unit](timespan,() => {
    if (shouldCheck){
      val newCheck = new java.util.Date().getTime()
      lastCache = actuallyFetchGroups
      lastModified = newCheck
    }
  })
  protected def shouldCheck:Boolean
  protected def actuallyFetchGroups:T
  protected def parseStore(username:String,store:T):List[OrgUnit]
  override def getGroupsFor(userData:LiftAuthStateData):List[OrgUnit] = parseStore(userData.username,lastCache)
}

abstract class PeriodicallyRefreshingFileReadingGroupsProvider[T](override val storeId:String,override val name:String,path:String,refreshPeriod:TimeSpan) extends PeriodicallyRefreshingGroupsProvider[T](storeId,name,refreshPeriod) {
  override def shouldCheck = {
    val newCheck = new java.io.File(path).lastModified()
    newCheck > lastModified
  }
}

abstract class PerUserFlatFileGroupsProvider(override val storeId:String,override val name:String,path:String,refreshPeriod:TimeSpan) extends PeriodicallyRefreshingFileReadingGroupsProvider[Map[String,List[OrgUnit]]](storeId,name,path,refreshPeriod) {
  override def startingValue = Map.empty[String,List[OrgUnit]]
  override protected def parseStore(username:String,store:Map[String,List[OrgUnit]]):List[OrgUnit] = store.get(username).getOrElse(Nil)
}
