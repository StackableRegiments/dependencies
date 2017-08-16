package com.metl.external

import net.liftweb.common.Logger

abstract class ExternalGroupStoreProviderConfigurator extends ExternalConfigurator[GroupStoreProvider]

trait GroupStoreProvider extends Logger {
  val canQuery:Boolean = false
  def getData:GroupStoreData = GroupStoreData()
  def getGroups:Map[String,List[OrgUnit]] = getData.groupsForMembers
  def getMembers:Map[String,List[Member]] = getData.membersForGroups
  def getPersonalDetails:Map[String,List[Detail]] = getData.detailsForMembers

  def getAllOrgUnits:List[OrgUnit] = getData.orgUnitsByName.values.toList
  def getOrgUnit(name:String):Option[OrgUnit] = getData.orgUnitsByName.get(name)

  def getGroupSet(orgUnit:OrgUnit,name:String):Option[GroupSet] = getData.groupSetsByOrgUnit.get(orgUnit).getOrElse(Nil).find(_.name == name)

  def getGroup(orgUnit:OrgUnit,groupSet:GroupSet,name:String):Option[Group] = getData.groupsByGroupSet.get((orgUnit,groupSet)).getOrElse(Nil).find(_.name == name)

  def getMembersFor(orgUnit:OrgUnit):List[Member] = {
    val res = getOrgUnit(orgUnit.name).toList.flatMap(_.members)
    trace("getMembersFor(%s) => %s".format(orgUnit,res))
    res
  }
  def getGroupSetsFor(orgUnit:OrgUnit,members:List[Member] = Nil):List[GroupSet] = getData.groupSetsByOrgUnit.get(orgUnit).getOrElse(Nil)

  def getMembersFor(orgUnit:OrgUnit,groupSet:GroupSet):List[Member] = {
    val res = getGroupSet(orgUnit,groupSet.name).toList.flatMap(_.members)
    trace("getMembersFor(%s,%s) => %s".format(orgUnit,groupSet,res))
    res
  }
  def getGroupsFor(orgUnit:OrgUnit,groupSet:GroupSet,members:List[Member] = Nil):List[Group] = getData.groupsByGroupSet.get((orgUnit,groupSet)).getOrElse(Nil)

  def getMembersFor(orgUnit:OrgUnit,groupSet:GroupSet,group:Group):List[Member] = {
    val res = getGroup(orgUnit,groupSet,group.name).toList.flatMap(_.members)
    trace("getMembersFor(%s,%s,%s) => %s".format(orgUnit,groupSet,group,res))
    res
  }
}

case class GroupStoreData(
                           groupsForMembers:Map[String,List[OrgUnit]] = Map.empty[String,List[OrgUnit]],
                           membersForGroups:Map[String,List[Member]] = Map.empty[String,List[Member]],
                           detailsForMembers:Map[String,List[Detail]] = Map.empty[String,List[Detail]],
                           orgUnitsByName:Map[String,OrgUnit] = Map.empty[String,OrgUnit],
                           groupSetsByOrgUnit:Map[OrgUnit,List[GroupSet]] = Map.empty[OrgUnit,List[GroupSet]],
                           groupsByGroupSet:Map[Tuple2[OrgUnit,GroupSet],List[Group]] = Map.empty[Tuple2[OrgUnit,GroupSet],List[Group]]
                         )

