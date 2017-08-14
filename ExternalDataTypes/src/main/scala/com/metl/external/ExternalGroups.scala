package com.metl.external

import net.liftweb.common.Logger

import scala.xml.NodeSeq

abstract class GroupsProviderConfigurator {
  def configureFromXml(in:NodeSeq):Either[Exception,List[GroupsProvider]]
}

abstract class GroupsProvider(val storeId:String,val name:String) extends Logger {
  val canQuery:Boolean = false
  val canRestrictConversations:Boolean = true

  def getGroupsFor(userData:LiftAuthStateData):List[OrgUnit] = userData.eligibleGroups.toList
  def getMembersFor(orgUnit:OrgUnit):List[Member] = orgUnit.members
  def getGroupSetsFor(orgUnit:OrgUnit,members:List[Member] = Nil):List[GroupSet] = orgUnit.groupSets
  def getMembersFor(orgUnit:OrgUnit,groupSet:GroupSet):List[Member] = groupSet.members
  def getGroupsFor(orgUnit:OrgUnit,groupSet:GroupSet,members:List[Member] = Nil):List[Group] = groupSet.groups
  def getMembersFor(orgUnit:OrgUnit,groupSet:GroupSet,group:Group):List[Member] = group.members

  def getAllOrgUnits:List[OrgUnit]
  def getOrgUnit(name:String):Option[OrgUnit]
  def getPersonalDetailsFor(userData:LiftAuthStateData):List[Detail] = userData.informationGroups.toList
}

object PersonalInformation {
  val personalInformation = "personalInformation"
  val email = "email"
  val firstName = "firstName"
  val surname = "surname"
  val displayName = "displayName"
}

object GroupKeys {
  val ou = "ou"
  val course = "course"
  val special = "special"
  val section = "section"
  val sectionCategory = "sectionCategory"
  val groupCategory = "groupCategory"
  val group = "group"
}

