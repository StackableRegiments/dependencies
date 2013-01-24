package test

import org.apache.http.Header
import org.apache.commons.io.IOUtils

import org.scalatest._
import org.scalacheck._
import org.scalatest.matchers._

import com.metl.data._
import com.metl.utils._
import com.metl.metl2011._

case class MockHttpProvider(client:MockHttpClient) extends SimpleAuthedHttpProvider("user","password"){
  override def getClient = client
}
case class MockHttpClient(zipResponse:Array[Byte],var xmlResponse:String) extends CleanHttpClient(null){
  override def getHttpHeaders = List.empty[Header]
  override def setHttpHeaders(headers:List[Header]) = {}
  override def getCookies = Map.empty[String,Header]
  override def setCookies(cookies:Map[String,Header]) = {}
  override def postUnencodedForm(uri:String,postItemList:List[(String,String)], additionalHeaders:List[(String,String)]) = Array.empty[Byte]
  override def postForm(uri:String,itemList:List[(String,String)],additionalHeaders:List[(String,String)]) = Array.empty[Byte]
  override def postBytes(uri:String,bytes:Array[Byte],additionalHeaders:List[(String,String)]) = Array.empty[Byte]
  override def getAsBytes(uri:String,additionalHeaders:List[(String,String)]) = zipResponse
  override def getAsString(uri:String,additionalHeaders:List[(String,String)]) = xmlResponse
  override def get(uri:String,additionalHeaders:List[(String,String)]) = xmlResponse
  override def addAuthorization(domain:String,username:String,password:String) = {}
}
class StructuredMockHttpClient extends MockHttpClient(Array.empty[Byte],""){
  override def getAsBytes(uri:String) = DummyConversations.loadZip("snapshot/all.zip")
  override def getAsString(uri:String) = {
    val path = new java.net.URL(uri).getPath
    val localPath = "snapshot/%s".format(path.drop("/Structure/".length))
    DummyConversations.loadString(localPath)
  }
  override def get(uri:String) = {
    if(uri.contains(":1188/primarykey.yaws")) {
      println("getting a jid")
      "12345"
    }
    else getAsString(uri)
  }
}

class MockMessageBus extends MessageBus(null,null){
  override def sendStanzaToRoom(stanza:MeTLStanza):Boolean = true
  override def recieveStanzaFromRoom(stanza:MeTLStanza) =  {}
  override def notifyConnectionLost = {}
  override def notifyConnectionResumed = {}
  override def release = {}
}

case class MockMessageBusProvider(bus:MessageBus) extends MessageBusProvider{
  override def getMessageBus(definition:MessageBusDefinition) = bus
  override def sendMessageToBus(busFilter:MessageBusDefinition => Boolean, message:MeTLStanza) = {}
  override def releaseMessageBus(definition:MessageBusDefinition) = {}
}

object DummyConversations {
  val MALFORMED_STRING = "malformed<xml>/>"//ABSOLUTE HORROR FOR AN XML PARSER
  def loadZip(filename:String):Array[Byte] = {
    IOUtils.toByteArray(getClass.getClassLoader.getResourceAsStream(filename))
  }
  def loadString(filename:String):String = {
    try{
      IOUtils.toString(getClass.getClassLoader.getResourceAsStream(filename))
    }
    catch{
      case e:Exception => {
      println(e)
        MALFORMED_STRING
      }
    }
  }
  val singleZip = loadZip("single.zip")
  val singleDetails = loadString("00/10000/details.xml")
}

class InMemoryMeggleSuite extends FunSuite with BeforeAndAfter with ShouldMatchers {
  test("Universe is sane") {
    2 should equal(2)
  }
 
  test("Single conversation is parsed correctly") {
    val httpProvider = MockHttpProvider(MockHttpClient(DummyConversations.singleZip,DummyConversations.singleDetails))
    val messageBusProvider = MockMessageBusProvider(new MockMessageBus)

    val meggle = new MeTL2011CachedConversations("config",httpProvider,messageBusProvider,(c)=>{})
    val conversations = meggle.conversations
    1 should equal(conversations.size)
    val conversation = conversations(12345)
    "firstAuthor" should equal(conversation.author)
  }

  test("Queries run correctly against a single conversation database") {
    val httpProvider = MockHttpProvider(MockHttpClient(DummyConversations.singleZip,DummyConversations.singleDetails))
    val messageBusProvider = MockMessageBusProvider(new MockMessageBus)

    val meggle = new MeTL2011CachedConversations("config",httpProvider,messageBusProvider,(c)=>{})
    1 should equal(meggle.search("firstAuthor").size)
    0 should equal(meggle.search("secondAuthor").size)
    1 should equal(meggle.search("ati").size)
    1 should equal(meggle.search("aTitl").size)
    1 should equal(meggle.search("tle").size)
    0 should equal(meggle.search("lions").size)
    0 should equal(meggle.search("").size)
    0 should equal(meggle.search("' DROP TABLE USERS;").size)
    0 should equal(meggle.search(null).size)
  }
  test("System should correctly return multiple conversations if query finds multiple options") {
    val httpProvider = MockHttpProvider(new StructuredMockHttpClient)
    val messageBusProvider = MockMessageBusProvider(new MockMessageBus)
    val meggle = new MeTL2011CachedConversations("config",httpProvider,messageBusProvider,(c)=>{})
    163 should equal(meggle.search("hagand").size)

  }
    test("System should correctly handle all conversations snapshotted before the February 2013 shutdown date") {
    val httpProvider = MockHttpProvider(new StructuredMockHttpClient)
    val messageBusProvider = MockMessageBusProvider(new MockMessageBus)
    val meggle = new MeTL2011CachedConversations("config",httpProvider,messageBusProvider,(c)=>{})
    5695 should equal(meggle.conversations.size)//This is a frozen snapshot, and the number will not change
  }
  
  test("System should correctly update conversation list when a new conversation is received") {
    val httpProvider = MockHttpProvider(new StructuredMockHttpClient)
    val messageBusProvider = MockMessageBusProvider(new MockMessageBus)
    val meggle = new MeTL2011CachedConversations("config",httpProvider,messageBusProvider,(c)=>{})
    163 should equal(meggle.search("hagand").size)
    meggle.createConversation("a new conversation", "hagand")
    164 should equal(meggle.search("hagand").size)

  }
  test("system should update slide count on AddSlide") {
    val httpProvider = MockHttpProvider(new StructuredMockHttpClient)
    val messageBusProvider = MockMessageBusProvider(new MockMessageBus)
    val meggle = new MeTL2011CachedConversations("config",httpProvider,messageBusProvider,(c)=>{})
    val conversation = meggle.createConversation("a new conversation", "hagand")
    1 should equal(conversation.slides.size)
    meggle.addSlideAtIndexOfConversation(conversation.jid.toString, 0)
    2 should equal(meggle.detailsOf(conversation.jid).slides.size)
  }
  test("system should update on conversation delete"){
    val httpProvider = MockHttpProvider(new StructuredMockHttpClient)
    val messageBusProvider = MockMessageBusProvider(new MockMessageBus)
    val meggle = new MeTL2011CachedConversations("config",httpProvider,messageBusProvider,(c)=>{})
    val conversation = meggle.createConversation("a new conversation", "hagand")
    meggle.deleteConversation(conversation.jid.toString)
    "deleted" should equal(meggle.detailsOf(conversation.jid).subject)
  }
  test("system should update on a conversation rename") {
    val httpProvider = MockHttpProvider(new StructuredMockHttpClient)
    val messageBusProvider = MockMessageBusProvider(new MockMessageBus)
    val meggle = new MeTL2011CachedConversations("config",httpProvider,messageBusProvider,(c)=>{})
    val conversation = meggle.createConversation("a new conversation", "hagand")
    meggle.renameConversation(conversation.jid.toString, "renamed conversation")
    "renamed conversation" should equal(meggle.detailsOf(conversation.jid).title)
  }
  test("system should update on a change of permission") {
    val httpProvider = MockHttpProvider(new StructuredMockHttpClient)
    val messageBusProvider = MockMessageBusProvider(new MockMessageBus)
    val meggle = new MeTL2011CachedConversations("config",httpProvider,messageBusProvider,(c)=>{})
    val conversation = meggle.createConversation("a new conversation", "hagand")
    true should equal(conversation.permissions.studentsCanPublish)
    meggle.changePermissions(conversation.jid.toString, new Permissions(ServerConfiguration.empty, false, false, false))
    false should equal(meggle.detailsOf(conversation.jid).permissions.studentsCanPublish)
  }

  test("system should update on a change of subject"){
    val httpProvider = MockHttpProvider(new StructuredMockHttpClient)
    val messageBusProvider = MockMessageBusProvider(new MockMessageBus)
    val meggle = new MeTL2011CachedConversations("config",httpProvider,messageBusProvider,(c)=>{})
    val conversation = meggle.createConversation("a new conversation", "hagand")
    "unrestricted" should equal(conversation.subject)
    meggle.updateSubjectOfConversation(conversation.jid.toString, "hagand")
    "hagand" should equal(meggle.detailsOf(conversation.jid).subject)
  }
  test("system should update slide order on reorderSlidesOfConversation") {
    val httpProvider = MockHttpProvider(new StructuredMockHttpClient)
    val messageBusProvider = MockMessageBusProvider(new MockMessageBus)
    val meggle = new MeTL2011CachedConversations("config",httpProvider,messageBusProvider,(c)=>{})
    val conversation = meggle.createConversation("a new conversation", "hagand")
    meggle.addSlideAtIndexOfConversation(conversation.jid.toString, 1)
    val newConv = meggle.detailsOf(conversation.jid)
    12347 should equal(newConv.slides(0).id)
    12346 should equal(newConv.slides(1).id)
    meggle.reorderSlidesOfConversation(conversation.jid.toString, newConv.slides.reverse)
    12346 should equal(meggle.detailsOf(conversation.jid).slides(0).id)
    12347 should equal(meggle.detailsOf(conversation.jid).slides(1).id)
  }

}
