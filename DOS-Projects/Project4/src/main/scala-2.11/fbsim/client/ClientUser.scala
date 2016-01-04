package fbsim.client

import java.security.{PrivateKey, PublicKey}
import java.util.concurrent.TimeUnit

import akka.actor.Actor
import akka.util.Timeout
import fbsim.client.ClientManager.RequestComplete
import fbsim.client.ClientUser._
import fbsim.common._
import spray.httpx.SprayJsonSupport
import spray.json.{JsonParser, JsonReader}

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Random

object ClientUser{
  case class CreateRandomFriendList(avgFriends:Int)
  case class CreatePost()
  case class Register()
  case class AddFriend()
  case class InitUserFriends()
  case class GetProfile()
  case class GetPosts()
  case class CreateAPage()
}


class ClientUser() extends Actor with SprayJsonSupport{

  import DefaultJsonFormatterProtocol._
  import spray.client.pipelining._
  import spray.http._

  var userId = 0
  val waitTime = 50
  val urlPrefix = "http://localhost:8080"
  val userPrefix = urlPrefix + "/user"
  val pagePrefix = urlPrefix + "/page"
  val postsSuffix = "/posts"
  val friendsSuffix = "/friends"
  val idsOfFriends = new mutable.ListBuffer[Int]

  var prvtKey:PrivateKey = null
  var pubKey:PublicKey = null

  implicit val ec = context.dispatcher
  implicit val timeout = Timeout(120, TimeUnit.SECONDS)

  val manger = context.actorSelection("/user/")

  private def fbGraphApiUser(prefix:String, nodeId:Int, suffix:Option[String]): String = {
    val suffixVal = suffix match {
      case Some(suffixStr) => suffixStr
      case None => ""
    }
    prefix + "/" + nodeId + suffixVal.trim
  }

  private def getRandomFriendIds: List[Int]={
    val randomFriends = new mutable.HashSet[Int]
    val numFriends = Random.nextInt(idsOfFriends.size-1) + 1
    do {
      var friendId = 0
      do{
        friendId = idsOfFriends(Random.nextInt(idsOfFriends.size))
      }while(randomFriends.contains(friendId))
      randomFriends += friendId
    }while (randomFriends.size < numFriends)
    randomFriends += userId
    randomFriends.toList
  }


  private def addLoginInfoNSendRequest(httpReq: HttpRequest) : Future[HttpResponse] ={
    val signingContent = userId + httpReq.uri.toString + httpReq.entity.asString
    val sig = SecurityApis.sign(signingContent, prvtKey)
    val pipeline: HttpRequest => Future[HttpResponse] = (addHeader("Login", userId.toString)
      ~>addHeader("Signature", sig)
      ~> sendReceive)
    pipeline(httpReq)
  }


  def pageOrUser:Boolean = {
    if((Random.nextInt(100)+1)<10)
      true
    else false
  }

  private def deserializeResp[T:JsonReader](futureResp:Future[HttpResponse]) : Future[T] = {
    for {
      response <- futureResp
      result <- Future {
        val jsonAst = JsonParser(response.entity.asString)
        jsonAst.convertTo[T]
      }
    } yield result
  }

  override def receive: Receive = {
    case CreateAPage =>
      val createPagePipeline = (addHeader("Login", userId.toString)
        ~> sendReceive
        ~> unmarshal[Page])
      val title = faker.Lorem.sentence()
      val description = faker.Lorem.paragraph()
      val website = "http://www." + faker.Internet.domain_name
      val page = Page(None, Some(title), Some(description), Some(website), None, None)
      val resp = addLoginInfoNSendRequest(Put(pagePrefix, page))
      val pageFuture = deserializeResp[Page](resp)
      pageFuture foreach((newPage)=>{
        Client.pageIds.add(newPage.page_id.get)
        context.actorSelection("/user/client_manager")  ! RequestComplete
      })

    case CreatePost =>
      var message = faker.Lorem.sentence()
      var description = faker.Lorem.sentence()
      var link = "http://www." + faker.Internet.domain_name + "/" + Random.alphanumeric.take(10).mkString
      var post:fbsim.common.Post  = null
      if(!pageOrUser){
        val aesKey = SecurityApis.generateAESKey
        val IV = SecurityApis.genIV
        val encryptedKeys = new mutable.HashMap[Int, String]
        idsOfFriends foreach ((userId)=>{
          encryptedKeys.put(userId, SecurityApis.encryptSharedKey(aesKey, Client.userPublicKeys(userId)))
        })
        //Encrypt the fields, using IV to randomize aes algo in cbc mode
        message = SecurityApis.encrypt(message,aesKey, IV)
        description = SecurityApis.encrypt(description,aesKey, IV)
        link = SecurityApis.encrypt(link,aesKey, IV)
        post = fbsim.common.Post(None,message,Some(description), Some(link),Some(EncDetails(encryptedKeys.toMap, SecurityApis.base64Encode(IV))))
        addLoginInfoNSendRequest(Put(fbGraphApiUser(userPrefix, idsOfFriends(Random.nextInt(idsOfFriends.size)), Some(postsSuffix)), post)).foreach(printErrorResponse)
      } else {
        post = fbsim.common.Post(None,message,Some(description), Some(link),None)
        addLoginInfoNSendRequest(Put(fbGraphApiUser(pagePrefix, Client.pageIds.get(Random.nextInt(Client.pageIds.size())), Some(postsSuffix)), post)).foreach(printErrorResponse)
      }

    case GetProfile =>
      if(!pageOrUser)
        addLoginInfoNSendRequest(Get(userPrefix+"/"+idsOfFriends(Random.nextInt(idsOfFriends.size)))) foreach printErrorResponse
      else addLoginInfoNSendRequest(Get(pagePrefix+"/" + Client.pageIds.get(Random.nextInt(Client.pageIds.size)))) foreach printErrorResponse
    case GetPosts =>
      if(!pageOrUser){
        addLoginInfoNSendRequest(Get(fbGraphApiUser(userPrefix, idsOfFriends(Random.nextInt(idsOfFriends.size)), Some(postsSuffix)))).foreach((resp)=>{
          val json = JsonParser(resp.entity.asString)
          val posts = json.convertTo[Posts]
          posts.posts.foreach((post)=>{
            val json = postFormat.write(post)
            println("Encrypted Post: "+ json.prettyPrint)
            if(post.encDetails.isDefined){
              val encKey = post.encDetails.get.encAESKey.get(userId)
              if(encKey.isDefined){
                val aesKey = SecurityApis.decryptSharedKey(SecurityApis.base64Decode(encKey.get), prvtKey)
                val IV = SecurityApis.base64Decode(post.encDetails.get.IV)
                val message = SecurityApis.decrypt(post.post_message,aesKey, IV)
                val desc =  post.post_description map (SecurityApis.decrypt(_,aesKey, IV))
                val link = post.post_link map (SecurityApis.decrypt(_,aesKey, IV))
                val decryptedPost = fbsim.common.Post(post.post_id,message,desc, link, None)
                val json = postFormat.write(decryptedPost)
                println("Decrypted Post: "+ json.prettyPrint)
              }
            } 
          })
        })

      }
      else addLoginInfoNSendRequest(Get(fbGraphApiUser(pagePrefix, Client.pageIds.get(Random.nextInt(Client.pageIds.size)), Some(postsSuffix)))) foreach printErrorResponse

    case CreateRandomFriendList(avgFriends) =>
      addLoginInfoNSendRequest(Get(urlPrefix+"/intializefriends/" + avgFriends)) foreach {(httpresp)=>
        context.actorSelection("/user/client_manager")  ! RequestComplete
      }
    case InitUserFriends =>
      val response = addLoginInfoNSendRequest(Get(fbGraphApiUser(userPrefix, userId, Some(friendsSuffix))))
      val friendListFuture = deserializeResp[FriendList](response)
      friendListFuture foreach((friendList)=>{
        friendList.friends foreach (idsOfFriends += _.id.get)
        context.actorSelection("/user/client_manager") ! RequestComplete
      })
    case AddFriend =>
      val randFriendId = Random.nextInt(ClientStats.nUsers)+1
      val httpResponse = addLoginInfoNSendRequest(Put(fbGraphApiUser(userPrefix, userId, Some(friendsSuffix)), NodeId(randFriendId)))
      val result = Await.result(httpResponse, Duration.create(waitTime, TimeUnit.SECONDS))
      if(result.status.isFailure) {
        self ! AddFriend
      }
      else {
        context.actorSelection("/user/client_manager") ! RequestComplete
      }

    case Register =>
      val first_name = faker.Name.first_name
      val last_name = faker.Name.last_name
      val name =  first_name + " " + last_name
      val randdate = (1970+ Random.nextInt(35)) + "-" + (Random.nextInt(12)+1).formatted("%02d") +"-" + (Random.nextInt(28)+1).formatted("%02d") + "T00:00:00"
      val birthday = DateTime.fromIsoDateTimeString(randdate)
      val gender = Random.nextInt(2) match {
        case 0 => "male"
        case 1 => "female"
      }

      val keyPair = SecurityApis.generateRSAKeyPair
      prvtKey = keyPair.getPrivate
      pubKey = keyPair.getPublic


      val user = User(None,first_name,last_name, Some(faker.Internet.free_email(name)),birthday, Some(gender), None, None, SecurityApis.base64Encode(pubKey.getEncoded))
      userPipeline(Put(userPrefix, user)) map ((createdUser) => {
        userId = createdUser.id.get
        idsOfFriends += userId
        Client.userPublicKeys.put(userId, pubKey)
        context.actorSelection("/user/client_manager") ! RequestComplete
      })
  }
  val printErrorResponse : HttpResponse => Unit = (httpResponse) =>
    if(httpResponse.status.isFailure) {
      println("Failed: "+ httpResponse.entity.asString)
    }

  val userPipeline: HttpRequest => Future[User] = (
    addHeader("Login", userId.toString )
      ~> sendReceive
      ~> unmarshal[User])

}
