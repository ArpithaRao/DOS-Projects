package fbsim.server.service

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import fbsim.common._
import fbsim.server.service.PageService._
import fbsim.server.service.PostService.{CreatePost, GetPosts}
import fbsim.server.service.UserService._
import spray.http.MediaTypes._
import spray.routing.AuthenticationFailedRejection.CredentialsRejected
import spray.routing._
import spray.routing.authentication._

import scala.collection.concurrent.{Map, TrieMap}
import scala.concurrent.Future
import scala.concurrent.duration._

class FbRestServiceActor extends Actor with FbRestService {
  def actorRefFactory = context
  def receive = runRoute( restApiRoutes )
}

trait FbRestService extends HttpService with FbAuthn {

  implicit val ec = actorRefFactory.dispatcher
  implicit val timeout = Timeout(5 seconds)
  import fbsim.common.DefaultJsonFormatterProtocol._
  val userfunc = actorRefFactory.actorSelection("/user/fb_user")
  val pagefunc = actorRefFactory.actorSelection("/user/fb_page")
  val postfunc = actorRefFactory.actorSelection("/user/fb_post")

  val restApiRoutes = {
    authenticate(validatinglogin) { user =>
      respondWithMediaType(`application/json`) {
        path("page") {
          put {
            entity(as[Page]) { page =>
              onSuccess(pagefunc ? CreatePage(page, user)) {
                case response: Page =>
                  complete(response)
              }
            }
          }
        } ~ pathPrefix("page" / IntNumber) { (page_id) =>
          pathEnd {
            get {
              onSuccess(pagefunc ? GetPage(page_id)) {
                case response: Page =>
                  complete(response)
              }
            }
          }
        } ~ path("page" / IntNumber / "posts") { (page_id) =>
          put {
            entity(as[Post]) { post =>
              onSuccess(postfunc ? CreatePost(post, page_id, Profile.PageProfile, user)) {
                case response: Post =>
                  complete(response)
              }
            }
          } ~ get {
            onSuccess(postfunc ? GetPosts(page_id, Profile.PageProfile, user)) {
              case response: Posts =>
                complete(response)
            }
          }
        }
      }
    }
  } ~ {
    path("user") {
      put {
        entity(as[User]) { user =>
          respondWithMediaType(`application/json`) {
            onSuccess(userfunc ? CreateUser(user)) {
              case response: User =>
                complete(response)
            }
          }
        }
      }
    } ~ authenticate(validatinglogin) { user =>
      pathPrefix("user" / IntNumber) { (user_id) =>
        pathEnd {
          get {
            respondWithMediaType(`application/json`) {
              onSuccess(userfunc ? GetUser(user_id, user)) {
                case response: User =>
                  complete(response)
              }
            }
          }
        }
      } ~ path("user" / IntNumber / "friends") { (user_id) =>
        put {
          entity(as[NodeId]) { friendId =>
            respondWithMediaType(`application/json`) {
              onSuccess(userfunc ? AddFriend(friendId.id, user_id, user)) {
                case response: Successful =>
                  complete(response)
              }
            }
          }
        } ~ get {
          respondWithMediaType(`application/json`) {
            onSuccess(userfunc ? GetFriends(user_id, user)) {
              case response: FriendList =>
                complete(response)
            }
          }
        }
      } ~ path("user" / IntNumber / "posts") { (user_id) =>
        put {
          entity(as[Post]) { post =>
            onSuccess(postfunc ? CreatePost(post, user_id, Profile.UserProfile, user)) {
              case response: Post =>
                complete(response)
            }
          }
        } ~ get {
          onSuccess(postfunc ? GetPosts(user_id, Profile.UserProfile, user)) {
            case response: Posts =>
              complete(response)

          }
        }
      }
    }
  } ~ {
    path("intializefriends" / IntNumber) { averageNumFriends =>
      onSuccess(userfunc ? InitializeFriends(averageNumFriends)) {
        case response: Successful =>
          complete(response)
      }
    }
  }
}


trait FbAuthn {
  def actorRefFactory: ActorContext
  def validatinglogin: ContextAuthenticator[User] = context =>
  {
    val header_name = context.request.headers
    val loginHeader = header_name.filter( _.name == "Login")
    val sigHeader = header_name.filter( _.name == "Signature")
    val url = context.request.uri.toString
    val content = context.request.entity.asString

    if(!loginHeader.isEmpty && !sigHeader.isEmpty){
      checkforcredentials(loginHeader.head.value.toInt, sigHeader.head.value, url, content)
    }
    else checkforcredentials(0, null,null,null)
  }

  private def checkforcredentials(loginId: Int, signatureValue: String, url:String, content:String): Future[Authentication[User]] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    Future {
      if (loginId != 0) {
        val user = ModelofData.userprofiles.get(loginId)
        if (user.isDefined){
          val pubKey = user.get.publicKey
          val signedContent = loginId.toString + url + content
          if(SecurityApis.verify(signedContent, signatureValue,SecurityApis.buildRSAPublicKey(pubKey))){
            Either.cond(true, user.get, AuthenticationFailedRejection(CredentialsRejected, List()))
          } else {
            println("Signature Verification has been failed")
            Either.cond(false, null, AuthenticationFailedRejection(CredentialsRejected, List()))
          }
        }
        else Either.cond(false, null, AuthenticationFailedRejection(CredentialsRejected, List()))
      } else Either.cond(false, null, AuthenticationFailedRejection(CredentialsRejected, List()))
    }
  }
}


object ModelofData {
  val userprofiles:Map[Int, User]= new TrieMap[Int, User]()
  val pageprofiles:Map[Int, Page]= new TrieMap[Int, Page]()
  val posts:Map[Int, Post]= new TrieMap[Int, Post]()

}









