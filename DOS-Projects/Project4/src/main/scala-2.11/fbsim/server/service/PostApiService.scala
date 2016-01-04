package fbsim.server.service

import akka.actor.Actor
import fbsim.common.{Posts, ApiException, User, Post}
import fbsim.server.service.PostService.{GetPosts, CreatePost}


object PostService{
  case class CreatePost(post:Post, nodeId:Int, nodeType:Profile.Profile, loginContext: User)
  case class GetPosts(nodeId:Int, nodeType:Profile.Profile, loginContext: User)
}

class PostService extends Actor {
  var count = 0

  override def receive: Receive = {
    case CreatePost(post: Post, nodeId: Int, nodeType: Profile.Profile,  loginContext: User) =>
      count = count + 1
      post.post_id = Some(count)
      if (nodeType == Profile.PageProfile) {
        if(ModelofData.pageprofiles.get(nodeId).isDefined){
          val page = ModelofData.pageprofiles.get(nodeId).get
          page.posts.get += post
          sender () ! post
        }else sender() ! ApiException("API exception")
      } else {
        val isaFriend = UserService.isFriend(loginContext.id.get, nodeId)
        if(!isaFriend)
          sender() ! ApiException("Un - Authorized")
        else if(ModelofData.userprofiles.get(nodeId).isDefined){
          //Add the post into the posts structure of all users with whom the post is shared
          post.encDetails foreach((encDetails)=>{
            encDetails.encAESKey.keys foreach((userId)=>{
              val user = ModelofData.userprofiles.get(userId).get
              user.posts.get += post
            })
          })
          sender () ! post
        }else sender() ! ApiException("API exception")
      }

    case GetPosts(nodeId: Int, nodeType: Profile.Profile, loginContext: User) =>
      if(nodeType == Profile.PageProfile){
        if(ModelofData.pageprofiles.get(nodeId).isDefined){
          val page = ModelofData.pageprofiles.get(nodeId).get
          sender() ! Posts(page.posts.get.toList)
        }
      } else {
        if(UserService.isFriend(loginContext.id.get, nodeId)){
          val user = ModelofData.userprofiles.get(nodeId).get
          sender() ! Posts(user.posts.get.toList)
        }else sender() ! ApiException("Un-Authorized")
      }

  }






}

