package fbsim.server.service

import akka.actor.Actor
import fbsim.common._
import fbsim.server.service.PageService.{CreatePage, GetPage}
import fbsim.server.service.UserService._

import scala.collection.mutable
import scala.util.Random

object Profile extends Enumeration {
  type Profile = Value
  val PageProfile, UserProfile = Value
}

object PageService{
  case class GetPage(pageId:Int)
  case class CreatePage(page:Page, loginContext: User)
}

class PageService extends Actor{
  var pageCount = 0

  override def receive: Receive = {
    case CreatePage(page, loginContext) =>
      page.postby = loginContext.id
      pageCount += 1
      page.page_id =  Some(pageCount)
      page.posts = Some(mutable.ListBuffer[Post]())
      ModelofData.pageprofiles.put(pageCount, page)
      sender() ! page

    case GetPage(pageId) =>
      if(ModelofData.pageprofiles.get(pageId).isDefined){
        val page = ModelofData.pageprofiles(pageId)
        val limitedFieldsPage = Page(page.page_id,page.page_title, page.page_description, page.page_website,None, page.postby)
        sender() ! limitedFieldsPage
      }
      else ApiException("This page does not exist")
  }
}

object UserService{
  case class GetUser(id:Int, loginContext: User)
  case class CreateUser(user: User)
  case class AddFriend(friend_id:Int, userId:Int, loginContext: User)
  case class GetFriends(userId:Int, loginContext: User)
  case class InitializeFriends(averageFriends:Int)

  def isFriend(userId:Int, friendId:Int) : Boolean = {
    if(userId == friendId)
      true
    else{
      if(ModelofData.userprofiles.get(userId).isDefined){
        val user = ModelofData.userprofiles.get(userId).get
        if(user.friendIds.get.contains(friendId))
          true
        else false
      } else false
    }
  }
}

class UserService extends Actor{
  var count = 0

  override def receive: Receive = {
    case GetUser(id, loginContext) =>
      if(UserService.isFriend(loginContext.id.get, id)){
        val user = ModelofData.userprofiles.get(id)
        if(user.isDefined){
          val limitedFieldsUser = User(user.get.id, user.get.firstName, user.get.lastName, user.get.email, user.get.birthday, user.get.gender, None, None, user.get.publicKey)
          sender() ! limitedFieldsUser
        } else ApiException("Its invalid Request")

      } else sender() ! ApiException("Un-Authorized")

    case AddFriend(friendId:Int, userId:Int, loginContext: User) =>
      if(loginContext.id.get != userId){
        sender() ! ApiException("Un-Authorized")
      } else {
        val currentUser = ModelofData.userprofiles.get(loginContext.id.get).get
        if(!currentUser.friendIds.get.contains(friendId) && ModelofData.userprofiles.contains(friendId)){
          val friend = ModelofData.userprofiles.get(friendId).get
          currentUser.friendIds.get += friend.id.get
          friend.friendIds.get += currentUser.id.get

          sender() ! Successful()
        } else {
          sender() ! ApiException("Already a friend")
        }

      }

    case CreateUser(user:User) =>
      count += 1
      user.id = Some(count)
      user.friendIds = Some(mutable.Set[Int]())
      user.posts = Some(mutable.ListBuffer[Post]())
      ModelofData.userprofiles.put(count, user)
      sender() ! user

    case GetFriends(userId:Int, loginContext:User) =>
      if(!UserService.isFriend(loginContext.id.get, userId)){
        sender() ! ApiException("Un-Authorized")
      } else {
        val friendslist = new mutable.ListBuffer[User]()
        ModelofData.userprofiles.get(userId).get.friendIds.get foreach {(friendId) =>
          friendslist += ModelofData.userprofiles.get(friendId).get
        }
        sender() ! FriendList(friendslist.toList)
      }
    case InitializeFriends(avgFriends) =>
      val userIds = ModelofData.userprofiles.keySet.toList
      val numUsers = userIds.size
      var connectionCount = 0L
      while(connectionCount/numUsers < avgFriends){
        val user = ModelofData.userprofiles.get(userIds(Random.nextInt(numUsers))).get
        var randomFriendId = 0
        do {
          randomFriendId = userIds(Random.nextInt(numUsers))
        }while(user.friendIds.get.contains(randomFriendId))
        val anotherUser = ModelofData.userprofiles.get(randomFriendId).get
        user.friendIds.get += anotherUser.id.get
        anotherUser.friendIds.get += user.id.get
        connectionCount += 2
      }
      sender()! Successful()
  }
}

