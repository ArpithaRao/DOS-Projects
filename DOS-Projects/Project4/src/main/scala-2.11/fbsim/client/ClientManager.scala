package fbsim.client

import java.util.concurrent.TimeUnit

import akka.actor.{Actor, Cancellable}
import akka.util.Timeout
import fbsim.client.ClientManager.{RequestComplete, Start, StartSimulation}
import fbsim.client.ClientUser.{CreateAPage, InitUserFriends, Register, _}

import scala.concurrent.duration.Duration
import scala.util.Random

object ClientManager{
  case class Start()
  case class StartSimulation()
  case class RequestComplete()
}

class ClientManager extends Actor{

  implicit val ec = context.dispatcher
  implicit val timeout = Timeout(120, TimeUnit.SECONDS)

  var numreqs = 0
  val createTicker:Cancellable = null
  val getTicker:Cancellable = null

  def register: Receive = {
    case Start =>
      for(i <- 0L until ClientStats.nUsers){
        context.actorSelection("/user/client_user"+i) ! Register
      }
    case RequestComplete =>
      numreqs += 1
      //Start adding friends to reach the avg friends ratio
      if(numreqs == ClientStats.nUsers) {
        numreqs = 0
        context.become(randomFL)
        self ! Start
      }
  }

  def randomFL:Receive = {
    case Start =>
      //Perform add friends
      context.actorSelection("/user/client_user"+0) ! CreateRandomFriendList(ClientStats.averageFriends)
      context.system.scheduler.scheduleOnce(Duration.create(60,TimeUnit.SECONDS),self, RequestComplete)
    case RequestComplete =>
      println("Random FriendList creation is complete")
      numreqs = 0
      context.become(initializeFriendList)
      self ! Start
  }

  def initializeFriendList:Receive = {
    case Start =>
      for(i <- 0 until ClientStats.nUsers){
        context.actorSelection("/user/client_user"+i) ! InitUserFriends
      }
    case RequestComplete =>
      numreqs += 1
      if(numreqs == ClientStats.nUsers) {
        numreqs = 0
        println("FriendList is initialized")
        context.become(initializePages)
        self ! Start
      }
  }

  def initializePages: Receive = {
    case Start =>
      for(i <- 0 until ClientStats.numPages){
        context.actorSelection("/user/client_user"+i) ! CreateAPage
      }
    case RequestComplete =>
      numreqs += 1
      if(numreqs == ClientStats.numPages) {
        numreqs = 0
        println("All Pages have been Initialized")
        context.become(simulate)
        self ! Start
      }
  }

  def simulate: Receive = {
    case Start =>
      println("Initialization is complete")
      println("Simulating...kindly wait or press Ctrl + C to terminate simulation.")
      val createHeartBeat = (1/ClientStats.create_op_rate * 1000000).toLong
      context.system.scheduler.schedule(Duration.create(0, TimeUnit.SECONDS), Duration.create(createHeartBeat, TimeUnit.MICROSECONDS), new Runnable {
        override def run(): Unit = {
          context.actorSelection("/user/client_user" + Random.nextInt(ClientStats.nUsers)) ! CreatePost
        }
      })

      val getHeartBeat = (1/ClientStats.create_op_rate * 1000000).toLong
      context.system.scheduler.schedule(Duration.create(0, TimeUnit.SECONDS), Duration.create(getHeartBeat, TimeUnit.MICROSECONDS), new Runnable {
        override def run(): Unit = {
          if((Random.nextInt(100)+1)<10)
            context.actorSelection("/user/client_user" + Random.nextInt(ClientStats.nUsers)) ! GetPosts
          else context.actorSelection("/user/client_user" + Random.nextInt(ClientStats.nUsers)) ! GetProfile
        }
      })
  }

  override def receive: Receive = {
    case StartSimulation =>
      context.become(register)
      self ! Start
  }
}
