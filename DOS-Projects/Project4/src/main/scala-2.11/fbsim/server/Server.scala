package fbsim.server

import java.util.concurrent.TimeUnit

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.io.IO
import akka.util.Timeout
import fbsim.server.ServerMonitor.{ShowStats, Start}
import fbsim.server.service._
import spray.can.Http
import spray.can.server.Stats

import scala.concurrent.duration.Duration


object Server extends App {
  
  implicit val system = ActorSystem("facebook_sim_server")
  val service = system.actorOf(Props[FbRestServiceActor], "fb_api")
  val fb_user = system.actorOf(Props[UserService], "fb_user")
  val fb_page = system.actorOf(Props[PageService], "fb_page")
  val fb_post = system.actorOf(Props[PostService], "fb_post")

  val serverMonitor = system.actorOf(Props[ServerMonitor], "fb_monitor")
  serverMonitor ! Start(service)

}

object ServerMonitor{
  case class Start(service:ActorRef)
  case class ShowStats()

}
class ServerMonitor extends Actor{
  implicit val ec = context.dispatcher
  implicit val timeout = Timeout(5, TimeUnit.SECONDS)
  implicit val system = context.system
  var httpListenerRef: ActorRef = null
  var receivedRequests:Int = 0
  var respondedRequests:Int = 0
  override def receive: Receive = {
    case Start(service) =>
      IO(Http) ! Http.Bind(service, interface = "localhost", port = 8080)
    case x:Http.Bound =>
      httpListenerRef = sender()
      context.system.scheduler.schedule(Duration.create(0, TimeUnit.SECONDS), Duration.create(1, TimeUnit.SECONDS),self, ShowStats)
    case x: Stats =>
      println("Received Requests: " + x.totalRequests)
      httpListenerRef ! Http.ClearStats
      respondedRequests = 0
    case ShowStats =>
      httpListenerRef ! Http.GetStats
  }

}


