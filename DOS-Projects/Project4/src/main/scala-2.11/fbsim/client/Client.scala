package fbsim.client

import java.security.PublicKey
import java.util
import java.util.Collections

import akka.actor.{ActorSystem, Props}
import fbsim.client.ClientManager.StartSimulation

import scala.collection.concurrent.TrieMap


object ClientStats {

  var nUsers = 100
  def numPages = (0.2 * nUsers).toInt
  def create_op_rate = 10
  def get_operation_rate = 100
  def averageFriends:Int = 10


}

object Client extends App{
  val pageIds = Collections.synchronizedList(new util.ArrayList[Int]())
  val userPublicKeys = new TrieMap[Int, PublicKey]

  if(args.length < 1) {
    println("\t\tsbt \"run numUsers\"")
  } else {
    try{
      val numUsers = args(0).toInt
      ClientStats.nUsers = numUsers
      implicit val system = ActorSystem("client_simulator")
      val manager = system.actorOf(Props[ClientManager], "client_manager")
      for(i <-0 until ClientStats.nUsers) {
        system.actorOf(Props[ClientUser], "client_user" +i)
      }
      manager ! StartSimulation

    }catch{
      case e: NumberFormatException =>
        println("\t\tsbt \"run numUsers \"")
        System.exit(1)
    }

  }


}
