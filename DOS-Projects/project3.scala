import akka.actor.{ActorRef, Props, Actor, ActorSystem}
import akka.util.Timeout
import akka.pattern.ask
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.concurrent.{Await, Future}
import akka.util.Timeout
import scala.concurrent.duration._
import scala.util.Random

case class Initialize(id: Int, m: Int, nodeActors: List[ActorRef])

case class Start(numNodes: Int, numRequests: Int, actorSystem: ActorSystem)

case class BuildFingerTable()

case class GetFingerEntry(index: Int)


case class GetOwnPredecessorAsReply()

case class GetOwnSuccessorAsReply()

case class NotifyForStabilization(id: Int)

case class Join(id: Int, isFirsNode: Boolean)

case class InitFingerTable(id: Int, isFirsNode: Boolean)

case class NotifyJoinInNetwork(id: Int)

case class FixFingers()

case class Stabilize()

case class GetIDAsReply()

case class FindPredecessorAsReply(ofID: Int)

case class FindSucessorAsReply(ofID: Int)

case class ClosestPrecedingFingerAsReply(id: Int)

case class SetOwnPredecessor(id: Int)

case class SendRequests(nodeActtors: List[ActorRef], nodesInNetwork: ListBuffer[Int], numOfNodes: Int, numOfReq: Int)

case class RunPeriodically(numNodes: Int, nodeActors: List[ActorRef])

object project3 extends App {
  override def main(args: Array[String]) {

    var numNodes = 8
    var numRequests = 10
    if (args.length != 2) {
      println("invalid arguments")
    }
    else {
      numNodes = args(0).toInt
      numRequests = args(1).toInt
    }

    val actorSystem: ActorSystem = ActorSystem("ChordSystem")

    val parentActor = actorSystem.actorOf(Props(new ParentActor), name = "ParentActor")
    parentActor ! Start(numNodes, numRequests, actorSystem)

  }
}

class ParentActor extends Actor {
  var numOfNodes: Int = 10
  var numOfRequests: Int = 10
  val nodeActorName: String = "nodeActor"
  var nodesInNetwork: ListBuffer[Int] = new ListBuffer[Int]
  var m: Int = calculateM(numOfNodes)

  def receive = {
    case Start(numNodes, numRequests, actorSystem) => {
      numOfNodes = numNodes
      numOfRequests = numRequests
      var nodeActors: List[ActorRef] = Nil

      for (i <- 0 to numOfNodes - 1) {
        nodeActors ::= context.actorOf(Props(new NodeActor()), nodeActorName + i)
        /*nodesInNetwork.+=(i);*/
      }

      for (i <- 0 to numOfNodes - 1) {
        nodeActors(i) ! Initialize(i, m, nodeActors)
        //checking if working
        //nodeActors(i) ! BuildFingerTable()
      }

      /*for (i <- 0 to numOfNodes - 1) {
        nodeActors(i) ! BuildFingerTable()
      }*/

      //Join and init finger table
      nodeActors(0) ! Join(0, true)
      //nodeActors(0) ! InitFingerTable(0, true)
      nodesInNetwork.+=(0)
      var randomExistingID = 0
      for (i <- 1 to numNodes - 1) {
        if (i == 1) {
          randomExistingID = 0
        }
        else {
          randomExistingID = nodesInNetwork(Random.nextInt(nodesInNetwork.length - 1))
        }
        nodeActors(i) ! Join(randomExistingID, false)
        //nodeActors(i) ! InitFingerTable(randomExistingID, false)
        nodesInNetwork.+=(i)
      }
      /*self ! SendRequests(nodeActors, nodesInNetwork, numOfNodes, numOfRequests)


      import actorSystem.dispatcher
      val cancellable =
        actorSystem.scheduler.schedule(0 milliseconds,
          50 milliseconds,
          self,
          RunPeriodically(numNodes, nodeActors))*/
    }
    case SendRequests(nodeActors: List[ActorRef], nodesInNetwork, numOfNodes: Int, numOfRequests: Int) => {
      for (j <- 0 to numOfNodes - 1) {
        for (i <- 0 to numOfRequests - 1) {
          val randomIndex: Int = Random.nextInt(nodesInNetwork.length - 1)
          nodeActors(j) ! FindSucessorAsReply(nodesInNetwork(randomIndex))
        }
      }
    }
    case RunPeriodically(numNodes: Int, nodeActors: List[ActorRef]) => {
      for(i<- 0 to numNodes - 1){
        nodeActors(i) ! Stabilize()
        nodeActors(i) ! FixFingers()
      }
    }

    /*case NotifyJoinInNetwork(id: Int) => {
      //nodesInNetwork.+=(id)
      //println(nodesInNetwork.length)
    }*/
  }

  def calculateM(numberOfNodes: Int): Int = {
    math.ceil(math.log10(numOfNodes) / math.log10(2)).toInt
  }
}

class NodeActor extends Actor {
  var parent: ActorRef = null
  var nodeActors: List[ActorRef] = Nil
  var id: Int = 0
  var m: Int = 0
  var predecessor: Int = 0
  var successor: Int = 0
  var finger: ListBuffer[Int] = new ListBuffer[Int]
  var numOfHops: Int = 0

  def receive = {

    case Initialize(id: Int, m: Int, nodeActors: List[ActorRef]) => {
      parent = sender()
      this.id = id
      this.m = m
      this.nodeActors = nodeActors
      println("nodeac: "+ nodeActors.length)
      for (i <- 0 to m - 1) {
        finger += 0
      }
    }

    /*case BuildFingerTable() => {
      for (i <- 0 to m - 1) {

        println("finger for " + this.id + " " + finger(i))
      }
      println("checking if working" + this.id)
    }*/

    case GetOwnPredecessorAsReply() => {
      sender ! predecessor
    }

    case GetOwnSuccessorAsReply() => {
      sender ! successor
    }

    case SetOwnPredecessor(idToSet: Int) => {
      this.predecessor = idToSet
    }
    case FindSucessorAsReply(ofID: Int) => {
      implicit val timeout = Timeout(5 seconds)
      val future = self ? FindPredecessorAsReply(ofID)
      val predecessorID = Await.result(future, timeout.duration).asInstanceOf[Int]
      val future2 = nodeActors(predecessorID) ? GetOwnSuccessorAsReply()
      val successor = Await.result(future2, timeout.duration).asInstanceOf[Int]
      sender ! successor
    }

    case FindPredecessorAsReply(ofID: Int) => {
      var predecessorID = this.id
      while (ofID <= predecessorID || ofID > successor) {
        implicit val timeout = Timeout(5 seconds)
        println("length: "+ nodeActors.length + predecessorID + ofID)
        val future = nodeActors(predecessorID) ? ClosestPrecedingFingerAsReply(ofID)
        predecessorID = Await.result(future, timeout.duration).asInstanceOf[Int]
      }
      sender ! predecessorID
    }

    case ClosestPrecedingFingerAsReply(id: Int) => {
      for (i <- m - 1 to 0 by -1) {
        if (finger(i) > this.id && finger(i) < id) {
          sender ! finger(i)
        }
      }
      sender ! this.id
    }

    case Join(existindID, isFirstNode) => {
      if (isFirstNode) {
        predecessor = 0
        successor = this.id

      }
      else {
        predecessor = 0
        implicit val timeout = Timeout(5 seconds)
        val future = nodeActors(existindID) ? FindSucessorAsReply(this.id)
        successor = Await.result(future, timeout.duration).asInstanceOf[Int]
      }
      self ! InitFingerTable(existindID, isFirstNode)
      //self ! Stabilize()
      //self ! FixFingers()
      //sender() ! NotifyJoinInNetwork(this.id)
    }

    case InitFingerTable(existingID, isFirstNode) => {
      if (isFirstNode) {
        for (i <- 0 to m - 1) {
          finger(i) = this.id
        }
      }
      else {
        implicit val timeout = Timeout(5 seconds)
        val future = nodeActors(existingID) ? FindSucessorAsReply(finger(0))
        finger(0) = Await.result(future, timeout.duration).asInstanceOf[Int]
        val future2 = nodeActors(finger(0)) ? GetOwnPredecessorAsReply()
        this.predecessor = Await.result(future2, timeout.duration).asInstanceOf[Int]
        nodeActors(successor) ! SetOwnPredecessor(this.id)
        for (i <- 0 to m - 2) {
          if (finger(i + 1) >= this.id && finger(i + 1) <= finger(i)) {
            finger(i + 1) = finger(i)
          }
          else {
            implicit val timeout = Timeout(5 seconds)
            val future2 = nodeActors(existingID) ? FindSucessorAsReply(finger(i + 1))
            finger(i + 1) = Await.result(future2, timeout.duration).asInstanceOf[Int]
          }
        }
      }
    }

    case GetFingerEntry(index: Int) => {
      sender ! finger(index)
    }

    case NotifyForStabilization(idToStabilize) => {
      if (predecessor == 0 || (idToStabilize > predecessor && idToStabilize < this.id)) {
        predecessor = idToStabilize
      }
    }

    case FixFingers() => {
      for (i <- 0 to m - 1) {
        implicit val timeout = Timeout(5 seconds)
        val future = self ? FindSucessorAsReply(((this.id + math.pow(2, i)) % m).toInt)
        finger(i) = Await.result(future, timeout.duration).asInstanceOf[Int]
      }
    }
    case Stabilize() => {
      implicit val timeout = Timeout(5 seconds)
      val future = nodeActors(finger(0)) ? GetOwnPredecessorAsReply()
      val x: Int = Await.result(future, timeout.duration).asInstanceOf[Int]
      if (x > this.id && x < this.successor) {
        successor = x
      }
      nodeActors(successor) ! NotifyForStabilization(this.id)
    }
    case GetIDAsReply() => {
      sender ! this.id
    }
  }
}
