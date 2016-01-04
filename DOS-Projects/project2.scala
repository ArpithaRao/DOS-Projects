import java.io.{FileWriter, BufferedWriter, File}

import akka.actor._
import scala.collection.immutable.IntMap
import scala.collection.mutable.ListBuffer
import scala.util.Random

case class Start(numNodes: Int, topology: String, algorithm: String)

case class Gossip()

case class PushSum(s: Int, w: Int)

case class Stop()

case class Initialize(id: Int, nodeActors: List[ActorRef], numNodes: Int, neighbours: ListBuffer[Int])

case class RemoveNeighbor(id: Int)

object project2 extends App {
  override def main(args: Array[String]) {
    var numNodes = 10
    var topology = "i3d"
    var algorithm = "gossip"
    if (args.length != 3) {
      println("Invalid arguments")
    }
    else {
      numNodes = args(0).toInt
      topology = args(1)
      algorithm = args(2)
    }
    val actorSystem = ActorSystem("GossipSystem")
    val parentActor = actorSystem.actorOf(Props(new ParentActor), name = "ParentActor")
    parentActor ! Start(numNodes, topology, algorithm)
  }
}

class ParentActor extends Actor {
  var startTime: Long = 0
  var endTime: Long = 0
  var neighbours: ListBuffer[Int] = new ListBuffer[Int]
  var nodes: Int = 0
  var nodesOnTop: ListBuffer[Int] = new ListBuffer[Int]
  var nodesOnBottom: ListBuffer[Int] = new ListBuffer[Int]
  var nodesOnRight: ListBuffer[Int] = new ListBuffer[Int]
  var nodesOnLeft: ListBuffer[Int] = new ListBuffer[Int]
  var nodesAtBack: ListBuffer[Int] = new ListBuffer[Int]
  var nodesAtFront: ListBuffer[Int] = new ListBuffer[Int]
  var cubeRootOfNumnodes: Double = 0
  var squareOfCubeRoot: Double = 0
  val nodeActorName: String = "nodeActor"
  var originalNumNodes: Int = 0

  def receive = {



    case Start(numNodes, topology, algorithm) =>
      nodes = numNodes
      originalNumNodes = numNodes
      if (topology.equalsIgnoreCase("3D") || topology.equalsIgnoreCase("i3D")) {
        cubeRootOfNumnodes = math.cbrt(nodes)
        if (cubeRootOfNumnodes % 1 != 0) {
          cubeRootOfNumnodes = cubeRootOfNumnodes.toInt + 1
          nodes = math.pow(cubeRootOfNumnodes, 3).toInt
        }
        squareOfCubeRoot = math.pow(cubeRootOfNumnodes, 2)
        nodesAtFront = ListBuffer.range(0, squareOfCubeRoot.toInt)
        nodesAtBack = ListBuffer.range((nodes - squareOfCubeRoot).toInt, nodes)
        nodesOnLeft = nodesAtFront.map(_ * cubeRootOfNumnodes.toInt)
        nodesOnRight = nodesOnLeft.map(_ + (cubeRootOfNumnodes - 1).toInt)
        for (j <- 0 to (cubeRootOfNumnodes - 1).toInt) {
          nodesOnTop ++= (nodesAtFront.take(cubeRootOfNumnodes.toInt).map(_ + (squareOfCubeRoot * j).toInt))
        }
        nodesOnBottom = nodesOnTop.map(_ + (squareOfCubeRoot - cubeRootOfNumnodes).toInt)
      }

      var nodeActors: List[ActorRef] = Nil
      for (i <- 0 to nodes - 1) {
        nodeActors ::= context.actorOf(Props(new NodeActor()), nodeActorName + i)
      }

      if (topology.equalsIgnoreCase("full")) {
        for (i <- 0 to nodes - 1) {
          neighbours = new ListBuffer[Int]
          neighbours = ListBuffer.range(0, nodes - 1) - i
          nodeActors(i) ! Initialize(i, nodeActors, nodes, neighbours)
        }
      }
      else if (topology.equalsIgnoreCase("3D")) {
        for (i <- 0 to nodes - 1) {
          neighbours = new ListBuffer[Int]
          if (!nodesOnBottom.contains(i))
            neighbours.+=(i + cubeRootOfNumnodes.toInt)
          if (!nodesOnTop.contains(i))
            neighbours.+=(i - cubeRootOfNumnodes.toInt)
          if (!nodesOnLeft.contains(i))
            neighbours.+=(i - 1)
          if (!nodesOnRight.contains(i))
            neighbours.+=(i + 1)
          if (!nodesAtBack.contains(i))
            neighbours.+=(i + squareOfCubeRoot.toInt)
          if (!nodesAtFront.contains(i))
            neighbours.+=(i - squareOfCubeRoot.toInt)
          nodeActors(i) ! Initialize(i, nodeActors, nodes, neighbours)
        }
      }
      else if (topology.equalsIgnoreCase("line")) {
        for (i <- 0 to nodes - 1) {
          neighbours = new ListBuffer[Int]
          if (i == 0) {
            neighbours.+=(i + 1)
          }
          else if (i == nodes - 1) {
            neighbours.+=(i - 1)
          }
          else {
            neighbours.+=(i + 1, i - 1)
          }
          nodeActors(i) ! Initialize(i, nodeActors, nodes, neighbours)
        }
      }
      else if (topology.equalsIgnoreCase("i3D")) {
        var randomVariableMap: Map[Int, Int] = IntMap()
        var randomNotToSelect: ListBuffer[Int] = ListBuffer()
        for (i <- 0 to nodes - 1) {
          var randomNeighbours: ListBuffer[Int] = ListBuffer.range(0, nodes)
          neighbours = new ListBuffer[Int]
          if (!nodesOnBottom.contains(i))
            neighbours.+=(i + cubeRootOfNumnodes.toInt)
          if (!nodesOnTop.contains(i))
            neighbours.+=(i - cubeRootOfNumnodes.toInt)
          if (!nodesOnLeft.contains(i))
            neighbours.+=(i - 1)
          if (!nodesOnRight.contains(i))
            neighbours.+=(i + 1)
          if (!nodesAtBack.contains(i))
            neighbours.+=(i + squareOfCubeRoot.toInt)
          if (!nodesAtFront.contains(i))
            neighbours.+=(i - squareOfCubeRoot.toInt)

          if (randomVariableMap.contains(i)) {
            neighbours += (randomNeighbours(randomVariableMap(i)))
          }
          else {
            randomNeighbours.-=(i)
            randomNeighbours --= neighbours
            randomNeighbours --= randomNotToSelect
            if (randomNeighbours.length > 0) {
              val randomNeighbourIndex = Random.nextInt(randomNeighbours.length)

              randomVariableMap += (i -> randomNeighbours(randomNeighbourIndex))
              randomVariableMap += (randomNeighbours(randomNeighbourIndex) -> i)
              randomNotToSelect +=(i, randomNeighbours(randomNeighbourIndex))
              neighbours += randomNeighbours(randomNeighbourIndex)
            }
          }
          nodeActors(i) ! Initialize(i, nodeActors, nodes, neighbours)
        }
      }

      startTime = System.currentTimeMillis()

      val nodeIndex = Random.nextInt(nodes)

      if (algorithm.equalsIgnoreCase("gossip"))
        nodeActors(nodeIndex) ! Gossip

      else if (algorithm.equalsIgnoreCase("push-sum"))
        nodeActors(nodeIndex) ! PushSum(0.1,0.1)

      else
        self ! Stop

    case Stop =>
      endTime = System.currentTimeMillis()
      //println(originalNumNodes, endTime - startTime)
      println((endTime - startTime) + "ms")
      context.system.shutdown()
  }
}

class NodeActor() extends Actor {
  var count = 0
  var own_iD = 0
  var own_s: Double = 0.0
  var own_w: Double = 1.0
  var neighbours: ListBuffer[Int] = new ListBuffer[Int]
  var parent: ActorRef = null
  var nodeActors: List[ActorRef] = Nil
  var numNodes = 0
  var newSumEstimate: Double = 0.0
  var oldSumEstimate: Double = 0.0

  def receive = {
    case Initialize(id, nodeActors1, numNodes1, neighbours1) =>
      own_iD = id
      parent = sender()
      nodeActors = nodeActors1
      numNodes = numNodes1
      neighbours = neighbours1
      own_s = id.toDouble

    case Gossip =>
      if (neighbours.length > 0) {
        count += 1
        if (count >= 10) {
          for (i <- 0 to neighbours.length - 1) {
            nodeActors(neighbours(i)) ! RemoveNeighbor(own_iD)
          }
          val i = Random.nextInt(neighbours.length)
          nodeActors(neighbours(i)) ! Gossip
          self ! PoisonPill
        }
        else {
          val i = Random.nextInt(neighbours.length)
          nodeActors(neighbours(i)) ! Gossip
        }
      }
      else
        parent ! Stop

    case PushSum(prev_s, prev_w) =>
      if (neighbours.length > 0) {
        count += 1
        oldSumEstimate = own_s / own_w
        own_s = (own_s + prev_s) / 2
        own_w = (own_w + prev_w) / 2
        newSumEstimate = own_s / own_w

        if (math.abs(newSumEstimate - oldSumEstimate) < math.pow(10, -10))
          count += 1
        else
          count = 0
        if (count >= 3) {
          for (i <- 0 to neighbours.length - 1) {
            nodeActors(neighbours(i)) ! RemoveNeighbor(own_iD)
          }
          val i = Random.nextInt(neighbours.length)
          nodeActors(neighbours(i)) ! PushSum(own_s, own_w)
          self ! PoisonPill
        }
        else {
          val i = Random.nextInt(neighbours.length)
          nodeActors(neighbours(i)) ! PushSum(own_s, own_w)
        }
      }
      else
        parent ! Stop

    case RemoveNeighbor(idToRemove) =>
      neighbours -= idToRemove
  }
}
