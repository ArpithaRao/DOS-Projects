package fbsim.common

import spray.http.DateTime
import spray.httpx.SprayJsonSupport
import spray.json._

import scala.collection.mutable

case class Post(var post_id:Option[Int], post_message:String, post_description:Option[String], post_link:Option[String], encDetails: Option[EncDetails])
case class User(var id:Option[Int], firstName:String, lastName:String, email:Option[String], birthday:Option[DateTime], gender:Option[String], var friendIds:Option[mutable.Set[Int]] , var posts:Option[mutable.ListBuffer[Post]], publicKey:String)
case class Page(var page_id:Option[Int], page_title:Option[String], page_description:Option[String], page_website:Option[String], var posts:Option[mutable.ListBuffer[Post]], var postby:Option[Int])
case class FriendList(friends:List[User])
case class Posts(posts:List[Post])
case class ApiException(error:String)
case class Successful(status:String="Successful")
case class NodeId(id:Int)
case class EncDetails(encAESKey:Map[Int, String], IV: String)

object DefaultJsonFormatterProtocol extends DefaultJsonProtocol with SprayJsonSupport{


  implicit def mutableSetFormat[T :JsonFormat] = viaSeq[mutable.Set[T], T](seq => mutable.Set(seq :_*))
  implicit def mutableListBufferFormat[T :JsonFormat] = viaSeq[mutable.ListBuffer[T], T](seq => mutable.ListBuffer(seq :_*))


  implicit object dateFormat extends RootJsonFormat[spray.http.DateTime] {
    def read(json: JsValue): spray.http.DateTime =
      DateTime.fromIsoDateTimeString(json.convertTo[String]) getOrElse (null)
    def write(date: DateTime) = JsString(date.toIsoDateTimeString)
  }
  
  override implicit def mapFormat[K :JsonFormat, V :JsonFormat] = new RootJsonFormat[Map[K, V]] {
    def write(m: Map[K, V]) = JsObject {
      m.map { field =>
        field._1.toJson match {
          case JsString(x) => x -> field._2.toJson
          case JsNumber(x) => x.toString() -> field._2.toJson
          case x => throw new SerializationException("Map key must be formatted as JsString, not '" + x + "'")
        }
      }
    }
    def read(value: JsValue) = value match {
      case x: JsObject => x.fields.map { field =>{
        if(field._1.matches("[-+]?\\d+(\\.\\d+)?"))
          (JsNumber(field._1).convertTo[K], field._2.convertTo[V])
        else (JsString(field._1).convertTo[K], field._2.convertTo[V])
      }
      } (collection.breakOut)
      case x => deserializationError("Expected Map as JsObject, but got " + x)
    }
  }

  implicit val encInfoFormat = jsonFormat2(EncDetails)
  implicit val postFormat = jsonFormat5(Post)
  implicit val userFormat= jsonFormat9(User)
  implicit val pageFormat = jsonFormat6(Page)
  implicit val friendsFormat = jsonFormat1(FriendList)
  implicit val postsFormat = jsonFormat1(Posts)
  implicit val apiExceptionFormat = jsonFormat1(ApiException)
  implicit val successFormat = jsonFormat1(Successful)
  implicit val nodeIdFormat = jsonFormat1(NodeId)



}




