package structure

import play.api.db.DB
import play.api.Play.current
import anorm._
import anorm.SqlParser._

import spray.json._

/**
 * Project IntelliJ IDEA
 * Module structure
 * User: Gyuhyeon
 * Date: 2014. 1. 13.
 * Time: 오후 5:30
 */
case class User (member_srl:Pk[Int],
                 member_name:String,
                 member_password:String,
                 member_salt:String,
                 member_type:String,
                 member_created:Int,
                 member_updated:Int)

object User
{
  val passer = 
  {
    get[Pk[Int]]("member_srl") ~
    get[String]("member_name") ~
    get[String]("member_password") ~
    get[String]("member_salt") ~
    get[String]("member_type") ~
    get[Int]("member_created") ~
    get[Int]("member_updated") map 
      {
        case member_srl ~ member_name ~ member_password ~ member_salt ~ member_type ~ member_created ~ member_updated 
          => User(member_srl, member_name, member_password, member_salt, member_type, member_created, member_updated)
      }
  }
}

object UserFormatter extends DefaultJsonProtocol
{
  implicit object UserFormat extends RootJsonFormat[User]
  {
    def write(u:User) = JsObject(
      "member_srl" -> JsNumber(u.member_srl.get),
      "member_name" -> JsString(u.member_name),
      "member_password" -> JsString(u.member_password),
      "member_salt" -> JsString(u.member_salt),
      "member_type" -> JsString(u.member_type),
      "member_created" -> JsNumber(u.member_created),
      "member_updated" -> JsNumber(u.member_updated)
    )
    
    def read(v:JsValue) = 
    {
      v.asJsObject.getFields("member_srl", "member_name", "member_password", "member_salt", "member_type", "member_created", "member_updated") match
      {
        case Seq(JsNumber(member_srl), JsString(member_name), JsString(member_password), JsString(member_salt), JsString(member_type), JsNumber(member_created), JsNumber(member_updated))
          => new User(new Id(member_srl.toInt), member_name, member_password, member_salt, member_type, member_created.toInt, member_updated.toInt)
      }
    }
  }
}