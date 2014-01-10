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
 * Date: 2014. 1. 8.
 * Time: 오전 1:05
 */

case class Manufacture (manufacture_srl:Pk[Int] = NotAssigned,
                        manufacture_name:String,
                        manufacture_type:String,
                        manufacture_address:String,
                        manufacture_phone:String,
                        manufacture_charger:String,
                        manufacture_mobile:String,
                        manufacture_created:Int,
                        manufacture_updated:Int)

object Manufacture 
{
  val parser = 
  {
    get[Pk[Int]]("manufacture_srl") ~
    get[String]("manufacture_name") ~
    get[String]("manufacture_type") ~
    get[String]("manufacture_address") ~
    get[String]("manufacture_phone") ~
    get[String]("manufacture_charger") ~
    get[String]("manufacture_mobile") ~
    get[Int]("manufacture_created") ~
    get[Int]("manufacture_updated") map {
      case manufacture_srl ~ manufacture_name ~ manufacture_type ~ manufacture_address ~ manufacture_phone ~ manufacture_charger ~ manufacture_mobile ~ manufacture_created ~ manufacture_updated 
        => Manufacture(manufacture_srl, manufacture_name, manufacture_type, manufacture_address, manufacture_phone, manufacture_charger, manufacture_mobile, manufacture_created, manufacture_updated)
    }
  }
}

object ManufactureFormatter extends DefaultJsonProtocol
{
  implicit object ManufactureJsonFormat extends RootJsonFormat[Manufacture]
  {
    def write(m:Manufacture) = JsObject(
      "manufacture_srl" -> JsNumber(m.manufacture_srl.get),
      "manufacture_name" -> JsString(m.manufacture_name),
      "manufacture_type" -> JsString(m.manufacture_type),
      "manufacture_address" -> JsString(m.manufacture_address),
      "manufacture_phone" -> JsString(m.manufacture_phone),
      "manufacture_charger" -> JsString(m.manufacture_charger),
      "manufacture_mobile" -> JsString(m.manufacture_mobile),
      "manufacture_created" -> JsNumber(m.manufacture_created),
      "manufacture_updated" -> JsNumber(m.manufacture_updated)
    )

    def read(v:JsValue) =
    {
      v.asJsObject.getFields("manufacture_srl", "manufacture_name", "manufacture_type", "manufacture_address", "manufacture_phone", "manufacture_charger", "manufacture_mobile", "manufacture_created", "manufacture_updated") match {
        case Seq(JsNumber(manufacture_srl), JsString(manufacture_name), JsString(manufacture_type), JsString(manufacture_address), JsString(manufacture_phone), JsString(manufacture_charger), JsString(manufacture_mobile), JsNumber(manufacture_created), JsNumber(manufacture_updated))
          => new Manufacture(new Id(manufacture_srl.toInt), manufacture_name, manufacture_type, manufacture_address, manufacture_phone, manufacture_charger, manufacture_mobile, manufacture_created.toInt, manufacture_updated.toInt)
      }
    }
  }
}