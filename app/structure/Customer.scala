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
 * Time: 오전 12:51
 */

case class Customer (customer_srl:Pk[Int] = NotAssigned,
                     customer_code:String,
                     customer_name:String,
                     customer_reg_no:String,
                     customer_phone:String,
                     customer_birthday:Int,
                     customer_address:String,
                     customer_created:Int,
                     customer_updated:Int)

object Customer
{
  val parser = 
  {
    get[Pk[Int]]("customer_srl") ~
    get[String]("customer_code") ~
    get[String]("customer_name") ~
    get[String]("customer_reg_no") ~
    get[String]("customer_phone") ~ 
    get[Int]("customer_birthday") ~
    get[String]("customer_address") ~
    get[Int]("customer_created") ~ 
    get[Int]("customer_updated") map 
    {
      case customer_srl ~ customer_code ~ customer_name ~ customer_reg_no ~ customer_phone ~ customer_birthday ~ customer_address ~ customer_created ~ customer_updated 
        => Customer(customer_srl, customer_code, customer_name, customer_reg_no, customer_phone, customer_birthday, customer_address, customer_created, customer_updated)
    }
  }
}

object CustomerFormatter extends DefaultJsonProtocol
{
  implicit object CustomerJsonFormat extends RootJsonFormat[Customer]
  {
    def write(c:Customer) = JsObject(
      "customer_srl" -> JsNumber(c.customer_srl.get),
      "customer_code" -> JsString(c.customer_code),
      "customer_name" -> JsString(c.customer_name),
      "customer_reg_no" -> JsString(c.customer_reg_no),
      "customer_phone" -> JsString(c.customer_phone),
      "customer_birthday" -> JsNumber(c.customer_birthday),
      "customer_address" -> JsString(c.customer_address),
      "customer_created" -> JsNumber(c.customer_created),
      "customer_updated" -> JsNumber(c.customer_updated)
    )

    def read(v:JsValue) =
    {
      v.asJsObject.getFields("customer_srl", "customer_code", "customer_name", "customer_reg_no", "customer_phone", "customer_birthday", "customer_address", "customer_created", "customer_updated") match {
        case Seq(JsNumber(customer_srl), JsString(customer_code), JsString(customer_name), JsString(customer_reg_no), JsString(customer_phone), JsNumber(customer_birthday), JsString(customer_address), JsNumber(customer_created), JsNumber(customer_updated))
          => new Customer(new Id(customer_srl.toInt), customer_code, customer_name, customer_reg_no, customer_phone, customer_birthday.toInt, customer_address, customer_created.toInt, customer_updated.toInt)
      }
    }
  }

}