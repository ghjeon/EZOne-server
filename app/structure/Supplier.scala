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
 * Time: 오후 5:20
 */
case class Supplier (supplier_srl:Pk[Int],
                     supplier_name:String,
                     supplier_phone:String,
                     supplier_address:String,
                     supplier_charger:String,
                     supplier_mobile:String,
                     supplier_visiting:String,
                     supplier_created:Int,
                     supplier_updated:Int)

object Supplier
{
  val parser =
  {
    get[Pk[Int]]("supplier_srl") ~
    get[String]("supplier_name") ~
    get[String]("supplier_phone") ~
    get[String]("supplier_address") ~
    get[String]("supplier_charger") ~
    get[String]("supplier_mobile") ~
    get[String]("supplier_visiting") ~
    get[Int]("supplier_created") ~
    get[Int]("supplier_updated") map
      {
        case supplier_srl ~ supplier_name ~ supplier_phone ~ supplier_address ~ supplier_charger ~ supplier_mobile ~ supplier_visiting ~ supplier_created ~ supplier_updated
          => Supplier(supplier_srl, supplier_name, supplier_phone, supplier_address, supplier_charger, supplier_mobile, supplier_visiting, supplier_created, supplier_updated)
      }
  }
}

object SupplierFormatter extends DefaultJsonProtocol
{
  implicit object SupplierFormat extends RootJsonFormat[Supplier]
  {
    def write(s:Supplier) = JsObject(
      "supplier_srl" -> JsNumber(s.supplier_srl.get),
      "supplier_name" -> JsString(s.supplier_name),
      "supplier_phone" -> JsString(s.supplier_phone),
      "supplier_address" -> JsString(s.supplier_address),
      "supplier_charger" -> JsString(s.supplier_charger),
      "supplier_mobile" -> JsString(s.supplier_mobile),
      "supplier_visiting" -> JsString(s.supplier_visiting),
      "supplier_created" -> JsNumber(s.supplier_created),
      "supplier_updated" -> JsNumber(s.supplier_updated)
    )

    def read(v:JsValue) =
    {
      v.asJsObject.getFields("supplier_srl", "supplier_name", "supplier_phone", "supplier_address", "supplier_charger", "supplier_mobile", "supplier_visiting", "supplier_created", "supplier_updated") match
      {
        case Seq(JsNumber(supplier_srl), JsString(supplier_name), JsString(supplier_phone), JsString(supplier_address), JsString(supplier_charger), JsString(supplier_mobile), JsString(supplier_visiting), JsNumber(supplier_created), JsNumber(supplier_updated))
          => new Supplier(new Id(supplier_srl.toInt), supplier_name, supplier_phone, supplier_address, supplier_charger, supplier_mobile, supplier_visiting, supplier_created.toInt, supplier_updated.toInt)
      }
    }
  }
}