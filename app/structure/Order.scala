package structure

import play.api.db.DB
import play.api.Play.current
import anorm._
import anorm.SqlParser._

import spray.json._
import util.db._

/**
 * Project IntelliJ IDEA
 * Module structure
 * User: Gyuhyeon
 * Date: 2014. 1. 13.
 * Time: 오후 5:37
 */
case class Order (order_srl:Pk[Int],
                  order_supplier:Int,
                  order_date:Int,
                  order_amount:Int,
                  order_bill:String,
                  order_products:String,
                  order_created:Int,
                  order_updated:Int)

object Order
{
  val parser =
  {
    get[Pk[Int]]("order_srl") ~
    get[Int]("order_supplier") ~
    get[Int]("order_date") ~
    get[Int]("order_amount") ~
    get[String]("order_bill") ~
    get[String]("order_products") ~
    get[Int]("order_created") ~
    get[Int]("order_updated") map
      {
        case order_srl ~ order_supplier ~ order_date ~ order_amount ~ order_bill ~ order_products ~ order_created ~ order_updated
          => Order(order_srl, order_supplier, order_date, order_amount, order_bill, order_products, order_created, order_updated)
      }
  }
}

object OrderFormatter extends DefaultJsonProtocol
{
  implicit object OrderFormat extends RootJsonFormat[Order]
  {
    def write(o:Order) = JsObject(
      "order_srl" -> JsNumber(o.order_srl.get),
      "order_supplier" -> JsNumber(o.order_supplier),
      "order_date" -> JsNumber(o.order_date),
      "order_amount" -> JsNumber(o.order_amount),
      "order_bill" -> JsString(o.order_bill),
      "order_products" -> JsString(o.order_products),
      "order_created" -> JsNumber(o.order_created),
      "order_updated" -> JsNumber(o.order_updated)
    )

    def read(v:JsValue) =
    {
      v.asJsObject.getFields("order_srl", "order_supplier", "order_date", "order_amount", "order_bill", "order_products", "order_created", "order_updated") match {
        case Seq(JsNumber(order_srl), JsNumber(order_supplier), JsNumber(order_date), JsNumber(order_amount), JsString(order_bill), JsString(order_products), JsNumber(order_created), JsNumber(order_updated))
        => new Order(new Id(order_srl.toInt), order_supplier.toInt, order_date.toInt, order_amount.toInt, order_bill, order_products, order_created.toInt, order_updated.toInt)
      }
    }
  }
}