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
 * Time: 오전 1:15
 */
case class Product(product_srl:Pk[Int] = NotAssigned,
                   product_code:String,
                   product_name:String,
                   product_size:String,
                   product_purchase_price:Int,
                   product_sale_price:Int,
                   product_stock:Int,
                   product_supplier_srl:Int,
                   product_manufacture_srl:Int,
                   product_created:Int,
                   product_updated:Int)

object Product
{
  val parser =
  {
    get[Pk[Int]]("product_srl") ~
    get[String]("product_code") ~
    get[String]("product_name") ~
    get[String]("product_size") ~
    get[Int]("product_purchase_price") ~
    get[Int]("product_sale_price") ~
    get[Int]("product_stock") ~
    get[Int]("product_supplier_srl") ~
    get[Int]("product_manufacture_srl") ~
    get[Int]("product_created") ~
    get[Int]("product_updated") map {
      case product_srl ~ product_code ~ product_name ~ product_size ~ product_purchase_price ~ product_sale_price ~ product_stock ~ product_supplier_srl ~ product_manufacture_srl ~ product_created ~ product_updated 
        => Product(product_srl, product_code, product_name, product_size, product_purchase_price, product_sale_price, product_stock, product_supplier_srl, product_manufacture_srl, product_created, product_updated)
    }
  }
}

object ProductFormatter extends DefaultJsonProtocol
{
  implicit object ProductJsonFormat extends RootJsonFormat[Product]
  {
    def write(p:Product) = JsObject(
      "product_srl" -> JsNumber(p.product_srl.get),
      "product_code" -> JsString(p.product_code),
      "product_name" -> JsString(p.product_name),
      "product_size" -> JsString(p.product_size),
      "product_purchase_price" -> JsNumber(p.product_purchase_price),
      "product_sale_price" -> JsNumber(p.product_sale_price),
      "product_stock" -> JsNumber(p.product_stock),
      "product_supplier_srl" -> JsNumber(p.product_supplier_srl),
      "product_manufacture_srl" -> JsNumber(p.product_manufacture_srl),
      "product_created" -> JsNumber(p.product_created),
      "product_updated" -> JsNumber(p.product_updated)
    )
    def read(v:JsValue) =
    {
      v.asJsObject.getFields("product_srl", "product_code", "product_name", "product_size", "product_purchase_price", "product_sale_price", "product_stock", "product_supplier_srl", "product_manufacture_srl", "product_created", "product_updated") match {
        case Seq(JsNumber(product_srl), JsString(product_code), JsString(product_name), JsString(product_size), JsNumber(product_purchase_price), JsNumber(product_sale_price), JsNumber(product_stock), JsNumber(product_supplier_srl), JsNumber(product_manufacture_srl), JsNumber(product_created), JsNumber(product_updated))
          => new Product(new Id(product_srl.toInt), product_code, product_name, product_size, product_purchase_price.toInt, product_sale_price.toInt, product_stock.toInt, product_supplier_srl.toInt, product_manufacture_srl.toInt, product_created.toInt, product_updated.toInt)
      }
    }
  }
}