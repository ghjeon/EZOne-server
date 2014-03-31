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
 * Date: 2014. 1. 8.
 * Time: 오전 1:15
 */
case class Product(product_srl:Pk[Int] = NotAssigned,
                   product_code:String,
                   product_barcode:String,
                   product_name:String,
                   product_size:String,
                   product_purchase_price:Int,
                   product_sale_price:Int,
                   product_stock:Int,
                   product_supplier_srl:Int,
                   product_manufacture_srl:Int,
                   product_created:Int,
                   product_updated:Int)

case class ProductExtend(product_srl:Pk[Int] = NotAssigned,
                         product_code:String,
                         product_barcode:String,
                         product_name:String,
                         product_size:String,
                         product_purchase_price:Int,
                         product_sale_price:Int,
                         product_stock:Int,
                         product_supplier_srl:Int,
                         supplier_name:String,
                         product_manufacture_srl:Int,
                         manufacture_name:String,
                         product_created:Int,
                         product_updated:Int)

object ProductExtend
{
  val parser =
  {
    get[Pk[Int]]("product_srl") ~
    get[String]("product_code") ~
    get[String]("product_barcode") ~
    get[String]("product_name") ~
    get[String]("product_size") ~
    get[Int]("product_purchase_price") ~
    get[Int]("product_sale_price") ~
    get[Int]("product_stock") ~
    get[Int]("product_supplier_srl") ~
    get[String]("supplier_name") ~
    get[Int]("product_manufacture_srl") ~
    get[String]("manufacture_name") ~
    get[Int]("product_created") ~
    get[Int]("product_updated") map {
      case product_srl ~ product_code ~ product_barcode ~ product_name ~ product_size ~ product_purchase_price ~ product_sale_price ~
        product_stock ~ product_supplier_srl ~ supplier_name ~ product_manufacture_srl ~ product_manufacture_name ~ product_created ~ product_updated
        => ProductExtend(product_srl, product_code, product_barcode, product_name, product_size, product_purchase_price, product_sale_price, product_stock,
                         product_supplier_srl, supplier_name, product_manufacture_srl, product_manufacture_name, product_created, product_updated)
    }
  }
}

object Product
{
  val parser =
  {
    get[Pk[Int]]("product_srl") ~
    get[String]("product_code") ~
    get[String]("product_barcode") ~
    get[String]("product_name") ~
    get[String]("product_size") ~
    get[Int]("product_purchase_price") ~
    get[Int]("product_sale_price") ~
    get[Int]("product_stock") ~
    get[Int]("product_supplier_srl") ~
    get[Int]("product_manufacture_srl") ~
    get[Int]("product_created") ~
    get[Int]("product_updated") map {
      case product_srl ~ product_code ~ product_barcode ~ product_name ~ product_size ~ product_purchase_price ~ product_sale_price ~
        product_stock ~ product_supplier_srl ~ product_manufacture_srl ~ product_created ~ product_updated
        => Product(product_srl, product_code, product_barcode, product_name, product_size, product_purchase_price, product_sale_price,
                   product_stock, product_supplier_srl, product_manufacture_srl, product_created, product_updated)
    }
  }

  def findAll(page:Int, count:Int, orderBy:String, orderType:String) = DB.withConnection
  {
    implicit connection =>
      //try
      //{
        SQL("SELECT * from product_extend order by {orderBy} " + util.db.validateOrderType(orderType) + " limit {page}, {count}")
          .on("orderBy"->toParameterValue("product_" + orderBy),
          "page"->util.db.getPageIndex(page, count),
          "count"->count).as(ProductExtend.parser *)
      //} catch {
      //  case e => null
      //}
  }

  def findByOption(target:String, keyword:String, option:String):List[ProductExtend] = DB.withConnection
  {
    implicit connection =>
      try
      {
        val keywordType:String = target match {
          case "srl" => "Int"
          case "created" => "Int"
          case "updated" => "Int"
          case _ => "String"
        }

        val query = SQL("SELECT * from product_extend where " + target + " " + option + " {keyword}")
        if(keywordType == "String")
          query.on("keyword"->keyword).as(ProductExtend.parser *)
        else if(keywordType == "Int")
          query.on("keyword"->keyword.toInt).as(ProductExtend.parser *)
        else
          null
      } catch {
        case e => null
      }
  }

  def findByKeyword(keyword:String):List[ProductExtend] = DB.withConnection
  {
    implicit connection =>
      try
      {
        val query = SQL("SELECT * FROM product_extend WHERE product_code LIKE {keyword} OR " +
                                                           "product_barcode LIKE {keyword} OR " +
                                                           "product_name LIKE {keyword} OR " +
                                                           "supplier_name LIKE {keyword};")
        query.on("keyword"->keyword).as(ProductExtend.parser *)
      } catch {
        case e => null
      }
  }

  def findLastCode(id:Int):Product = DB.withConnection
  {
    implicit connection =>
      try
      {
        SQL("SELECT * from product where product_supplier_srl = {srl} order by product_code desc limit 1")
          .on("srl"->id)
          .using(this.parser).single()
      } catch {
        case e => null
      }
  }

  def findDuplicate(name:String, size:String, supplier_srl:Int):Product = DB.withConnection
  {
    implicit connection =>
      try
      {
        SQL("SELECT * FROM product WHERE product_name = {name} AND product_size = {size} AND product_supplier_srl = {supplier} LIMIT 1")
          .on("name"->name,
              "size"->size,
              "supplier"->supplier_srl)
          .using(this.parser).single()
      } catch {
        case e=> null
      }
  }

  def findById(id:Pk[Int]):ProductExtend = DB.withConnection
  {
    implicit connection =>
      try
      {
        SQL("SELECT * from product_extend where product_srl = {srl};")
          .on("srl"->id.get).using(ProductExtend.parser).single()
      } catch {
        case e=> null
      }
  }

  def create(p:Product):ProductExtend = DB.withConnection
  {
    implicit connection =>
      val insertRow = SQL("INSERT INTO product(product_code, product_barcode, product_name, product_size, product_purchase_price, product_sale_price, product_stock, product_supplier_srl, product_manufacture_srl, product_created, product_updated) " +
        "values ({code}, {barcode}, {name}, {size}, {purchase_price}, {sale_price}, {stock}, {supplier_srl}, {manufacture_srl}, {created}, {updated}); ")
        .on("code"->p.product_code,
            "barcode"->p.product_barcode,
            "name"->p.product_name,
            "size"->p.product_size,
            "purchase_price"->p.product_purchase_price,
            "sale_price"->p.product_sale_price,
            "stock"->p.product_stock,
            "supplier_srl"->p.product_supplier_srl,
            "manufacture_srl"->p.product_manufacture_srl,
            "created"->p.product_created,
            "updated"->p.product_updated).executeInsert(scalar[Long].single).toInt

      findById(new Id(insertRow))
  }

  def update(p:Product):ProductExtend = DB.withConnection
  {
    implicit connection =>
      val updateRow = SQL("UPDATE product set " +
                          "product_name = {name}, " +
                          "product_barcode = {barcode}, " +
                          "product_size = {size}, " +
                          "product_purchase_price = {purchase_price}, " +
                          "product_sale_price = {sale_price}, " +
                          "product_stock = {stock}, " +
                          "product_supplier_srl = {supplier_srl}, " +
                          "product_manufacture_srl = {manufacture_srl}, " +
                          "product_updated = {updated} " +
                          "where product_srl = {srl} AND product_code = {code};")
      .on("name"->p.product_name,
          "barcode"->p.product_barcode,
          "size"->p.product_size,
          "purchase_price"->p.product_purchase_price,
          "sale_price"->p.product_sale_price,
          "stock"->p.product_stock,
          "supplier_srl"->p.product_supplier_srl,
          "manufacture_srl"->p.product_manufacture_srl,
          "updated"->p.product_updated,
          "srl"->p.product_srl.get,
          "code"->p.product_code).executeUpdate()

      findById(p.product_srl)
  }
}

object ProductFormatter extends DefaultJsonProtocol
{
  implicit object ProductJsonFormat extends RootJsonFormat[Product]
  {
    def write(p:Product) = JsObject(
      "product_srl" -> JsNumber(p.product_srl.get),
      "product_code" -> JsString(p.product_code),
      "product_barcode" -> JsString(p.product_barcode),
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
      v.asJsObject.getFields("product_srl", "product_code", "product_barcode", "product_name", "product_size", "product_purchase_price", "product_sale_price", "product_stock", "product_supplier_srl", "product_manufacture_srl", "product_created", "product_updated") match {
        case Seq(JsNumber(product_srl), JsString(product_code), JsString(product_barcode), JsString(product_name), JsString(product_size), JsNumber(product_purchase_price), JsNumber(product_sale_price), JsNumber(product_stock), JsNumber(product_supplier_srl), JsNumber(product_manufacture_srl), JsNumber(product_created), JsNumber(product_updated))
          => new Product(new Id(product_srl.toInt), product_code, product_barcode, product_name, product_size, product_purchase_price.toInt, product_sale_price.toInt, product_stock.toInt, product_supplier_srl.toInt, product_manufacture_srl.toInt, product_created.toInt, product_updated.toInt)
      }
    }
  }

  implicit object ProductExtendJsonFormat extends RootJsonFormat[ProductExtend]
  {
    def write(p:ProductExtend) = JsObject(
      "product_srl" -> JsNumber(p.product_srl.get),
      "product_code" -> JsString(p.product_code),
      "product_barcode" -> JsString(p.product_barcode),
      "product_name" -> JsString(p.product_name),
      "product_size" -> JsString(p.product_size),
      "product_purchase_price" -> JsNumber(p.product_purchase_price),
      "product_sale_price" -> JsNumber(p.product_sale_price),
      "product_stock" -> JsNumber(p.product_stock),
      "product_supplier_srl" -> JsNumber(p.product_supplier_srl),
      "supplier_name" -> JsString(p.supplier_name),
      "product_manufacture_srl" -> JsNumber(p.product_manufacture_srl),
      "manufacture_name" -> JsString(p.manufacture_name),
      "product_created" -> JsNumber(p.product_created),
      "product_updated" -> JsNumber(p.product_updated)
    )
    def read(v:JsValue) =
    {
      v.asJsObject.getFields("product_srl", "product_code", "product_barcode", "product_name", "product_size", "product_purchase_price", "product_sale_price", "product_stock", "product_supplier_srl", "supplier_name", "product_manufacture_srl", "manufacture_name", "product_created", "product_updated") match {
        case Seq(JsNumber(product_srl), JsString(product_code), JsString(product_barcode), JsString(product_name), JsString(product_size), JsNumber(product_purchase_price), JsNumber(product_sale_price), JsNumber(product_stock), JsNumber(product_supplier_srl), JsString(supplier_name), JsNumber(product_manufacture_srl), JsString(manufacture_name), JsNumber(product_created), JsNumber(product_updated))
        => new ProductExtend(new Id(product_srl.toInt), product_code, product_barcode, product_name, product_size, product_purchase_price.toInt, product_sale_price.toInt, product_stock.toInt, product_supplier_srl.toInt, supplier_name, product_manufacture_srl.toInt, manufacture_name, product_created.toInt, product_updated.toInt)
      }
    }
  }
}