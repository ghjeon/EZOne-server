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
case class Warehouse (warehouse_srl:Pk[Int],
                  warehouse_supplier_srl:Int,
                  warehouse_due_date:Int,
                  warehouse_amount:Int,
                  warehouse_bill:String,
                  warehouse_stored:String,
                  warehouse_products:String,
                  warehouse_created:Int,
                  warehouse_updated:Int)

object Warehouse
{
  val parser =
  {
    get[Pk[Int]]("warehouse_srl") ~
    get[Int]("warehouse_supplier_srl") ~
    get[Int]("warehouse_due_date") ~
    get[Int]("warehouse_amount") ~
    get[String]("warehouse_bill") ~
    get[String]("warehouse_stored") ~
    get[String]("warehouse_products") ~
    get[Int]("warehouse_created") ~
    get[Int]("warehouse_updated") map
      {
        case warehouse_srl ~ warehouse_supplier_srl ~ warehouse_date ~ warehouse_amount ~ warehouse_bill ~ warehouse_stored ~ warehouse_products ~ warehouse_created ~ warehouse_updated
          => Warehouse(warehouse_srl, warehouse_supplier_srl, warehouse_date, warehouse_amount, warehouse_bill, warehouse_stored, warehouse_products, warehouse_created, warehouse_updated)
      }
  }

  def findById(id:Pk[Int]):Warehouse = DB.withConnection
  {
    implicit connection =>
      try
      {
        SQL("SELECT * FROM warehouse " +
          "WHERE warehouse_srl = {srl}")
          .on("srl"->id.get)
          .using(this.parser).single()
      } catch {
        case e=> null
      }
  }

  def findBySupplier(supplier_srl:Int, start:Int, end:Int, orderBy:String, orderType:String):List[Warehouse] = DB.withConnection
  {
    implicit connection =>
      try
      {
        SQL("SELECT * FROM warehouse " +
          "WHERE warehouse_supplier_srl = {srl} AND " +
                "warehouse_updated >= {start} AND " +
                "warehouse_updated <= {end} " +
          "ORDER BY {orderBy} " + util.db.validateOrderType(orderType) + ";")
        .on("srl"->supplier_srl,
            "start"->start,
            "end"->end,
            "orderBy"->orderBy)
        .as(this.parser *)
      } catch
      {
        case e => null
      }
  }

  def findByDate(start:Int, end:Int, orderBy:String, orderType:String):List[Warehouse] = DB.withConnection
  {
    implicit connection =>
      try
      {
        SQL("SELECT * FROM warehouse " +
          "WHERE warehouse_updated >= {start} AND " +
                "warehouse_updated <= {end} " +
                "ORDER BY {orderBy} " + util.db.validateOrderType(orderType) + ";")
        .on("start"->start,
            "end"->end,
            "orderBy"->orderBy)
        .as(this.parser *)
      } catch {
        case e=> null
      }
  }

  def findByProduct(keyword:String, start:Int, end:Int):List[Warehouse] = DB.withConnection
  {
    implicit connection =>
      try
      {
        val query = SQL("SELECT * FROM warehouse " +
          "WHERE warehouse_products like {keyword} AND " +
                "warehouse_updated >= {start} AND " +
                "warehouse_updated <= {end};")
        query.on("keyword"->keyword).as(this.parser *)
      } catch {
        case e => null
      }
  }

  def findByOption(target:String, keyword:String, option:String):List[Warehouse] = DB.withConnection
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

        val query = SQL("SELECT * FROM warehouse WHERE " + target + " " + option + " {keyword}")
        if(keywordType == "String")
          query.on("keyword"->keyword).as(this.parser *)
        else if(keywordType == "Int")
          query.on("keyword"->keyword.toInt).as(this.parser *)
        else
          null
      } catch {
        case e => null
      }
  }

  def create(w:Warehouse):Warehouse = DB.withConnection
  {
    implicit connection =>
      val insertRow = SQL("INSERT INTO warehouse(warehouse_supplier_srl, warehouse_due_date, warehouse_amount, warehouse_bill, " +
                                "warehouse_stored, warehouse_products, warehouse_created, warehouse_updated) " +
          "VALUES({supplier}, {date}, {amount}, {bill}, {stored}, {products}, {created}, {updated});")
        .executeInsert(scalar[Long].single).toInt

      findById(new Id(insertRow))
  }

  def update(w:Warehouse):Warehouse = DB.withConnection
  {
    implicit connection =>
      val updateRow = SQL("UPDATE warehouse SET " +
                            "warehouse_due_date = {date}, " +
                            "warehouse_amount = {amount}, " +
                            "warehouse_bill = {bill}, " +
                            "warehouse_stored = {stored}, " +
                            "warehouse_products = {products}, " +
                            "warehouse_updated = {updated}" +
                          "WHERE warehouse_srl = {srl};")
        .on("date"->w.warehouse_due_date,
            "amount"->w.warehouse_amount,
            "bill"->w.warehouse_bill,
            "stored"->w.warehouse_stored,
            "products"->w.warehouse_products,
            "updated"->w.warehouse_updated)
        .executeUpdate()

      findById(w.warehouse_srl)
  }

  def delete(id:Pk[Int]):Warehouse = DB.withConnection
  {
    implicit connection =>
      val row = findById(id)
      SQL("DELETE FROM warehouse " +
        "WHERE warehouse_srl = {srl};")
        .on("srl"->id.get)
        .execute()

      row
  }
}

object WarehouseFormatter extends DefaultJsonProtocol
{
  implicit object warehouseFormat extends RootJsonFormat[Warehouse]
  {
    def write(o:Warehouse) = JsObject(
      "warehouse_srl" -> JsNumber(o.warehouse_srl.get),
      "warehouse_supplier_srl" -> JsNumber(o.warehouse_supplier_srl),
      "warehouse_due_date" -> JsNumber(o.warehouse_due_date),
      "warehouse_amount" -> JsNumber(o.warehouse_amount),
      "warehouse_bill" -> JsString(o.warehouse_bill),
      "warehouse_stored" -> JsString(o.warehouse_stored),
      "warehouse_products" -> JsString(o.warehouse_products),
      "warehouse_created" -> JsNumber(o.warehouse_created),
      "warehouse_updated" -> JsNumber(o.warehouse_updated)
    )

    def read(v:JsValue) =
    {
      v.asJsObject.getFields("warehouse_srl", "warehouse_supplier_srl", "warehouse_date", "warehouse_amount", "warehouse_bill", "warehouse_stored", "warehouse_products", "warehouse_created", "warehouse_updated") match {
        case Seq(JsNumber(warehouse_srl), JsNumber(warehouse_supplier_srl), JsNumber(warehouse_date), JsNumber(warehouse_amount), JsString(warehouse_bill), JsString(warehouse_stored), JsString(warehouse_products), JsNumber(warehouse_created), JsNumber(warehouse_updated))
        => new Warehouse(new Id(warehouse_srl.toInt), warehouse_supplier_srl.toInt, warehouse_date.toInt, warehouse_amount.toInt, warehouse_bill, warehouse_stored, warehouse_products, warehouse_created.toInt, warehouse_updated.toInt)
      }
    }
  }
}