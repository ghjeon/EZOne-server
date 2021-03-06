package controllers

import play.api._
import play.api.mvc._

import play.api.libs.json.Json

import anorm._
import spray.json._

import structure.WarehouseFormatter._
import util.time._
import java.io.FileInputStream
import java.nio.channels.FileChannel

/**
 * Project IntelliJ IDEA
 * Module controllers
 * User: Gyuhyeon
 * Date: 2014. 2. 6.
 * Time: 오전 2:14
 */
object Warehouse extends Controller {

  def create() = Action(parse.multipartFormData)
  {
    request =>

      val body = request.body.dataParts

      val warehouse_supplier_srl = body.getOrElse("supplier", util.dummy.dummyListInt)(0).toString.toInt
      val warehouse_due_date = body.getOrElse("due_date", util.dummy.dummyListInt)(0).toString.toInt
      val warehouse_amount = body.getOrElse("amount", util.dummy.dummyListInt)(0).toString.toInt
      val warehouse_bill = body.getOrElse("bill", util.dummy.dummyList)(0)
      val warehouse_stored = body.getOrElse("stored", util.dummy.dummyList)(0)

      //val warehouse_products = body.getOrElse("products", util.dummy.dummyList)(0)
      //val warehouse_products = request.headers.get("products").toString
      val warehouse_products = request.body.file("products") map {
        data =>
         val src = scala.io.Source.fromFile(data.ref.file, "UTF-8")
         val str = src.mkString
         src.close()
         str
      }
      val warehouse_created = timestamp
      val warehouse_updated = timestamp

      val warehouse = structure.Warehouse(NotAssigned,
          warehouse_supplier_srl,
          warehouse_due_date,
          warehouse_amount,
          warehouse_bill,
          warehouse_stored,
          warehouse_products.get,
          warehouse_created,
          warehouse_updated)

      val dbResult = structure.Warehouse.create(warehouse)

      if(dbResult != null)
        Ok(Json.obj("result"->"OK", "code"->"200", "data"->dbResult.toJson.toString))
      else
        Ok(Json.obj("result"->"Fail", "code"->"410", "message"->"DATABASE_EXECUTION_EXCEPTION"))
  }

  def modify(id:Int) =  Action(parse.multipartFormData)
  {
    request =>

      val body = request.body.dataParts

      val warehouse_srl = id
      val warehouse_supplier_srl = body.getOrElse("supplier", util.dummy.dummyListInt)(0).toString.toInt
      val warehouse_due_date = body.getOrElse("due_date", util.dummy.dummyListInt)(0).toString.toInt
      val warehouse_amount = body.getOrElse("amount", util.dummy.dummyListInt)(0).toString.toInt
      val warehouse_bill = body.getOrElse("bill", util.dummy.dummyList)(0)
      val warehouse_stored = body.getOrElse("stored", util.dummy.dummyList)(0)
      val warehouse_products = request.body.file("products") map {
        data =>
          val src = scala.io.Source.fromFile(data.ref.file, "UTF-8")
          val str = src.mkString
          src.close()
          str
      }
      val warehouse_created = timestamp
      val warehouse_updated = timestamp

      val warehouse = structure.Warehouse(new Id(warehouse_srl),
        warehouse_supplier_srl,
        warehouse_due_date,
        warehouse_amount,
        warehouse_bill,
        warehouse_stored,
        warehouse_products.get,
        warehouse_created,
        warehouse_updated)

      val dbResult = structure.Warehouse.update(warehouse)

      if(dbResult != null)
        Ok(Json.obj("result"->"OK", "code"->"200", "data"->dbResult.toJson.toString))
      else
        Ok(Json.obj("result"->"Fail", "code"->"410", "message"->"DATABASE_EXECUTION_EXCEPTION"))
  }

  def delete(id:Int) = Action
  {
    request =>
      val dbResult = structure.Warehouse.delete(new Id(id))

      if(dbResult != null)
        Ok(Json.obj("result"->"OK", "code"->"200", "data"->dbResult.toJson.toString))
      else
        Ok(Json.obj("result"->"Fail", "code"->"404", "message"->"NOT_FOUND"))
  }

  def get(id:Int) = Action
  {
    request =>
      val dbResult = structure.Warehouse.findById(new Id(id))

      if(dbResult != null)
        Ok(Json.obj("result"->"OK", "code"->"200", "data"->dbResult.toJson.toString))
      else
        Ok(Json.obj("result"->"Fail", "code"->"404", "message"->"NOT_FOUND"))
  }

  def list(start:Int, end:Int, orderBy:String, orderType:String) = Action
  {
    request =>
      val dbResult = structure.Warehouse.findByDate(start, end, orderBy, orderType)
      if(dbResult != null)
        Ok(Json.obj("result"->"OK", "code"->"200", "data"->dbResult.toJson.toString))
      else
        Ok(Json.obj("result"->"Fail", "code"->"404", "message"->"NOT_FOUND"))
  }

  def findBySupplier(supplier_srl:Int, start:Int, end:Int, orderBy:String, orderType:String) = Action
  {
    request =>
      val dbResult = structure.Warehouse.findBySupplier(supplier_srl, start, end, orderBy, orderType)
      if(dbResult != null)
        Ok(Json.obj("result"->"OK", "code"->"200", "data"->dbResult.toJson.toString))
      else
        Ok(Json.obj("result"->"Fail", "code"->"404", "message"->"NOT_FOUND"))
  }

  def find(target:String, keyword:String, option:String, start:Int, end:Int, orderBy:String, orderType:String) = Action
  {
    request =>
      var keywordEscape:String = ""
      val optionEscape:String = option match
      {
        case "=" => "="
        case ">=" => ">="
        case "<=" => "<="
        case "like" => "like"
        case _ => "="
      }
      if(optionEscape == "like")
        keywordEscape = "%" + keyword + "%"
      else keywordEscape = keyword
      val dbResult = structure.Warehouse.findByOption("warehouse_" + target, keywordEscape, optionEscape, orderBy, orderType)
      if(dbResult != null)
        Ok(Json.obj("result"->"OK", "code"->"200", "data"->dbResult.toJson.toString))
      else
        Ok(Json.obj("result"->"Fail", "code"->"404", "message"->"NOT_FOUND"))
  }

}
