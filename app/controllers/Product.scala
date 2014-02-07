package controllers

import play.api._
import play.api.mvc._

import play.api.libs.json.Json

import anorm._
import spray.json._

import structure.ProductFormatter._
import util.time._

/**
 * Project IntelliJ IDEA
 * Module controllers
 * User: Gyuhyeon
 * Date: 2014. 2. 6.
 * Time: 오전 2:13
 */
object Product extends Controller {

  def create() = Action(parse.urlFormEncoded)
  {
    request =>
      val body:Map[String, Seq[String]] = request.body

      val product_code = body.getOrElse("code", util.dummy.dummyList)(0)
      val product_name = body.getOrElse("name", util.dummy.dummyList)(0)
      val product_size:String = body.getOrElse("size", util.dummy.dummyList)(0)
      val product_purchase_price = body.getOrElse("purchase_price", util.dummy.dummyListInt)(0).toString.toInt
      val product_sale_price = body.getOrElse("sale_price", util.dummy.dummyListInt)(0).toString.toInt
      val product_stock = body.getOrElse("mobile", util.dummy.dummyListInt)(0).toString.toInt
      val product_supplier_srl = body.getOrElse("visiting", util.dummy.dummyListInt)(0).toString.toInt
      val product_manufacture_srl = body.getOrElse("visiting", util.dummy.dummyListInt)(0).toString.toInt
      val product_created = timestamp
      val product_updated = timestamp

      val product = structure.Product(NotAssigned,
        product_code,
        product_name,
        product_size,
        product_purchase_price,
        product_sale_price,
        product_stock,
        product_supplier_srl,
        product_manufacture_srl,
        product_created,
        product_updated)

      val dbResult = structure.Product.create(product)

      if(dbResult != null)
        Ok(Json.obj("result"->"OK", "code"->"200", "data"->dbResult.toJson.toString))
      else
        Ok(Json.obj("result"->"Fail", "code"->"410", "message"->"DATABASE_EXECUTION_EXCEPTION"))
  }

  def modify(id:Int) = TODO

  def delete(id:Int) = TODO

  def get(id:Int) = Action
  {
    request =>
      val dbResult = structure.Product.findById(new Id(id))

      if(dbResult != null)
        Ok(Json.obj("result"->"OK", "code"->"200", "data"->dbResult.toJson.toString))
      else
        Ok(Json.obj("result"->"Fail", "code"->"404", "message"->"NOT_FOUND"))
  }

  def list(page:Int, count:Int, orderBy:String, orderType:String) = Action
  {
    request =>
      val dbResult = structure.Product.findAll(page, count, orderBy, orderType)
      if(dbResult != null)
        Ok(Json.obj("result"->"OK", "code"->"200", "data"->dbResult.toJson.toString))
      else
        Ok(Json.obj("result"->"Fail", "code"->"404", "message"->"NOT_FOUND"))
  }

  def find(keyword:String, option:String) = TODO

}
