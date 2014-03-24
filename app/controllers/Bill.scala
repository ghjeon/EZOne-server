package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json.Json

import anorm._
import spray.json._

import structure.BillFormatter._
import util.time._

/**
 * Project IntelliJ IDEA
 * Module controllers
 * User: Gyuhyeon
 * Date: 2014. 2. 6.
 * Time: 오전 2:15
 */
object Bill extends Controller {
  def create() = Action(parse.urlFormEncoded)
  {
    request =>
      val body:Map[String, Seq[String]] = request.body

      val bill_customer_srl:Int = body.getOrElse("customer_srl", util.dummy.dummyListInt)(0).toString.toInt
      val bill_member_srl:Int = body.getOrElse("member_srl", util.dummy.dummyListInt)(0).toString.toInt
      val bill_info:String = body.getOrElse("info", util.dummy.dummyList)(0)
      val bill_type = body.getOrElse("type", util.dummy.dummyList)(0)
      val bill_due = body.getOrElse("due", util.dummy.dummyListInt)(0).toString.toInt
      val bill_isTaxReceipt = body.getOrElse("isTaxReceipt", List("N"))(0)
      val bill_amount = body.getOrElse("amount", util.dummy.dummyListInt)(0).toString.toInt
      val bill_created = timestamp
      val bill_updated = timestamp

      val bill = structure.Bill(
          NotAssigned,
          bill_customer_srl,
          bill_member_srl,
          bill_created,
          bill_updated,
          bill_info,
          bill_type,
          bill_due,
          bill_isTaxReceipt,
          bill_amount
      )

      val dbResult = structure.Bill.create(bill)



      if(dbResult != null)
        Ok(Json.obj("result"->"OK", "code"->"200", "data"->dbResult.toJson.toString))
      else
        Ok(Json.obj("result"->"Fail", "code"->"410", "message"->"DATABASE_EXECUTION_EXCEPTION"))
  }

  def modify(id:Int) = Action(parse.urlFormEncoded)
  {
    request =>

      val body:Map[String, Seq[String]] = request.body

      val manufacture_name = body.getOrElse("name", util.dummy.dummyList)(0)
      val manufacture_type = body.getOrElse("type", util.dummy.dummyList)(0)
      val manufacture_address:String = body.getOrElse("address", util.dummy.dummyList)(0)
      val manufacture_phone = body.getOrElse("phone", util.dummy.dummyList)(0)
      val manufacture_charger = body.getOrElse("charger", util.dummy.dummyList)(0)
      val manufacture_mobile = body.getOrElse("mobile", util.dummy.dummyList)(0)
      val manufacture_updated = timestamp

      val manufacture = structure.Manufacture(
        new Id(id),
        manufacture_name,
        manufacture_type,
        manufacture_address,
        manufacture_phone,
        manufacture_charger,
        manufacture_mobile,
        0,
        manufacture_updated)

      val dbResult = structure.Manufacture.update(manufacture)

      if(dbResult != null)
        Ok(Json.obj("result"->"OK", "code"->"200", "data"->dbResult.toJson.toString))
      else
        Ok(Json.obj("result"->"Fail", "code"->"410", "message"->"DATABASE_EXECUTION_EXCEPTION"))
  }

  def delete(id:Int) = TODO

  def get(id:Int) = Action
  {
    request =>

      val dbResult = structure.Manufacture.findById(new Id(id))
      if(dbResult != null)
        Ok(Json.obj("result"->"OK", "code"->"200", "data"->dbResult.toJson.toString))
      else
        Ok(Json.obj("result"->"Fail", "code"->"404", "message"->"NOT_FOUND"))

  }

  def list(page:Int, count:Int, orderBy:String, orderType:String) = Action
  {
    request =>
      val dbResult = structure.Manufacture.findAll(page, count, orderBy, orderType)
      if(dbResult != null)
        Ok(Json.obj("result"->"OK", "code"->"200", "data"->dbResult.toJson.toString))
      else
        Ok(Json.obj("result"->"Fail", "code"->"404", "message"->"NOT_FOUND"))
  }

  def find(target:String, keyword:String, option:String) = Action
  {
    request =>
      var keywordEscape:String = "";
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
      val dbResult = structure.Manufacture.findByOption("manufacture_" + target, keywordEscape, optionEscape)
      if(dbResult != null)
        Ok(Json.obj("result"->"OK", "code"->"200", "data"->dbResult.toJson.toString))
      else
        Ok(Json.obj("result"->"Fail", "code"->"404", "message"->"NOT_FOUND"))
      Ok("")
  }
}
