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
      val bill_actual_amount = body.getOrElse("actual_amount", util.dummy.dummyListInt)(0).toString.toInt
      val bill_partial_amount = body.getOrElse("partial_amount", util.dummy.dummyListInt)(0).toString.toInt
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
          bill_amount,
          bill_actual_amount,
          bill_partial_amount
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

      val bill_customer_srl:Int = body.getOrElse("customer_srl", util.dummy.dummyListInt)(0).toString.toInt
      val bill_info:String = body.getOrElse("info", util.dummy.dummyList)(0)
      val bill_type = body.getOrElse("type", util.dummy.dummyList)(0)
      val bill_due = body.getOrElse("due", util.dummy.dummyListInt)(0).toString.toInt
      val bill_isTaxReceipt = body.getOrElse("isTaxReceipt", List("N"))(0)
      val bill_amount = body.getOrElse("amount", util.dummy.dummyListInt)(0).toString.toInt
      val bill_actual_amount = body.getOrElse("actual_amount", util.dummy.dummyListInt)(0).toString.toInt
      val bill_partial_amount = body.getOrElse("partial_amount", util.dummy.dummyListInt)(0).toString.toInt
      val bill_updated = timestamp

      val bill = structure.Bill(
        new Id(id),
        bill_customer_srl,
        0,
        0,
        bill_updated,
        bill_info,
        bill_type,
        bill_due,
        bill_isTaxReceipt,
        bill_amount,
        bill_actual_amount,
        bill_partial_amount
      )

      val dbResult = structure.Bill.update(bill)

      if(dbResult != null)
        Ok(Json.obj("result"->"OK", "code"->"200", "data"->dbResult.toJson.toString))
      else
        Ok(Json.obj("result"->"Fail", "code"->"410", "message"->"DATABASE_EXECUTION_EXCEPTION"))
  }

  def delete(id:Int) = Action
  {
    request =>
      val dbResult = structure.Bill.delete(id)
      if(dbResult != null)
        Ok(Json.obj("result"->"OK", "code"->"200", "data"->dbResult.toJson.toString))
      else
        Ok(Json.obj("result"->"Fail", "code"->"404", "message"->"NOT_FOUND"))
  }

  def get(id:Int) = Action
  {
    request =>

      val dbResult = structure.Bill.findBy(new Id(id))
      if(dbResult != null)
        Ok(Json.obj("result"->"OK", "code"->"200", "data"->dbResult.toJson.toString))
      else
        Ok(Json.obj("result"->"Fail", "code"->"404", "message"->"NOT_FOUND"))

  }

  def list(start:Int, end:Int, orderBy:String, orderType:String) = Action
  {
    request =>
      val dbResult = structure.Bill.findByDate(start, end, orderBy, orderType)
      if(dbResult != null)
        Ok(Json.obj("result"->"OK", "code"->"200", "data"->dbResult.toJson.toString))
      else
        Ok(Json.obj("result"->"Fail", "code"->"404", "message"->"NOT_FOUND"))
  }

  def findByCustomerId(keyword:String, option:String, start:Int, end:Int, orderBy:String, orderType:String) = Action
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
      else
        keywordEscape = keyword
      val dbResult = structure.Bill.findByCustomerId(keyword.toInt, start, end, orderBy, orderType)
      if(dbResult != null)
        Ok(Json.obj("result"->"OK", "code"->"200", "data"->dbResult.toJson.toString))
      else
        Ok(Json.obj("result"->"Fail", "code"->"404", "message"->"NOT_FOUND"))
      Ok("")
  }
}
