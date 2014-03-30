package controllers

import play.api._
import play.api.mvc._

import play.api.libs.json.Json

import anorm._
import spray.json._

import structure.SupplierFormatter._
import util.time._

object Supplier extends Controller {

  def create() = Action(parse.urlFormEncoded)
  {
    request =>
      val body:Map[String, Seq[String]] = request.body

      val supplier_name = body.getOrElse("name", util.dummy.dummyList)(0)
      val supplier_reg_no = body.getOrElse("reg_no", util.dummy.dummyList)(0)
      val supplier_address:String = body.getOrElse("address", util.dummy.dummyList)(0)
      val supplier_phone = body.getOrElse("phone", util.dummy.dummyList)(0)
      val supplier_charger = body.getOrElse("charger", util.dummy.dummyList)(0)
      val supplier_mobile = body.getOrElse("mobile", util.dummy.dummyList)(0)
      val supplier_visiting = body.getOrElse("visiting", util.dummy.dummyList)(0)
      val supplier_created = timestamp
      val supplier_updated = timestamp

      val supplier = structure.Supplier(NotAssigned,
                                       supplier_name,
                                       supplier_reg_no,
                                       supplier_phone,
                                       supplier_address,
                                       supplier_charger,
                                       supplier_mobile,
                                       supplier_visiting,
                                       supplier_created,
                                       supplier_updated)

      val dbResult = structure.Supplier.create(supplier)

      if(dbResult != null)
        Ok(Json.obj("result"->"OK", "code"->"200", "data"->dbResult.toJson.toString))
      else
        Ok(Json.obj("result"->"Fail", "code"->"410", "message"->"DATABASE_EXECUTION_EXCEPTION"))
  }

  def modify(id:Int) = Action(parse.urlFormEncoded)
  {
    request =>
      val body:Map[String, Seq[String]] = request.body

      val supplier_name = body.getOrElse("name", util.dummy.dummyList)(0)
      val supplier_reg_no = body.getOrElse("reg_no", util.dummy.dummyList)(0)
      val supplier_address:String = body.getOrElse("address", util.dummy.dummyList)(0)
      val supplier_phone = body.getOrElse("phone", util.dummy.dummyList)(0)
      val supplier_charger = body.getOrElse("charger", util.dummy.dummyList)(0)
      val supplier_mobile = body.getOrElse("mobile", util.dummy.dummyList)(0)
      val supplier_visiting = body.getOrElse("visiting", util.dummy.dummyList)(0)
      val supplier_updated = timestamp

      val supplier = structure.Supplier(new Id(id),
        supplier_name,
        supplier_reg_no,
        supplier_phone,
        supplier_address,
        supplier_charger,
        supplier_mobile,
        supplier_visiting,
        0,
        supplier_updated)

      val dbResult = structure.Supplier.update(supplier)

      if(dbResult != null)
        Ok(Json.obj("result"->"OK", "code"->"200", "data"->dbResult.toJson.toString))
      else
        Ok(Json.obj("result"->"Fail", "code"->"410", "message"->"DATABASE_EXECUTION_EXCEPTION"))
  }

  def delete(id:Int) = TODO

  def get(id:Int) = Action
  {
    request =>
      val dbResult = structure.Supplier.findById(new Id(id))

      if(dbResult != null)
        Ok(Json.obj("result"->"OK", "code"->"200", "data"->dbResult.toJson.toString))
      else
        Ok(Json.obj("result"->"Fail", "code"->"404", "message"->"NOT_FOUND"))
  }

  def list(page:Int, count:Int, orderBy:String, orderType:String) = Action
  {
    request =>
      val dbResult = structure.Supplier.findAll(page, count, orderBy, orderType)
      if(dbResult != null)
        Ok(Json.obj("result"->"OK", "code"->"200", "data"->dbResult.toJson.toString))
      else
        Ok(Json.obj("result"->"Fail", "code"->"404", "message"->"NOT_FOUND"))
  }

  def find(target:String, keyword:String, option:String) = Action
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
      val dbResult = structure.Supplier.findByOption("supplier_" + target, keywordEscape, optionEscape)
      if(dbResult != null)
        Ok(Json.obj("result"->"OK", "code"->"200", "data"->dbResult.toJson.toString))
      else
        Ok(Json.obj("result"->"Fail", "code"->"404", "message"->"NOT_FOUND"))
      Ok("")
  }

}