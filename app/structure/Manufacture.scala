package structure

import play.api.db.DB
import play.api.Play.current
import anorm._
import anorm.SqlParser._

import spray.json._

import util.structure._
/**
 * Project IntelliJ IDEA
 * Module structure
 * User: Gyuhyeon
 * Date: 2014. 1. 8.
 * Time: 오전 1:05
 */

case class Manufacture (manufacture_srl:Pk[Int] = NotAssigned,
                        manufacture_name:String,
                        manufacture_type:String,
                        manufacture_address:String,
                        manufacture_phone:String,
                        manufacture_charger:String,
                        manufacture_mobile:String,
                        manufacture_created:Int,
                        manufacture_updated:Int)

object Manufacture 
{
  val parser = 
  {
    get[Pk[Int]]("manufacture_srl") ~
    get[String]("manufacture_name") ~
    get[String]("manufacture_type") ~
    get[String]("manufacture_address") ~
    get[String]("manufacture_phone") ~
    get[String]("manufacture_charger") ~
    get[String]("manufacture_mobile") ~
    get[Int]("manufacture_created") ~
    get[Int]("manufacture_updated") map {
      case manufacture_srl ~ manufacture_name ~ manufacture_type ~ manufacture_address ~ manufacture_phone ~ manufacture_charger ~ manufacture_mobile ~ manufacture_created ~ manufacture_updated 
        => Manufacture(manufacture_srl, manufacture_name, manufacture_type, manufacture_address, manufacture_phone, manufacture_charger, manufacture_mobile, manufacture_created, manufacture_updated)
    }
  }

  def findAll(page:Int, count:Int, orderBy:String, orderType:String) = DB.withConnection
  {
    implicit connection =>
      try
      {
        SQL("SELECT * from manufacture order by {orderBy} " + validateOrderType(orderType) + " limit {page}, {count}")
          .on("orderBy"->toParameterValue("manufacture_" + orderBy),
              "page"->getPageIndex(page, count),
              "count"->count).as(this.parser *)
      } catch {
        case e => null
      }
  }

  def findById(srl:Pk[Int]):Manufacture = DB.withConnection
  {
    implicit connection =>
      try
      {
        SQL("SELECT * from manufacture where manufacture_srl = {srl};")
          .on("srl"->srl.get)
          .using(this.parser).single()
      } catch {
        case e=> null
      }
  }

  def findByOption(target:String, keyword:String, option:String):List[Manufacture] = DB.withConnection
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

        val query = SQL("SELECT * from manufacture where " + target + " " + option + " {keyword}")
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

  def create(m:Manufacture):Manufacture = DB.withConnection
  {
    implicit connection =>
      val insertRow = SQL("INSERT INTO manufacture(manufacture_name, manufacture_type, manufacture_address, manufacture_phone, manufacture_charger, manufacture_mobile, manufacture_created, manufacture_updated) " +
        "values ({name}, {type}, {address}, {phone}, {charger}, {mobile}, {created}, {updated}); ")
        .on("name"->m.manufacture_name,
            "type"->m.manufacture_type,
            "address"->m.manufacture_address,
            "phone"->m.manufacture_phone,
            "charger"->m.manufacture_charger,
            "mobile"->m.manufacture_mobile,
            "created"->m.manufacture_created,
            "updated"->m.manufacture_updated).executeInsert(scalar[Long].single).toInt

      findById(new Id(insertRow))
  }

  def update(m:Manufacture):Manufacture = DB.withConnection
  {
    implicit connection =>
      val updateRow = SQL("UPDATE manufacture set manufacture_name = {name}, " +
                                                 "manufacture_type = {type}, " +
                                                 "manufacture_address = {address}, " +
                                                 "manufacture_phone = {phone}, " +
                                                 "manufacture_charger = {charger}, " +
                                                 "manufacture_mobile = {mobile}, " +
                                                 "manufacture_updated = {updated} " +
                                                 "where manufacture_srl = {id};")
      .on("name"->m.manufacture_name,
          "type"->m.manufacture_type,
          "address"->m.manufacture_address,
          "phone"->m.manufacture_phone,
          "charger"->m.manufacture_charger,
          "mobile"->m.manufacture_mobile,
          "updated"->m.manufacture_updated,
          "id"->m.manufacture_srl.get).executeUpdate()

      findById(m.manufacture_srl)
  }


}

object ManufactureFormatter extends DefaultJsonProtocol
{
  implicit object ManufactureJsonFormat extends RootJsonFormat[Manufacture]
  {
    def write(m:Manufacture) = JsObject(
      "manufacture_srl" -> JsNumber(m.manufacture_srl.get),
      "manufacture_name" -> JsString(m.manufacture_name),
      "manufacture_type" -> JsString(m.manufacture_type),
      "manufacture_address" -> JsString(m.manufacture_address),
      "manufacture_phone" -> JsString(m.manufacture_phone),
      "manufacture_charger" -> JsString(m.manufacture_charger),
      "manufacture_mobile" -> JsString(m.manufacture_mobile),
      "manufacture_created" -> JsNumber(m.manufacture_created),
      "manufacture_updated" -> JsNumber(m.manufacture_updated)
    )

    def read(v:JsValue) =
    {
      v.asJsObject.getFields("manufacture_srl", "manufacture_name", "manufacture_type", "manufacture_address", "manufacture_phone", "manufacture_charger", "manufacture_mobile", "manufacture_created", "manufacture_updated") match {
        case Seq(JsNumber(manufacture_srl), JsString(manufacture_name), JsString(manufacture_type), JsString(manufacture_address), JsString(manufacture_phone), JsString(manufacture_charger), JsString(manufacture_mobile), JsNumber(manufacture_created), JsNumber(manufacture_updated))
          => new Manufacture(new Id(manufacture_srl.toInt), manufacture_name, manufacture_type, manufacture_address, manufacture_phone, manufacture_charger, manufacture_mobile, manufacture_created.toInt, manufacture_updated.toInt)
      }
    }
  }
}