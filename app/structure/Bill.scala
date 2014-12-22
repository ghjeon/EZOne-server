package structure

import play.api.db.DB
import play.api.Play.current
import anorm._
import anorm.SqlParser._

import spray.json._

import util.db._

/**
 * Project EZOne-Server
 * Module structure
 * User: Gyuhyeon
 * Date: 2013. 12. 29.
 * Time: 오후 5:12
 */


case class Bill (bill_srl:Pk[Int] = NotAssigned,
                 bill_customer_srl:Int,
                 bill_member_srl:Int,
                 bill_created:Int,
                 bill_updated:Int,
                 bill_info:String,
                 bill_type:String,
                 bill_due:Int,
                 bill_isTaxReceipt:String,
                 bill_amount:Int,
                 bill_actual_amount:Int,
                 bill_partial_amount:Int)

case class BillExtend (bill_srl:Pk[Int] = NotAssigned,
                 bill_customer_srl:Int,
                 customer_name:String,
                 bill_member_srl:Int,
                 member_name:String,
                 bill_created:Int,
                 bill_updated:Int,
                 bill_info:String,
                 bill_type:String,
                 bill_due:Int,
                 bill_isTaxReceipt:String,
                 bill_amount:Int,
                 bill_actual_amount:Int,
                 bill_partial_amount:Int)

object BillExtend {
  val parser =
  {
    get[Pk[Int]]("bill_srl") ~
      get[Int]("bill_customer_srl") ~
      get[String]("customer_name") ~
      get[Int]("bill_member_srl") ~
      get[String]("member_name") ~
      get[Int]("bill_created") ~
      get[Int]("bill_updated") ~
      get[String]("bill_info") ~
      get[String]("bill_type") ~
      get[Int]("bill_due") ~
      get[String]("bill_isTaxReceipt") ~
      get[Int]("bill_amount") ~
      get[Int]("bill_actual_amount") ~
      get[Int]("bill_partial_amount") map
      {
        case bill_srl ~ bill_customer_srl ~ customer_name ~ bill_member_srl ~ member_name ~ bill_created ~ bill_updated ~ bill_info ~ bill_type ~ bill_due ~ bill_isTaxReceipt ~ bill_amount ~ bill_actual_amount ~ bill_partial_amount
        => BillExtend(bill_srl, bill_customer_srl, customer_name, bill_member_srl, member_name, bill_created, bill_updated, bill_info, bill_type, bill_due, bill_isTaxReceipt, bill_amount, bill_actual_amount, bill_partial_amount)
      }
  }
}

object Bill {
  val parser =
  {
    get[Pk[Int]]("bill_srl") ~
    get[Int]("bill_customer_srl") ~
    get[Int]("bill_member_srl") ~
    get[Int]("bill_created") ~
    get[Int]("bill_updated") ~
    get[String]("bill_info") ~
    get[String]("bill_type") ~
    get[Int]("bill_due") ~
    get[String]("bill_isTaxReceipt") ~
    get[Int]("bill_amount") ~
    get[Int]("bill_actual_amount") ~
    get[Int]("bill_partial_amount") map
    {
      case bill_srl ~ bill_customer_srl ~ bill_member_srl ~ bill_created ~ bill_updated ~ bill_info ~ bill_type ~ bill_due ~ bill_isTaxReceipt ~ bill_amount ~ bill_actual_amount ~ bill_partial_amount
        => Bill(bill_srl, bill_customer_srl, bill_member_srl, bill_created, bill_updated, bill_info, bill_type, bill_due, bill_isTaxReceipt, bill_amount, bill_actual_amount, bill_partial_amount)
    }
  }

  def findBy(id:Pk[Int]):Bill = DB.withConnection
  {
    implicit connection =>
      SQL("SELECT * FROM bill WHERE " +
        "bill_srl = {srl};")
      .on("srl"->id.get)
      .using(this.parser).single()
  }

  def findByProductId(product_srl:String, start:Int, end:Int, orderBy:String, orderType:String):List[BillExtend] = DB.withConnection
  {
    implicit connection =>
      val srl = "%" + product_srl + "%"
      SQL("SELECT * FROM bill_extend WHERE " +
          "bill_info LIKE {srl} AND bill_updated >= {start} AND bill_updated <= {end} order by {orderBy} " + util.db.validateOrderType(orderType) + ";")
      .on("srl"->srl,
          "start"->start,
          "end"->end,
          "orderBy"->toParameterValue("bill_" + orderBy))
      .as(BillExtend.parser *)
  }

  def findByDate(start:Int, end:Int, orderBy:String, orderType:String):List[BillExtend] = DB.withConnection
  {
    implicit connection =>
      SQL("SELECT * FROM bill_extend WHERE " +
        "bill_updated >= {start} AND bill_updated <= {end} order by {orderBy} " + util.db.validateOrderType(orderType) + ";")
        .on("start"->start,
            "end"->end,
            "orderBy"->toParameterValue("bill_" + orderBy))
        .as(BillExtend.parser *)
  }

  def findByCustomerId(customer_srl:Int, start:Int, end:Int, orderBy:String, orderType:String):List[BillExtend] = DB.withConnection
  {
    implicit connection =>
      SQL("SELECT * FROM bill_extend WHERE " +
        "bill_customer_srl = {srl} AND bill_updated >= {start} AND bill_updated <= {end} order by {orderBy} " + util.db.validateOrderType(orderType) + ";")
      .on("srl"->customer_srl,
          "start"->start,
          "end"->end,
          "orderBy"->toParameterValue("bill_" + orderBy))
      .as(BillExtend.parser *)
  }

  def create(b:Bill):Bill = DB.withConnection
  {
    implicit connection =>
      SQL("INSERT INTO bill(bill_customer_srl, bill_member_srl, bill_created, bill_updated, " +
                            "bill_info, bill_type, bill_due, bill_isTaxReceipt, bill_amount, bill_actual_amount, bill_partial_amount) VALUES(" +
                            "{customer}, {member}, {created}, {updated}, " +
                            "{info}, {type}, {due}, {tax}, {amount}, {actual_amount}, {partial_amount});")
      .on("customer"->b.bill_customer_srl,
          "member"->b.bill_member_srl,
          "created"->b.bill_created,
          "updated"->b.bill_updated,
          "info"->b.bill_info,
          "type"->b.bill_type,
          "due"->b.bill_due,
          "tax"->b.bill_isTaxReceipt,
          "amount"->b.bill_amount,
          "actual_amount"->b.bill_actual_amount,
          "partial_amount"->b.bill_partial_amount)
      .executeInsert()

      SQL("SELECT * from bill WHERE bill_customer_srl = {srl} AND bill_created = {created} AND bill_info = {info} AND bill_type = {type} AND bill_amount = {amount} ORDER BY bill_srl desc LIMIT 1;")
        .on("srl"->b.bill_customer_srl,
        "created"->b.bill_created,
        "info"->b.bill_info,
        "type"->b.bill_type,
        "amount"->b.bill_amount)
      .using(this.parser).single()
  }

  def update(b:Bill):Bill = DB.withConnection
  {
    implicit connection =>
      SQL("UPDATE bill SET bill_updated = {updated}, " +
          "bill_info = {info}, " +
          "bill_type = {type}, " +
          "bill_due = {due}, " +
          "bill_isTaxReceipt = {tax}, " +
          "bill_amount = {amount} " +
          "bill_actual_amount = {actual_amount} " +
          "bill_partial_amount = {partial_amount} " +
          "WHERE bill_srl = {srl} AND bill_customer_srl = {customer};")
        .on("customer"->b.bill_customer_srl,
            "updated"->b.bill_updated,
            "info"->b.bill_info,
            "type"->b.bill_type,
            "due"->b.bill_due,
            "tax"->b.bill_isTaxReceipt,
            "amount"->b.bill_amount,
            "actual_amount"->b.bill_actual_amount,
            "partial_amount"->b.bill_partial_amount).executeUpdate()

      findBy(b.bill_srl)
  }

  def delete(bill_srl:Int):Bill = DB.withConnection
  {
    implicit connection =>
      val data = findBy(new Id(bill_srl))

      SQL("DELETE FROM bill WHERE bill_srl = {bill_srl};")
        .on("bill_srl"->bill_srl).execute()

      data
  }
}

object BillFormatter extends DefaultJsonProtocol
{
  implicit object BillJsonFormat extends RootJsonFormat[Bill]
  {
    def write(b: Bill) = JsObject(
      "bill_srl" -> JsNumber(b.bill_srl.get),
      "bill_customer_srl"->JsNumber(b.bill_customer_srl),
      "bill_member_srl"->JsNumber(b.bill_member_srl),
      "bill_created"->JsNumber(b.bill_created),
      "bill_updated"->JsNumber(b.bill_updated),
      "bill_info"->JsString(b.bill_info),
      "bill_type"->JsString(b.bill_type),
      "bill_due"->JsNumber(b.bill_due),
      "bill_isTaxReceipt"->JsString(b.bill_isTaxReceipt),
      "bill_amount"->JsNumber(b.bill_amount),
      "bill_actual_amount"->JsNumber(b.bill_actual_amount),
      "bill_partial_amount"->JsNumber(b.bill_partial_amount)
    )

    def read(v: JsValue) =
    {
      v.asJsObject.getFields("bill_srl", "bill_customer_srl", "bill_member_srl", "bill_created", "bill_updated", "bill_info", "bill_type", "bill_due", "bill_isTaxReceipt", "bill_amount", "bill_actual_amount", "bill_partial_amount") match {
        case Seq(JsNumber(bill_srl), JsNumber(bill_customer_srl), JsNumber(bill_member_srl), JsNumber(bill_created), JsNumber(bill_updated), JsString(bill_info), JsString(bill_type), JsNumber(bill_due)
                ,JsString(bill_isTaxReceipt), JsNumber(bill_amount), JsNumber(bill_actual_amount), JsNumber(bill_partial_amount))
          => new Bill(new Id(bill_srl.toInt), bill_customer_srl.toInt, bill_member_srl.toInt, bill_created.toInt, bill_updated.toInt, bill_info, bill_type, bill_due.toInt, bill_isTaxReceipt, bill_amount.toInt, bill_actual_amount.toInt, bill_partial_amount.toInt)
      }
    }
  }

  implicit object BillExtendJsonFormat extends RootJsonFormat[BillExtend]
  {
    def write(b: BillExtend) = JsObject(
      "bill_srl" -> JsNumber(b.bill_srl.get),
      "bill_customer_srl"->JsNumber(b.bill_customer_srl),
      "customer_name"->JsString(b.customer_name),
      "bill_member_srl"->JsNumber(b.bill_member_srl),
      "member_name"->JsString(b.member_name),
      "bill_created"->JsNumber(b.bill_created),
      "bill_updated"->JsNumber(b.bill_updated),
      "bill_info"->JsString(b.bill_info),
      "bill_type"->JsString(b.bill_type),
      "bill_due"->JsNumber(b.bill_due),
      "bill_isTaxReceipt"->JsString(b.bill_isTaxReceipt),
      "bill_amount"->JsNumber(b.bill_amount),
      "bill_actual_amount"->JsNumber(b.bill_actual_amount),
      "bill_partial_amount"->JsNumber(b.bill_partial_amount)
    )

    def read(v: JsValue) =
    {
      v.asJsObject.getFields("bill_srl", "bill_customer_srl", "customer_name", "bill_member_srl", "member_name", "bill_created", "bill_updated", "bill_info", "bill_type", "bill_due", "bill_isTaxReceipt", "bill_amount", "bill_actual_amount", "bill_partial_amount") match {
        case Seq(JsNumber(bill_srl), JsNumber(bill_customer_srl), JsString(customer_name), JsNumber(bill_member_srl), JsString(member_name), JsNumber(bill_created), JsNumber(bill_updated), JsString(bill_info), JsString(bill_type), JsNumber(bill_due)
        ,JsString(bill_isTaxReceipt), JsNumber(bill_amount), JsNumber(bill_actual_amount), JsNumber(bill_partial_amount))
        => new BillExtend(new Id(bill_srl.toInt), bill_customer_srl.toInt, customer_name, bill_member_srl.toInt, member_name, bill_created.toInt, bill_updated.toInt, bill_info, bill_type, bill_due.toInt, bill_isTaxReceipt, bill_amount.toInt, bill_actual_amount.toInt, bill_partial_amount.toInt)
      }
    }
  }
}
