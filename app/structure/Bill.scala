package structure

import play.api.db.DB
import play.api.Play.current
import anorm._
import anorm.SqlParser._

import spray.json._

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
                 bill_amount:Int)

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
    get[Int]("bill_amount") map
    {
      case bill_srl ~ bill_customer_srl ~ bill_member_srl ~ bill_created ~ bill_updated ~ bill_info ~ bill_type ~ bill_due ~ bill_isTaxReceipt ~ bill_amount
        => Bill(bill_srl, bill_customer_srl, bill_member_srl, bill_created, bill_updated, bill_info, bill_type, bill_due, bill_isTaxReceipt, bill_amount)
    }
  }

  def create(b:Bill):Bill = DB.withConnection
  {
    implicit connection =>
      SQL("INSERT INTO bill(bill_customer_srl, bill_member_srl, bill_created, bill_updated, " +
                            "bill_info, bill_type, bill_due, bill_isTaxReceipt, bill_amount) VALUES(" +
                            "{customer}, {member}, {created}, {updated}, " +
                            "{info}, {type}, {due}, {tax}, {amount});")
      .on("customer"->b.bill_customer_srl,
          "member"->b.bill_member_srl,
          "created"->b.bill_created,
          "updated"->b.bill_updated,
          "info"->b.bill_info,
          "type"->b.bill_type,
          "due"->b.bill_due,
          "tax"->b.bill_isTaxReceipt,
          "amount"->b.bill_amount)
      .executeInsert()
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
      "bill_amount"->JsNumber(b.bill_amount)
    )

    def read(v: JsValue) =
    {
      v.asJsObject.getFields("bill_srl", "bill_customer_srl", "bill_member_srl", "bill_created", "bill_updated", "bill_info", "bill_type", "bill_due", "bill_isTaxReceipt", "bill_amount") match {
        case Seq(JsNumber(bill_srl), JsNumber(bill_customer_srl), JsNumber(bill_member_srl), JsNumber(bill_created), JsNumber(bill_updated), JsString(bill_info), JsString(bill_type), JsNumber(bill_due)
                ,JsString(bill_isTaxReceipt), JsNumber(bill_amount))
          => new Bill(new Id(bill_srl.toInt), bill_customer_srl.toInt, bill_member_srl.toInt, bill_created.toInt, bill_updated.toInt, bill_info, bill_type, bill_due.toInt, bill_isTaxReceipt, bill_amount.toInt)
      }
    }
  }
}
