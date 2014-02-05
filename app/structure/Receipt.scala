package structure

import play.api.db.DB
import play.api.Play.current
import anorm._
import anorm.SqlParser._

import spray.json._

/**
 * Project IntelliJ IDEA
 * Module structure
 * User: Gyuhyeon
 * Date: 2014. 1. 13.
 * Time: 오후 4:59
 */
case class Receipt (receipt_srl:Pk[Int],
                    receipt_bill_srl:Int,
                    receipt_price:Int,
                    receipt_tax:Int,
                    receipt_amount:Int,
                    receipt_customer_name:String,
                    receipt_customer_reg_no:String,
                    receipt_customer_srl:Int,
                    receipt_isSended:String,
                    receipt_publish_no:String,
                    receipt_created:Int)

object Receipt
{
  val parser = {
    get[Pk[Int]]("receipt_srl") ~
    get[Int]("receipt_bill_srl") ~
    get[Int]("receipt_price") ~
    get[Int]("receipt_tax") ~
    get[Int]("receipt_amount") ~
    get[String]("receipt_customer_name") ~
    get[String]("receipt_customer_reg_no") ~
    get[Int]("receipt_customer_srl") ~
    get[String]("receipt_isSended") ~
    get[String]("receipt_publish_no") ~
    get[Int]("receipt_created") map
      {
        case receipt_srl ~ receipt_bill_srl ~ receipt_price ~ receipt_tax ~ receipt_amount ~ receipt_customer_name ~ receipt_customer_reg_no ~ receipt_customer_srl ~ receipt_isSended ~ receipt_publish_no ~ receipt_created
          => Receipt(receipt_srl, receipt_bill_srl, receipt_price, receipt_tax, receipt_amount, receipt_customer_name, receipt_customer_reg_no, receipt_customer_srl, receipt_isSended, receipt_publish_no, receipt_created)
      }
  }
}

object ReceiptFormatter extends DefaultJsonProtocol
{
  implicit object ReceiptFormat extends RootJsonFormat[Receipt]
  {
    def write(r:Receipt) = JsObject(
      "receipt_srl" -> JsNumber(r.receipt_srl.get),
      "receipt_bill_srl" -> JsNumber(r.receipt_bill_srl),
      "receipt_price" -> JsNumber(r.receipt_price),
      "receipt_tax" -> JsNumber(r.receipt_tax),
      "receipt_amount" -> JsNumber(r.receipt_amount),
      "receipt_customer_name" -> JsString(r.receipt_customer_name),
      "receipt_customer_reg_no" -> JsString(r.receipt_customer_reg_no),
      "receipt_customer_srl" -> JsNumber(r.receipt_customer_srl),
      "receipt_isSended" -> JsString(r.receipt_isSended),
      "receipt_publish_no" -> JsNumber(r.receipt_publish_no),
      "receipt_created" -> JsNumber(r.receipt_created)
    )
    
    def read(v:JsValue) = 
    {
      v.asJsObject.getFields("receipt_srl", "receipt_bill_srl", "receipt_price", "receipt_tax", "receipt_amount", "receipt_customer_name", "receipt_customer_reg_no", "receipt_customer_srl", "receipt_isSended", "receipt_publish_no", "receipt_created") match {
        case Seq(JsNumber(receipt_srl), JsNumber(receipt_bill_srl), JsNumber(receipt_price), JsNumber(receipt_tax), JsNumber(receipt_amount), JsString(receipt_customer_name), JsString(receipt_customer_reg_no), JsNumber(receipt_customer_srl), JsString(receipt_isSended), JsString(receipt_publish_no), JsNumber(receipt_created))
        => new Receipt(new Id(receipt_srl.toInt), receipt_bill_srl.toInt, receipt_price.toInt, receipt_tax.toInt, receipt_amount.toInt, receipt_customer_name, receipt_customer_reg_no, receipt_customer_srl.toInt, receipt_isSended, receipt_publish_no, receipt_created.toInt)
      }
    }
  }
}