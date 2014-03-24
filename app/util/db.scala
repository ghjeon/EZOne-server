package util

/**
 * Project IntelliJ IDEA
 * Module util
 * User: Gyuhyeon
 * Date: 2014. 2. 7.
 * Time: 오전 11:38
 */
object db {

  def getPageIndex(page:Int, count:Int):Int =
  {
    (page-1)*count
  }

  def validateOrderType(orderType:String):String =
  {
    orderType match {
      case "desc" => "desc"
      case "asc" => "asc"
      case default => "asc"
    }
  }

}
