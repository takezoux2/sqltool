
package slick

import scala.slick.driver.MySQLDriver
import scala.slick.driver.MySQLDriver.simple.{Session,Database}
import scala.slick.jdbc.StaticQuery.interpolation
import Database.threadLocalSession

import MySQLDriver.simple._

case class BBB( id : Long, name : String)

object HogeTable extends Table[BBB]("Hoge"){
  def id = column[Long]("id",O.PrimaryKey)
  def name = column[String]("name")

  def * = id ~ name <> (BBB,BBB.unapply _)
}

object SQLPrinter{

	def createTable : List[String] = {
		HogeTable.ddl.createStatements.toList
	}

}