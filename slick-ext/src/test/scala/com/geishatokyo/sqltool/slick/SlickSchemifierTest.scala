package com.geishatokyo.sqltool.slick

import org.specs2.mutable.Specification
import scala.slick.driver.MySQLDriver.simple._

/**
 * 
 * User: takeshita
 * DateTime: 13/05/29 17:07
 */
class SlickSchemifierTest extends Specification {

  lazy val db = Database.forURL(
    "jdbc:mysql://localhost/sqltool_test",
    driver = "com.mysql.jdbc.Driver",
    user = "",
    password = "")


  lazy val schemifier = new SlickSchemifier(db)


  "Schemifier" should{

    "Create tables" in{

      schemifier.schemify(OldUserTable)

      schemifier.schemify(NewUserTable)
    }


  }


}

object OldUserTable extends Table[(Long,String,Int)]("MyTable"){

  def id = column[Long]("id", O.PrimaryKey)
  def name = column[String]("name")
  def age = column[Int]("age")

  def * = id ~ name ~ age

}

object NewUserTable extends Table[(Long,String,Double,Int)]("MyTable"){

  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def name = column[String]("name")
  def age = column[Double]("age")
  def gender = column[Int]("gender")

  def * = id ~ name ~ age ~ gender
}
