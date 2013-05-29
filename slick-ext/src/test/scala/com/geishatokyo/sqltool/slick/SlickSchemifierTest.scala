package com.geishatokyo.sqltool.slick

import org.specs2.mutable.Specification
import java.util.Properties
import slick.session.Database
import slick.driver.ExtendedProfile

object Driver{
  val driver : ExtendedProfile = scala.slick.driver.MySQLDriver
}

import Driver.driver.simple._

/**
 * 
 * User: takeshita
 * DateTime: 13/05/29 17:07
 */
class SlickSchemifierTest extends Specification {

  lazy val db = {
    val p = new Properties()
    p.load(getClass.getClassLoader.getResourceAsStream("dbsetting.properties"))
    Database.forURL(
    p.get("mysql.url").toString,
    driver = "com.mysql.jdbc.Driver",
    user = p.get("mysql.user").toString,
    password = p.get("mysql.password").toString)
  }


  lazy val schemifier = new SlickSchemifier(db)


  "Schemifier" should{

    schemifier.dropAllTables()

    "Create tables" in{

      schemifier.schemify(OldUserTable) must_==(SchemifyResult(List("MyTable"),Nil))

      schemifier.schemify(NewUserTable) must_==(SchemifyResult(Nil,List("MyTable")))
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
