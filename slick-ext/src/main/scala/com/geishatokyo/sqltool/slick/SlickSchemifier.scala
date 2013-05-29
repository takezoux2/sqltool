package com.geishatokyo.sqltool.slick

import scala.slick.session.Database
import Database.threadLocalSession
import scala.slick.jdbc.{GetResult, StaticQuery => Q}
import scala.slick.jdbc.StaticQuery.interpolation

import scala.slick.driver.MySQLDriver.simple._
import com.geishatokyo.sqltool.mysql.{SQLGenerator, Diff, CreateTableParser}


/**
 * 
 * User: takeshita
 * DateTime: 13/05/29 16:11
 */
class SlickSchemifier(db : Database) {


  def log(string : String) = println(string)
  def logSql(sql : String) = println(sql)

  var permitAlterTable = true

  private def showTables : List[String] = {
    Q.query[Unit,String]("SHOW TABLES;").list()
  }

  private def showCreateTable(tableName : String) : Option[String] = {
    Q.query[Unit,(String,String)]("SHOW CREATE TABLE " + tableName + ";").list().headOption.map(_._2)
  }

  def dropAllTables() = {
    db.withSession{
      showTables.foreach(tn => {
        Q.update[Unit]("DROP TABLE IF EXISTS " + tn + ";").execute()
      })
    }
  }

  def schemify( tables : Table[_]*) = {

    db.withSession{
      val tablesOnDb = showTables.map(_.toLowerCase)

      val (exists, notExists) = tables.partition(t => tablesOnDb.contains(t.tableName.toLowerCase))
      for (t <- notExists){
        log("Create table " + t.tableName)
        t.ddl.createStatements.foreach(s => {
          logSql(s)
        })
        t.ddl.create
      }
      if(permitAlterTable){
        for(t <- exists){
          val onDb = CreateTableParser(showCreateTable(t.tableName).get)
          val newTable = CreateTableParser(t.ddl.createStatements.next())

          val diff = Diff.diff(onDb,newTable)

          log("Diff:" + diff)

          if (diff.columnToAdd.size == 0){
            log("No changes on " + t.tableName)
          }else{
            log("Alter talbe " + t.tableName)
          }

          if (diff.columnToAdd.size > 0){
            diff.columnToAdd.foreach(c => {
              val sql = SQLGenerator.createAddColumn(t.tableName,c)
              logSql(sql)
              Q.query[Unit,String](sql).execute()
            })
          }

        }

        SchemifyResult(notExists.map(_.tableName).toList,exists.map(_.tableName).toList)
      }else{
        SchemifyResult(notExists.map(_.tableName).toList,Nil)
      }


    }


  }




}

case class SchemifyResult(creates : List[String], alters : List[String])
