package com.geishatokyo.sqltool.mysql

/**
 * 
 * User: takeshita
 * DateTime: 13/05/29 17:01
 */
object SQLGenerator {


  def createAddColumn( tableName : String, column : Column) = {
    "ALTER TABLE `" + tableName + "` ADD COLUMN `" + column.name + "` " + column.columnType + " " + column.options.mkString(" ") + ";"
  }


}
