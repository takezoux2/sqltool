package com.geishatokyo.sqltool.mysql

/**
 * 
 * User: takeshita
 * DateTime: 13/06/25 14:30
 */

trait CreateTableSyntax

case class Table(name : String,createDefs : List[CreateDefinition]) extends CreateTableSyntax{

  lazy val columns : List[Column] = createDefs.collect{
    case col : Column => col
  }

  def column(name : String) = columns.find(c => {
    c.name.toLowerCase == name.toLowerCase
  }).get

  lazy val indexes : List[NormalIndex] = createDefs.collect({
    case index : NormalIndex => index
  })

  def index(name : String) = indexes.find(i => {
    i.name.toLowerCase == name.toLowerCase
  }).get

  lazy val primaryKey : Option[PrimaryKey] = createDefs.collect({
    case pk : PrimaryKey => pk
  }).headOption orElse columns.find( col => col.hasOption(COpPrimaryKey())).map(col => {
    PrimaryKey( List(col.name))
  })

}

trait CreateDefinition

case class Column(name : String,columnType : String, options : List[ColumnOption]) extends CreateDefinition{
  def isSameName( n : String) = name.toLowerCase == n.toLowerCase
  def hasOption( op : ColumnOption) = options.contains(op)
}
trait Index extends CreateDefinition{
  def columns : List[String]
}
case class NormalIndex(name : String , columns : List[String]) extends Index
case class PrimaryKey(columns : List[String]) extends Index
trait ColumnOption

case class NotNull() extends ColumnOption{
  override def toString = "NOT NULL"
}
case class Default(value : String) extends ColumnOption{
  override def toString = "DEFAULT '" + value + "'"
}
case class COpPrimaryKey() extends ColumnOption{
  override def toString = "PRIMARY KEY"
}
case class COpUniqueKey() extends ColumnOption{
  override def toString = "UNIQUE KEY"

}

case class AutoIncrement() extends ColumnOption{
  override def toString = "AUTO_INCREMENT"
}
case class CharacterSet(charSet : String) extends ColumnOption{
  override def toString = "CHARACTER SET " + charSet
}

trait TableOption
case class Engine(engineName : String) extends TableOption
case class TOpCharSet(name : String) extends TableOption
case class TOpAutoIncrement(id : Long) extends TableOption

trait DBType