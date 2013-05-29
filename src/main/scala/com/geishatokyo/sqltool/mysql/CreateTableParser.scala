package com.geishatokyo.sqltool.mysql


import scala.util.parsing.combinator._
import util.matching.Regex
import java.util.regex.Pattern


object CreateTableParser extends RegexParsers{
  // Make literal case insensitive
  implicit override def literal(s: String): Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      val start = handleWhiteSpace(source, offset)
      var i = 0
      var j = start
      while (i < s.length && j < source.length && s.charAt(i).toLower == source.charAt(j).toLower) {
        i += 1
        j += 1
      }
      if (i == s.length)
        Success(source.subSequence(start, j).toString, in.drop(j - offset))
      else  {
        val found = if (start == source.length()) "end of source" else "`"+source.charAt(start)+"'"
        Failure("`"+s+"' expected but "+found+" found", in.drop(start - offset))
      }
    }
  }

  def IfNotExists = opt("IF" ~ "NOT" ~ "EXISTS")
  def CreateTable = "CREATE" ~ "TABLE"
  def AutoIncrementDef  = "AUTO_INCREMENT" ^^ { _ => AutoIncrement()}
  def PrimaryKeyDef  = "PRIMARY" ~ "KEY" ^^{ _ => COpPrimaryKey()}
  def NotNullDef = "NOT" ~ "NULL" ^^ { _ => NotNull()}

  def DefaultDef = "DEFAULT" ~> string ^^{value => Default(value)}
  def CharacterSetDef = "CHARACTER" ~> "SET" ~> word ^^ {value => CharacterSet(value)}


  def chars : Parser[String] = """[a-zA-Z0-9_]+""".r
  def dataTypeChars = """[a-zA-Z0-9¥(¥)]+""".r

  def word = ("`" ~> chars <~ "`") | chars

  def string = ("'" ~> chars <~ "'") | word


  def createDefinitions = "(" ~> createDefinition ~ rep("," ~> createDefinition) ~ ")" ~ tableOptionsDefinition <~ opt(";") ^^ {
    case cd ~ rest ~ ")" ~ tableOptions => {
      cd :: rest
    }
  }

  def createDefinition = (primaryKeyDefinition | indexDefinition | columnDefinition )

  def columnDefinition = word ~ dataTypeChars ~ rep(columnOption) ^^ {
    case name ~ dbType ~ options => {
      Column(name,dbType,options)
    }
  }
  def columnOption = (AutoIncrementDef | PrimaryKeyDef | NotNullDef | DefaultDef | CharacterSetDef)

  def primaryKeyDefinition = "PRIMARY" ~> "KEY" ~> "(" ~> word ~ rep( "," ~> word) <~ ")" ^^ {
    case col ~ restCols => {
      PrimaryKey(col :: restCols)
    }
  }
  /*def indexDefinition = ("INDEX" | "KEY") ~> word <~ "(" ~> word ~ rep("," ~> word) <~ ")" ^^ {
      case indexName ~ col ~ restCols => {
          Index(indexName, col :: restCols)
      }
  }*/
  def indexDefinition = ("INDEX" | "KEY") ~> word ~ opt(word) ~ "(" ~ word ~ rep( "," ~> word) ~ ")" ^^ {
    case indexName ~ indexType ~ "(" ~ col ~ restCols ~ ")" => {
      NormalIndex(indexName,col :: restCols)
    }
  }

  def tableOptionsDefinition = rep(engineDef | TOpCharSetDef)

  def engineDef = "ENGINE" ~> opt("=") ~> word ^^ {
    case engineName => Engine(engineName)
  }
  def TOpCharSetDef = opt("DEFAULT") ~> "CHARSET=" ~> word ^^{
    case name => TOpCharSet(name)
  }

  def createTableExpr = CreateTable ~> IfNotExists ~> word ~ createDefinitions ^^ {
    case tableName ~ createDefs => {
      Table(tableName,createDefs)
    }
  }


  def expr = createTableExpr





  def apply(s:String):Table = {
    parseAll(expr,s) match {
      case Success(tree, _) => tree
      case e: NoSuccess =>
        println(e)
        throw new IllegalArgumentException("Bad syntax: "+s)
    }
  }



}

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
case class AutoIncrement() extends ColumnOption{
  override def toString = "AUTO_INCREMENT"
}
case class CharacterSet(charSet : String) extends ColumnOption{
  override def toString = "CHARACTER SET " + charSet
}

trait TableOption
case class Engine(engineName : String) extends TableOption
case class TOpCharSet(name : String) extends TableOption

trait DBType


