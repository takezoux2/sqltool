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


  def chars : Parser[String] = """[a-zA-Z0-9_]+""".r
  def dataTypeChars = """[a-zA-Z0-9¥(¥)]+""".r

  def word = ("`" ~> chars <~ "`") | chars

  def string = ("'" ~> chars <~ "'") | word


  def createDefinitions = "(" ~> createDefinition ~ rep("," ~> createDefinition) ~ ")" ~ tableOptionsDefinition <~ opt(";") ^^ {
    case cd ~ rest ~ ")" ~ tableOptions => {
      cd :: rest
    }
  }

  def createDefinition = (primaryKeyDefinition | indexDefinition | uniqueKeyDefinition | columnDefinition )

  def columnDefinition = word ~ dataTypeChars ~ rep(columnOption) ^^ {
    case name ~ dbType ~ options => {
      Column(name,dbType,options)
    }
  }
  def columnOption = (AutoIncrementDef | PrimaryKeyDef | UniqueDef | NotNullDef | DefaultDef | CharacterSetDef)

  def IfNotExists = opt("IF" ~ "NOT" ~ "EXISTS")
  def CreateTable = "CREATE" ~ "TABLE"
  def AutoIncrementDef  = "AUTO_INCREMENT" ^^ { _ => AutoIncrement()}
  def PrimaryKeyDef  = "PRIMARY" ~ "KEY" ^^{ _ => COpPrimaryKey()}
  def NotNullDef = "NOT" ~ "NULL" ^^ { _ => NotNull()}
  def UniqueDef = "UNIQUE" ~ "KEY" ^^{ _ => COpUniqueKey()}
  def DefaultDef = "DEFAULT" ~> string ^^{value => Default(value)}
  def CharacterSetDef = "CHARACTER" ~> "SET" ~> word ^^ {value => CharacterSet(value)}


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
  def uniqueKeyDefinition = "UNIQUE" ~> "KEY" ~> word ~ "(" ~ rep1(word, "," ~> word) ~ ")" ^^ {
    case uniqueKeyName ~ "(" ~ cols ~ ")" => {
      UniqueIndex(uniqueKeyName,cols)
    }
  }

  def tableOptionsDefinition = rep(engineDef | TOpCharSetDef | TOpAutoIncrementDef)

  def engineDef = "ENGINE" ~> opt("=") ~> word ^^ {
    case engineName => Engine(engineName)
  }
  def TOpCharSetDef = opt("DEFAULT") ~> "CHARSET=" ~> word ^^{
    case name => TOpCharSet(name)
  }
  def TOpAutoIncrementDef = "AUTO_INCREMENT=" ~> word ^^{
    case n => TOpAutoIncrement(n.toLong)
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




