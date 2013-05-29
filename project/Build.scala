import sbt._
import Keys._


object SQLToolBuild extends Build{


  def testAllProject = Command.command("testAll")( state => {
    val subState = Command.process("project sqltool-slick-ext",state)
    Command.process("test",subState)
    state
  })

  def publishAll = Command.command("publishAll")( state => {
    Command.process("publish",Command.process("project sqltool",state))
    Command.process("publish",Command.process("project sqltool-slick-ext",state))
    state
  })


  override lazy val settings = super.settings ++ org.sbtidea.SbtIdeaPlugin.settings ++ Seq(
  	organization := "com.geishatokyo",
    version := "0.0.1-SNAPSHOT",
  	scalaVersion := "2.10.0",
  	crossScalaVersions := Seq("2.10.0"),
  	resolvers := Seq()
  )

  lazy val root = Project(id = "sqltool",
    base = file("."),
    settings = Project.defaultSettings ++ settings ++ Seq(
      libraryDependencies ++= List("org.specs2" %% "specs2" % "1.14" % "test"),
      commands ++= Seq(testAllProject,publishAll)
    )
  )

  lazy val slickExt = Project(id="sqltool-slick-ext",
    base = file("slick-ext"),
    settings = Project.defaultSettings ++ settings ++ Seq(
      libraryDependencies ++= List(
        "com.typesafe.slick" %% "slick" % "1.0.0",
        "org.specs2" %% "specs2" % "1.14" % "test",
        "mysql" % "mysql-connector-java" % "5.1.25" % "test"
      )
    )
  ) dependsOn(root)



}

