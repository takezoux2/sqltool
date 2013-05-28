import sbt._
import Keys._


object SQLToolBuild extends Build{


  override lazy val settings = super.settings ++ org.sbtidea.SbtIdeaPlugin.settings ++ Seq(
  	organization := "com.geishatokyo", 
  	scalaVersion := "2.10.0",
  	crossScalaVersions := Seq("2.10.0"),
  	resolvers := Seq()
  )

  lazy val root = Project(id = "sqltool",
    base = file("."),
    settings = Project.defaultSettings ++ settings ++ Seq(
    )
  )

  lazy val slickExt = Project(id="sqltool-slick-ext",
    base = file("slick-ext"),
    settings = Project.defaultSettings ++ settings ++ Seq(
      libraryDependencies ++= List("com.typesafe.slick" % "slick_2.10" % "1.0.0")
    )
  ) dependsOn(root)

}

