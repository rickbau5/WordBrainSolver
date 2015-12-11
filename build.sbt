classpathTypes += "maven-plugin"
resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

libraryDependencies ++= Seq(
  "com.typesafe" % "config" % "1.3.0",
  "org.bytedeco.javacpp-presets" % "tesseract" % "3.04-1.1",
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
)

lazy val root = (project in file(".")).
  settings(
    name := "WordBrainSolver",
    organization := "com.bau5",
    version := "1.0",
    scalaVersion := "2.11.7"
  )