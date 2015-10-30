

name := "Zisa"

version := "1.0"

scalaVersion := "2.11.4"




libraryDependencies  ++= Seq(
            "org.scala-lang" % "scala-swing" % "2.10.2",
            "com.github.tototoshi" %% "scala-csv" % "1.0.0"
            )
            
libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0" % "test"
//lazy val scalacheck = "org.scalacheck" %% "scalacheck" % "1.12.0"
//libraryDependencies += scalacheck % Test



javacOptions ++= Seq("-source", "1.8", "-target", "1.8", "-Xlint")

initialize := {
  val _ = initialize.value
  if (sys.props("java.specification.version") != "1.8")
    sys.error("Java 8 is required for this project.")
}

seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)