// val dottyVersion = "0.27.0-RC1"
val dottyVersion = "3.0.0-M2"


lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies ++= Seq(
      "com.novocode" % "junit-interface" % "0.11" % "test",
      "org.typelevel" %% "cats-core" % "2.3.1-SNAPSHOT",
      "org.typelevel" %% "cats-effect" % "3.0.0-M4"
    )
  )
