val catsVersion = "2.1.1"

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.erasmos",
      scalaVersion := "2.13.6"
    )),
    name := "cats-demo"
  )

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.scalatest" %% "scalatest" % "3.2.9" % Test
)

addCommandAlias("fmt", "scalafmtAll")
