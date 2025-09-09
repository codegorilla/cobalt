val scala3Version = "3.7.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "cobalt",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    // libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test

    resolvers += "Secured Central Repository" at "https://repo1.maven.org/maven2",

    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.0.0" % Test,
      "org.freemarker" % "freemarker" % "2.3.34",
      "org.antlr" % "ST4" % "4.3.4",
      "org.json" % "json" % "20250517",
      "com.fasterxml.jackson.core" % "jackson-databind" % "2.19.0"
    )

  )
