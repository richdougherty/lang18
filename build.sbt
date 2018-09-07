name := "lang18"
organization := "nz.rd.lang18"
scalaVersion := "2.12.6"
javacOptions ++= Seq("-processor", "com.oracle.truffle.dsl.processor.LanguageRegistrationProcessor")

libraryDependencies ++= Seq(
  "org.parboiled" %% "parboiled" % "2.1.4",
  "com.oracle.truffle" % "truffle-api" % "1.0.0-rc5",
  "com.oracle.truffle" % "truffle-dsl-processor" % "1.0.0-rc5" % "provided",
  "com.oracle.truffle" % "truffle-tck" % "1.0.0-rc5" % "provided",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
)