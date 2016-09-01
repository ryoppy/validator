name := """validator"""

organization := "com.github.ryoppy"

version := "0.0.1"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.2",
  "joda-time" % "joda-time" % "2.9.4",
  "org.joda" % "joda-convert" % "1.8",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test"
)

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Xlint",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused",
  "-Ywarn-value-discard"
)

sonatypeProfileName := "com.github.ryoppy"

pomExtra in Global := {
  <url>http://github.com/ryoppy/validator</url>
    <licenses>
      <license>
        <name>MIT</name>
        <url>http://github.com/ryoppy/validator/blob/master/LICENSE</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>git@github.com:ryoppy/validator.git</url>
      <connection>scm:git:git@github.com:ryoppy/validator.git</connection>
    </scm>
    <developers>
      <developer>
        <id>ryoppy</id>
        <name>Ryo Hongo</name>
        <url>https://github.com/ryoppy</url>
      </developer>
    </developers>
}