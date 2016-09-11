lazy val compilerOptions = Seq(
  scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-unchecked",
    "-Xlint",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-unused",
    "-Ywarn-value-discard",
    "-language:existentials",
    "-language:higherKinds",
    "-encoding", "UTF-8",
    "-Yno-adapted-args"
  )
)

lazy val buildSettings = Seq(
  organization := "com.github.ryoppy",
  scalaVersion := "2.11.8",
  libraryDependencies ++= Seq(
    "com.chuusai" %% "shapeless" % "2.3.2",
    "joda-time" % "joda-time" % "2.9.4",
    "org.joda" % "joda-convert" % "1.8",
    "org.scalatest" %% "scalatest" % "2.2.4" % "test"
  )
)

lazy val allSettings = buildSettings ++ compilerOptions ++ publishSettings

lazy val validator = project.in(file("."))
  .settings(allSettings)
  .settings(noPublishSettings)
  .settings(
    initialCommands in console := "import validator._"
  )
  .aggregate(aggregatedProjects: _*)
  .dependsOn(core)

lazy val core = project.in(file("core"))
  .settings(
    description := "validator core",
    moduleName := "validator-core",
    name := "core"
  )
  .settings(allSettings: _*)

lazy val playframework = project.in(file("playframework"))
  .settings(
    description := "validator playframework",
    moduleName := "validator-playframework",
    name := "playframework"
  )
  .settings(allSettings: _*)
  .settings(libraryDependencies ++= Seq(
    "com.typesafe.play" %% "play" % "2.5.6" % "provided"
  ))
  .dependsOn(core)

lazy val aggregatedProjects = Seq[ProjectReference](core, playframework)

lazy val noPublishSettings = Seq(
  publish := (),
  publishLocal := (),
  publishArtifact := false
)

lazy val publishSettings = Seq(
  sonatypeProfileName := "com.github.ryoppy",
  pomExtra := {
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
)