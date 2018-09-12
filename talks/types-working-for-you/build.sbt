import scala.sys.process._

enablePlugins(TutPlugin)

lazy val `presentation` = project.in(file("."))
  .settings(compilerOptions ++ compilerPlugins)
  .settings(
    showSuccess := false,

    // Reload terminal on compile:
    triggeredMessage in ThisBuild := Watched.clearWhenTriggered,

    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core"   % "1.1.0",
      "org.typelevel" %% "cats-effect" % "0.10",
      "com.chuusai"   %% "shapeless"   % "2.3.3",

      // Test dependencies:
      "org.scalatest" %% "scalatest" % "3.0.4"  % "test",
    ),

    tut := {
      val log = streams.value.log
      val tutRes = tut.value
      log.info("Running pandoc...")
      Seq(
        "pandoc",
        "-t beamer",
        "--pdf-engine=xelatex",
        "-H customizations.tex",
        "-o talk.pdf target/scala-2.12/tut/talk.md",
      ).mkString(" ") !! log
      tutRes
    }
  )

lazy val compilerOptions = Seq(
  scalacOptions ++= Seq(
    "-Ypartial-unification",
    "-Xfatal-warnings",
    "-feature",
    "-deprecation",
    "-language:higherKinds",
    "-language:implicitConversions",
  ),
  scalacOptions in (Test, compile) ~= (_ filterNot (_ == "-Ywarn-unused"))
)

lazy val compilerPlugins = Seq(
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4"),
  addCompilerPlugin(
    "org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full
  ),
)
