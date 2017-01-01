import sbt._
import Keys._
import Common._
import org.scalajs.sbtplugin.cross.{CrossType, CrossProject}
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

object Build {

  private[this] val zeroFormatterName = "zero-formatter"
  private[this] val scalazName = "zero-formatter-scalaz"
  val allName = "zero-formatter-all"

  private[this] def module(id: String) =
    CrossProject(id, file(id), CustomCrossType).settings(
      commonSettings
    ).settings(
      scalaJSStage in Test := FastOptStage,
      jsEnv := NodeJSEnv().value
    )

  val modules: List[String] = (
    zeroFormatterName ::
    Nil
  )

  // avoid move files
  // https://github.com/scala-js/scala-js/blob/v0.6.7/sbt-plugin/src/main/scala/scala/scalajs/sbtplugin/cross/CrossProject.scala#L193-L206
  object CustomCrossType extends CrossType {
    override def projectDir(crossBase: File, projectType: String) =
      crossBase / projectType

    def shared(projectBase: File, conf: String) =
      projectBase.getParentFile / "src" / conf / "scala"

    override def sharedSrcDir(projectBase: File, conf: String) =
      Some(shared(projectBase, conf))
  }

  private[this] val dogVersion = "0.7.0"

  lazy val zeroFormatter = module("zero-formatter").settings(
    name := zeroFormatterName,
    libraryDependencies ++= Seq(
      "org.spire-math" %%% "spire" % "0.13.0",
      "com.chuusai" %%% "shapeless" % "2.3.2",
      "com.github.pocketberserker" %%% "dog" % dogVersion % "test",
      "com.github.pocketberserker" %%% "dog-props" % dogVersion % "test"
    ),
    libraryDependencies ++= {
     if (scalaBinaryVersion.value startsWith "2.10")
       Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full))
     else
       Nil
    }
  ).jsSettings(
    libraryDependencies += "org.scala-js" %%% "scalajs-java-time" % "0.2.0"
  )

  lazy val scalaz = module("scalaz").settings(
    name := scalazName,
    libraryDependencies ++= Seq(
      "org.scalaz" %%% "scalaz-core" % "7.2.8",
      "com.github.scalaprops" %% "scalaprops-scalazlaws" % "0.3.4" % "test"
    )
  ).dependsOn(zeroFormatter % "test->test;compile->compile")
}
