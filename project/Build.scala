import sbt._
import Keys._
import Common._
import org.scalajs.sbtplugin.cross.{CrossType, CrossProject}
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

object Build {

  private[this] val zeroFormatterName = "zero-formatter"
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

  lazy val zeroFormatter = module("zero-formatter").settings(
    name := zeroFormatterName,
    libraryDependencies ++= Seq(
      "org.spire-math" %%% "spire" % "0.13.0",
      "com.github.scalaprops" %%% "scalaprops" % "0.3.4" % "test"
    )
  )
}
