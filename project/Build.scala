import sbt._
import Keys._
import Common._
import org.scalajs.sbtplugin.cross.{CrossType, CrossProject}
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

object Build {

  private[this] val zeroFormatterCoreName = "zero-formatter-core"
  private[this] val zeroFormatterMacrosName = "zero-formatter-macros"
  private[this] val zeroFormatterName = "zero-formatter"
  private[this] val scalazName = "zero-formatter-scalaz"
  private[this] val catsCoreName = "zero-formatter-cats-core"
  val allName = "zero-formatter-all"

  private[this] def module(id: String) =
    CrossProject(id, file(id), CustomCrossType).settings(
      commonSettings
    ).settings(
      scalaJSStage in Test := FastOptStage,
      jsEnv := NodeJSEnv().value
    ).jsSettings(
      scalacOptions += {
        val a = (baseDirectory in LocalRootProject).value.toURI.toString
        val g = "https://raw.githubusercontent.com/pocketberserker/scala-zero-formatter/" + Common.tagOrHash.value
        s"-P:scalajs:mapSourceURI:$a->$g/"
      }
    )

  val modules: List[String] = (
    zeroFormatterCoreName ::
    zeroFormatterMacrosName ::
    scalazName ::
    catsCoreName ::
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
  private[this] val catsVersion = "0.8.1"

  lazy val zeroFormatterCore = module("core").settings(
    name := zeroFormatterCoreName,
    libraryDependencies ++= Seq(
      "org.spire-math" %%% "spire" % "0.13.0"
    )
  )

  lazy val zeroFormatterMacros = module("macros").settings(
    name := zeroFormatterMacrosName,
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
      "com.chuusai" %%% "shapeless" % "2.3.2"
    )
  ).dependsOn(zeroFormatterCore)

  lazy val zeroFormatter = module(zeroFormatterName).settings(
    name := zeroFormatterName,
    libraryDependencies ++= Seq(
      "com.github.pocketberserker" %%% "dog" % dogVersion % "test",
      "com.github.pocketberserker" %%% "dog-props" % dogVersion % "test"
    )
  ).dependsOn(
    zeroFormatterMacros
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

  lazy val catsCore = module("cats-core").settings(
    name := catsCoreName,
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % "0.9.0"
    )
  ).dependsOn(zeroFormatter % "test->test;compile->compile")
}
