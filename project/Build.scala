import sbt._
import Keys._
import Common._
import org.scalajs.sbtplugin.cross.{CrossType, CrossProject}
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

object Build {

  private[this] val zeroFormatterName = "zero-formatter"
  private[this] val scalazName = "zero-formatter-scalaz"
  private[this] val catsCoreName = "zero-formatter-cats-core"
  val unsafeName = "zero-formatter-unsafe"
  val akkaHttpName = "akka-http-zero-formatter"
  val lz4Name = "zero-formatter-lz4"
  val zstdName = "zero-formatter-zstd"
  val allName = "zero-formatter-all"

  private[this] def module(id: String) =
    CrossProject(id, file(id), CustomCrossType).settings(
      commonSettings
    ).settings(
      scalaJSStage in Test := FastOptStage
    ).jsSettings(
      scalacOptions += {
        val a = (baseDirectory in LocalRootProject).value.toURI.toString
        val g = "https://raw.githubusercontent.com/pocketberserker/scala-zero-formatter/" + Common.tagOrHash.value
        s"-P:scalajs:mapSourceURI:$a->$g/"
      }
    )

  val modules: List[String] = (
    zeroFormatterName ::
    scalazName ::
    catsCoreName ::
    unsafeName ::
    akkaHttpName ::
    lz4Name ::
    zstdName ::
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

  private[this] val dogVersion = "0.8.0"
  private[this] val catsVersion = "0.9.0"

  lazy val zeroFormatter = module(zeroFormatterName).settings(
    name := zeroFormatterName,
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "spire" % "0.14.1",
      "com.chuusai" %%% "shapeless" % "2.3.2",
      "com.github.pocketberserker" %%% "dog" % dogVersion % "test",
      "com.github.pocketberserker" %%% "dog-props" % dogVersion % "test"
    )
  ).jsSettings(
    libraryDependencies += "org.scala-js" %%% "scalajs-java-time" % "0.2.0"
  )

  lazy val scalaz = module("scalaz").settings(
    name := scalazName,
    libraryDependencies ++= Seq(
      "org.scalaz" %%% "scalaz-core" % "7.2.12",
      "com.github.scalaprops" %%% "scalaprops-scalazlaws" % "0.4.2" % "test"
    )
  ).dependsOn(zeroFormatter % "test->test;compile->compile")

  lazy val catsCore = module("cats-core").settings(
    name := catsCoreName,
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % catsVersion
    )
  ).dependsOn(zeroFormatter % "test->test;compile->compile")
}
