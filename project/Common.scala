import sbt._
import Keys._
import sbtrelease.ReleaseStateTransformations._
import sbtrelease.ReleasePlugin.autoImport._
import xerial.sbt.Sonatype._
import com.typesafe.sbt.pgp.PgpKeys
import dog.DogPlugin.autoImport._

object Common {

  private def gitHash: String = scala.util.Try(
    sys.process.Process("git rev-parse HEAD").lines_!.head
  ).getOrElse("master")

  private[this] val unusedWarnings = (
    "-Ywarn-unused" ::
    "-Ywarn-unused-import" ::
    Nil
  )

  private val tagName = Def.setting{
    s"v${if (releaseUseGlobalVersion.value) (version in ThisBuild).value else version.value}"
  }

  val tagOrHash = Def.setting{
    if(isSnapshot.value) gitHash else tagName.value
  }

  private[this] val compilerOptions = Seq(
    "-deprecation",
    "-unchecked",
    "-Xlint",
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-language:reflectiveCalls"
  )

  private[this] val scala211 = "2.11.11"

  lazy val commonSettings = Seq(
    sonatypeSettings,
    dogCoreSettings
  ).flatten ++ Seq(
    scalaVersion := scala211,
    crossScalaVersions := Seq(scala211, "2.12.2"),
    resolvers += Opts.resolver.sonatypeReleases,
    scalacOptions ++= compilerOptions ++ unusedWarnings,
    releaseTagName := tagName.value,
    releaseProcess := Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      runClean,
      runTest,
      setReleaseVersion,
      commitReleaseVersion,
      UpdateReadme.updateReadmeProcess,
      tagRelease,
      ReleaseStep(
        action = { state =>
          val extracted = Project extract state
          extracted.runAggregated(PgpKeys.publishSigned in Global in extracted.get(thisProjectRef), state)
        },
        enableCrossBuild = true
      ),
      setNextVersion,
      commitNextVersion,
      UpdateReadme.updateReadmeProcess,
      pushChanges
    ),
    credentials ++= PartialFunction.condOpt(sys.env.get("SONATYPE_USER") -> sys.env.get("SONATYPE_PASS")){
      case (Some(user), Some(pass)) =>
        Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", user, pass)
    }.toList,
    organization := "com.github.pocketberserker",
    homepage := Some(url("https://github.com/pocketberserker/scala-zero-formatter")),
    licenses := Seq("MIT License" -> url("http://www.opensource.org/licenses/mit-license.php")),
    pomExtra :=
      <developers>
        <developer>
          <id>pocketberserker</id>
          <name>Yuki Nakayama</name>
          <url>https://github.com/pocketberserker</url>
        </developer>
      </developers>
      <scm>
        <url>git@github.com:pocketberserker/scala-zero-formatter.git</url>
        <connection>scm:git:git@github.com:pocketberserker/scala-zero-formatter.git</connection>
        <tag>{tagOrHash.value}</tag>
      </scm>
    ,
    description := "Implementation of ZeroFormatter in Scala",
    pomPostProcess := { node =>
      import scala.xml._
      import scala.xml.transform._
      def stripIf(f: Node => Boolean) = new RewriteRule {
        override def transform(n: Node) =
          if (f(n)) NodeSeq.Empty else n
      }
      val stripTestScope = stripIf { n => n.label == "dependency" && (n \ "scope").text == "test" }
      new RuleTransformer(stripTestScope).transform(node)(0)
    }
  ) ++ Seq(Compile, Test).flatMap(c =>
    scalacOptions in (c, console) ~= {_.filterNot(unusedWarnings.toSet)}
  )
}
