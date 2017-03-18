import Build._

lazy val jvmProjects = Seq[ProjectReference](
  zeroFormatterJVM, scalazJVM, catsCoreJVM, unsafe, akkaHttp, benchmark
)

lazy val jsProjects = Seq[ProjectReference](
  zeroFormatterJS, scalazJS, catsCoreJS
)

lazy val zeroFormatterJS = zeroFormatter.js
lazy val zeroFormatterJVM = zeroFormatter.jvm
lazy val zeroFormatterRoot = project.aggregate(zeroFormatterJS, zeroFormatterJVM)
lazy val scalazJS = scalaz.js
lazy val scalazJVM = scalaz.jvm
lazy val scalazRoot = project.aggregate(scalazJS, scalazJVM)
lazy val catsCoreJS = catsCore.js
lazy val catsCoreJVM = catsCore.jvm
lazy val catsCoreRoot = project.aggregate(catsCoreJS, catsCoreJVM)

lazy val unsafe = Project("unsafe", file("unsafe")).settings(
  Common.commonSettings
).settings(
  name := unsafeName
).dependsOn(zeroFormatterJVM % "compile->compile;test->test")

lazy val akkaHttp = Project("akka-http", file("akka-http")).settings(
  Common.commonSettings
).settings(
  name := akkaHttpName,
  libraryDependencies ++= Seq(
    "com.typesafe.akka" %% "akka-http" % "10.0.5",
    "org.scalatest" %% "scalatest" % "3.0.1" % "test"
  )
).dependsOn(zeroFormatterJVM, unsafe)

val root = Project("root", file(".")).settings(
  Common.commonSettings
).settings(
  name := allName,
  packagedArtifacts := Map.empty
).aggregate(
  jvmProjects ++ jsProjects : _*
)

lazy val benchmark = Project("benchmark", file("benchmark")).settings(
  Common.commonSettings
).settings(
  name := "benchmark",
  publishArtifact := false,
  publish := {},
  publishLocal := {}
).enablePlugins(JmhPlugin).dependsOn(catsCoreJVM, zeroFormatterJVM % "test->test", unsafe)

lazy val rootJS = project.aggregate(jsProjects: _*)
lazy val rootJVM = project.aggregate(jvmProjects: _*)
