import Build._

lazy val jvmProjects = Seq[ProjectReference](
  zeroFormatterJVM, scalazJVM, catsCoreJVM, unsafe, akkaHttp, lz4, zstd, benchmark
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
    "com.typesafe.akka" %% "akka-http" % "10.0.6",
    "org.scalatest" %% "scalatest" % "3.0.1" % "test"
  )
).dependsOn(zeroFormatterJVM, unsafe)

lazy val lz4 = Project("lz4", file("lz4")).settings(
  Common.commonSettings
).settings(
  name := lz4Name,
  libraryDependencies ++= Seq(
    "net.jpountz.lz4" % "lz4" % "1.3.0"
  )
).dependsOn(zeroFormatterJVM % "compile->compile;test->test", unsafe)

lazy val zstd = Project("zstd", file("zstd")).settings(
  Common.commonSettings
).settings(
  name := zstdName,
  libraryDependencies ++= Seq(
    "com.github.luben" % "zstd-jni" % "1.2.0"
  )
).dependsOn(zeroFormatterJVM % "compile->compile;test->test", unsafe)

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
).dependsOn(
  catsCoreJVM,
  zeroFormatterJVM % "test->test",
  unsafe,
  lz4,
  zstd
).enablePlugins(JmhPlugin)

lazy val rootJS = project.aggregate(jsProjects: _*)
lazy val rootJVM = project.aggregate(jvmProjects: _*)
