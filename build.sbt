import Build._

lazy val jvmProjects = Seq[ProjectReference](
  zeroFormatterJVM, scalazJVM, catsCoreJVM, benchmark
)

lazy val jsProjects = Seq[ProjectReference](
  zeroFormatterJS, scalazJS, catsCoreJS
)

lazy val benchmarkProjects = Seq[ProjectReference](
  benchmark
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

val root = Project("root", file(".")).settings(
  Common.commonSettings
).settings(
  name := allName,
  packagedArtifacts := Map.empty
).aggregate(
  jvmProjects ++ jsProjects ++ benchmarkProjects : _*
)

lazy val benchmark = Project("benchmark", file("benchmark")).settings(
  Common.commonSettings
).settings(
  name := "benchmark",
  publishArtifact := false,
  publish := {},
  publishLocal := {}
).enablePlugins(JmhPlugin).dependsOn(catsCoreJVM, zeroFormatterJVM % "test->test")

lazy val rootJS = project.aggregate(jsProjects: _*)
lazy val rootJVM = project.aggregate(jvmProjects: _*)
