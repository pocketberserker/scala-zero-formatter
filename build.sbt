import Build._

lazy val jvmProjects = Seq[ProjectReference](
  zeroFormatterCoreJVM, zeroFormatterMacrosJVM, zeroFormatterJVM, scalazJVM, catsCoreJVM, benchmark
)

lazy val jsProjects = Seq[ProjectReference](
  zeroFormatterCoreJS, zeroFormatterMacrosJS, zeroFormatterJS, scalazJS, catsCoreJS
)

lazy val benchmarkProjects = Seq[ProjectReference](
  benchmark
)

lazy val zeroFormatterCoreJS = zeroFormatterCore.js
lazy val zeroFormatterCoreJVM = zeroFormatterCore.jvm
lazy val zeroFormatterCoreRoot = project.aggregate(zeroFormatterCoreJS, zeroFormatterCoreJVM)
lazy val zeroFormatterMacrosJS = zeroFormatterMacros.js
lazy val zeroFormatterMacrosJVM = zeroFormatterMacros.jvm
lazy val zeroFormatterMacrosRoot = project.aggregate(zeroFormatterMacrosJS, zeroFormatterMacrosJVM)
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
