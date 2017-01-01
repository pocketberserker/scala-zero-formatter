import Build._

lazy val jvmProjects = Seq[ProjectReference](
  zeroFormatterJVM, scalazJVM
)

lazy val jsProjects = Seq[ProjectReference](
  zeroFormatterJS, scalazJS
)

lazy val zeroFormatterJS = zeroFormatter.js
lazy val zeroFormatterJVM = zeroFormatter.jvm
lazy val zeroFormatterRoot = project.aggregate(zeroFormatterJS, zeroFormatterJVM)
lazy val scalazJS = scalaz.js
lazy val scalazJVM = scalaz.jvm
lazy val scalazRoot = project.aggregate(scalazJS, scalazJVM)

val root = Project("root", file(".")).settings(
  Common.commonSettings
).settings(
  name := allName,
  packagedArtifacts := Map.empty
).aggregate(
  jvmProjects ++ jsProjects : _*
)

lazy val rootJS = project.aggregate(jsProjects: _*)
lazy val rootJVM = project.aggregate(jvmProjects: _*)
