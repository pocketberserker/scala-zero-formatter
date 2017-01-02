addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.0.0")
addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.3")
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "0.4.0")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.14")
addSbtPlugin("com.github.pocketberserker" % "sbt-dog" % "0.1.1")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.2.20")

scalacOptions ++= (
  "-deprecation" ::
  "-unchecked" ::
  "-Xlint" ::
  "-language:existentials" ::
  "-language:higherKinds" ::
  "-language:implicitConversions" ::
  "-Yno-adapted-args" ::
  Nil
)
