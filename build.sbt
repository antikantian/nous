name := "nous"

version := "0.1.0"

isSnapshot := true

lazy val versions = new {
  val bloomfilter = "0.4.0"
  val cats = "0.7.2"
  val debox = "0.7.3"
  val dogs = "0.3.1"
  val freasy = "0.4.1"
  val fs2 = "0.9.0-RC2"
  val fs2Cats = "0.1.0-RC2"
  val imp = "0.2.1"
  val quiver = "5.4.9"
  val scalaj = "2.3.0"
  val scrimage = "2.1.7"
  val scalacheck = "1.13.2"
  val scalameter = "0.7"
  val scalatest = "3.0.0"
  val shapeless = "2.3.2"
  val simulacrum = "0.9.0"
  val spire = "0.11.0"
}

lazy val functionalibs = Seq(
  "org.typelevel"          %% "cats"          % versions.cats,
  "com.chuusai"            %% "shapeless"     % versions.shapeless,
  "com.thangiee"           %% "freasy-monad"  % versions.freasy,
  "com.github.mpilquist"   %% "simulacrum"    % versions.simulacrum,
  "org.spire-math"         %% "imp"           % versions.imp % "provided"
)

lazy val datastructs = Seq(
  "org.typelevel"              %% "dogs-core"     % versions.dogs,
  "org.spire-math"             %% "debox"         % versions.debox,
  "com.github.alexandrnikitin" %% "bloom-filter" % versions.bloomfilter
)

lazy val mathlibs = Seq(
  "org.spire-math"         %% "spire"            % versions.spire
)

lazy val streamlibs = Seq(
  "co.fs2"                 %% "fs2-core"      % versions.fs2,
  "co.fs2"                 %% "fs2-io"        % versions.fs2,
  "co.fs2"                 %% "fs2-cats"      % versions.fs2Cats
)

lazy val testlibs = Seq(
  "org.scalacheck" %% "scalacheck" % versions.scalacheck,
  "org.scalatest" %% "scalatest" % versions.scalatest,
  "com.storm-enroute" %% "scalameter-core" % versions.scalameter
)

lazy val imagelibs = Seq(
  "com.sksamuel.scrimage"  %% "scrimage-core" % versions.scrimage
)

lazy val graphlibs = Seq(
  "io.verizon.quiver" %% "core" % versions.quiver
)

lazy val httplibs = Seq(
  "org.scalaj" %% "scalaj-http" % versions.scalaj
)

lazy val netlib = Seq(
  "com.github.fommil.netlib" % "all" % "1.1.2" pomOnly()
)

lazy val commonSettings = Seq(
  organization := "co.quine",
  scalaVersion := "2.11.8",
  crossScalaVersions := Seq("2.11.8"),
  scalacOptions ++= Seq(
        "-encoding", "UTF-8",
        "-language:postfixOps",
        "-language:higherKinds",
        "-language:existentials",
        "-language:implicitConversions",
        "-language:experimental.macros"
      ),

  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.8.0"),

  addCompilerPlugin("com.milessabin" % "si2712fix-plugin_2.11.8" % "1.2.0"),

  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),

  libraryDependencies ++= (
    functionalibs ++
      datastructs ++
      mathlibs ++
      streamlibs ++
      testlibs ++
      imagelibs ++
      graphlibs ++
      httplibs ++
      netlib),

  resolvers ++= Seq(
    Resolver.mavenLocal,
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases"),
    Resolver.typesafeRepo("snapshots"),
    Resolver.jcenterRepo
  ),

  classpathTypes += "maven-plugin",

  evictionWarningOptions in update := EvictionWarningOptions.default
    .withWarnTransitiveEvictions(false)
    .withWarnDirectEvictions(false)
    .withWarnScalaVersionEviction(false)
)

lazy val root = project.in(file(".")).
  settings(commonSettings)

lazy val core = project.in(file("nous-core")).
  settings(commonSettings:_*).
  settings(name := "nous-core")

