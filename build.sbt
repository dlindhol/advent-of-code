ThisBuild / scalaVersion := "2.13.7"

val catsVersion       = "2.6.1"
val catsEffectVersion = "3.2.9"
val fs2Version        = "3.2.2"
val http4sVersion     = "0.23.6"

lazy val advent_of_code = (project in file("."))
  .settings(
    name := "Advent of Code",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core"   % catsVersion,
      "org.typelevel" %% "cats-effect" % catsEffectVersion,
      "co.fs2"        %% "fs2-core"    % fs2Version,
      "co.fs2"        %% "fs2-io"      % fs2Version,
      "org.scalameta" %% "munit"       % "0.7.29" % Test,
      "org.http4s"    %% "http4s-dsl"          % http4sVersion,
      "org.http4s"    %% "http4s-blaze-server" % http4sVersion,
      "org.http4s"    %% "http4s-blaze-client" % http4sVersion,
      "org.scodec"    %% "scodec-core"         % "1.11.9",
    )
  )
