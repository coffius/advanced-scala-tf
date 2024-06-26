import sbt._
object Dependencies {
  type Deps = List[ModuleID]

  val cats       = "org.typelevel" %% "cats-core"        % "2.10.0"
  val catsEffect = "org.typelevel" %% "cats-effect"      % "3.5.1"
  val catsMtl    = "org.typelevel" %% "cats-mtl"         % "1.3.0"
  val zioInterop = "dev.zio"       %% "zio-interop-cats" % "23.1.0.0"
  val zioPrelude = "dev.zio"       %% "zio-prelude"      % "1.0.0-RC23"
  val monocle    = "dev.optics"    %% "monocle-core"     % "3.2.0"

  val all: Deps = List(cats, catsEffect, catsMtl, zioInterop, zioPrelude, monocle)
}
