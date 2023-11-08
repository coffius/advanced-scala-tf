import sbt._
object Dependencies {
  type Deps = List[ModuleID]

  val cats       = "org.typelevel" %% "cats-core"        % "2.10.0"
  val catsEffect = "org.typelevel" %% "cats-effect"      % "3.5.1"
  val catsMtl    = "org.typelevel" %% "cats-mtl"         % "1.3.0"
  val zioInterop = "dev.zio"       %% "zio-interop-cats" % "23.1.0.0"

  val allCats: Deps = List(cats, catsEffect, catsMtl, zioInterop)
}
