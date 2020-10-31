import sbt._

import Dependencies._

object Dependencies {

  // scala version
  val scalaOrganization = "org.scala-lang"
  val scalaVersion = "2.13.3"
  val crossScalaVersions = Seq("2.12.10", "2.13.3")

  // libraries versions
  val catsVersion = "2.2.0"
  val specs2Version = "4.10.3"

  // resolvers
  val resolvers = Seq(
    Resolver sonatypeRepo "public",
    Resolver typesafeRepo "releases"
  )

  // compiler plugins
  val kindProjector = "org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full
  val macroParadise = "org.scalamacros" % "paradise" % "2.1.1" cross sbt.CrossVersion.full
  // functional libraries
  val cats = "org.typelevel" %% "cats-core" % catsVersion
  val catsFree = "org.typelevel" %% "cats-free" % catsVersion
  val catsLaws = "org.typelevel" %% "cats-laws" % catsVersion
  val newtype = "io.estatico" %% "newtype" % "0.4.4"
  // testing
  val spec2Core = "org.specs2" %% "specs2-core" % specs2Version
  val spec2Scalacheck = "org.specs2" %% "specs2-scalacheck" % specs2Version
}

trait Dependencies {

  val scalaOrganizationUsed = scalaOrganization
  val scalaVersionUsed = scalaVersion
  val crossScalaVersionsUsed = crossScalaVersions

  // resolvers
  val commonResolvers = resolvers

  val mainDeps = Seq(
    cats,
    catsFree,
    newtype
  )

  val testDeps = Seq(catsLaws, spec2Core, spec2Scalacheck)

  implicit final class ProjectRoot(project: Project) {

    def root: Project = project in file(".")
  }

  implicit final class ProjectFrom(project: Project) {

    private val commonDir = "modules"

    def from(dir: String): Project = project in file(s"$commonDir/$dir")
  }

  implicit final class DependsOnProject(project: Project) {

    private val testConfigurations = Set("test", "fun", "it")

    private def findCompileAndTestConfigs(p: Project) =
      (p.configurations.map(_.name).toSet intersect testConfigurations) + "compile"

    private val thisProjectsConfigs = findCompileAndTestConfigs(project)

    private def generateDepsForProject(p: Project) =
      p % (thisProjectsConfigs intersect findCompileAndTestConfigs(p) map (c => s"$c->$c") mkString ";")

    def compileAndTestDependsOn(projects: Project*): Project =
      project dependsOn (projects.map(generateDepsForProject): _*)
  }

}
