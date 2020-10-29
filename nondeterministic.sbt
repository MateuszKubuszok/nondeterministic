import sbt._
import Settings._

lazy val root = project.root
  .setName("nondeterministic")
  .setDescription("Build of NonDeterministic")
  .configureRoot
  .aggregate(core)

lazy val core = project.from("core")
  .setName("nondeterministic-core")
  .setDescription("NonDeterministic library")
  .setInitialImport()
  .configureModule
  .configureTests()
  .configureFunctionalTests()
  .configureIntegrationTests()
  .settings(Compile / resourceGenerators += task[Seq[File]] {
    val file = (Compile / resourceManaged).value / "nondeterministic-version.conf"
    IO.write(file, s"version=${version.value}")
    Seq(file)
  })

addCommandAlias("fullTest", ";test;scalastyle")
addCommandAlias("fullCoverageTest", ";coverage;test;coverageReport;coverageAggregate;scalastyle")
