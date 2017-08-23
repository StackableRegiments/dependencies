name := "metl-data-migrator"
version := "0.2.0"
organization := "io.github.stackableregiments"

val scalaVersionString = "2.11.5"

scalaVersion := scalaVersionString

ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) }

resolvers ++= Seq(
  "snapshots"     at "https://oss.sonatype.org/content/repositories/snapshots",
  "releases"        at "https://oss.sonatype.org/content/repositories/releases"
)

mainClass := Some("com.metl.datamigrator.Main")

//fork in run := true

scalacOptions ++= Seq("-deprecation", "-unchecked")

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.+"

libraryDependencies ++= {
  val liftVersion = "2.6.2"
  Seq(
    "net.databinder.dispatch" %% "dispatch-core" % "0.11.2",
    "org.scala-lang" % "scala-library" % scalaVersionString,
    "org.scalatest" %% "scalatest" % "2.2.5" % "test",
    "org.scalaz.stream" %% "scalaz-stream" % "0.7.+",
    "org.specs2" %% "specs2" % "3.3.1" % "test",
    "org.mockito" % "mockito-core" % "1.9.0" % "test",
    "commons-io" % "commons-io" % "1.4",
    "javax.mail" % "mail" % "1.4",
    "net.liftweb" %% "lift-mapper" % liftVersion,
    "net.liftweb" %% "lift-webkit" % liftVersion,
    "net.liftweb" %% "lift-mongodb" % liftVersion,
    "net.liftweb" %% "lift-mongodb-record" % liftVersion,
    "org.seleniumhq.selenium" % "selenium-java" % "2.8.0",
    "io.github.stackableregiments" %% "common-utils" % "1.1.+",
    "io.github.stackableregiments" %% "metl2011" % "3.11.+",
    "io.github.stackableregiments" %% "metldata" % "3.20.+",
    "io.github.stackableregiments" %% "persisted-metl" % "3.10.+",
    "io.github.stackableregiments" %% "metl-h2" % "3.27.+",
    "mysql" % "mysql-connector-java" % "5.1.38"
  )
}.map(_.excludeAll(ExclusionRule(organization = "org.slf4j")).exclude("com.sun.jdmk","jmxtools").exclude("javax.jms","jms").exclude("com.sun.jmx","jmxri"))

javacOptions ++= Seq("-source", "1.8", "-target", "1.8")

// append -deprecation to the options passed to the Scala compiler
scalacOptions += "-deprecation"

// set Ivy logging to be at the highest level
ivyLoggingLevel := UpdateLogging.Full

// disable updating dynamic revisions (including -SNAPSHOT versions)
offline := false

// set the prompt (for this build) to include the project id.
shellPrompt in ThisBuild := { state => Project.extract(state).currentRef.project + "> " }

// set the prompt (for the current project) to include the username
shellPrompt := { state => System.getProperty("user.name") + "> " }

// disable printing timing information, but still print [success]
showTiming := true

// disable printing a message indicating the success or failure of running a task
showSuccess := true

// change the format used for printing task completion time
timingFormat := {
  import java.text.DateFormat
  DateFormat.getDateTimeInstance(DateFormat.SHORT, DateFormat.SHORT)
}

testOptions in Test += Tests.Argument("-eI")

// add a JVM option to use when forking a JVM for 'run'
//javaOptions += "-Xmx2G"
//javaOptions += "-Xmx3072m"
javaOptions += "-Xmx6172m"
javaOptions += "-Dmetlx.configurationFile=./config/configuration.local.xml"
javaOptions += "-Dlogback.configurationFile=config/logback.xml"

// don't aggregate clean (See FullConfiguration for aggregation details)
aggregate in clean := false

// only show warnings and errors on the screen for compilations.
//  this applies to both test:compile and compile and is Info by default
logLevel in compile := Level.Warn

// only show warnings and errors on the screen for all tasks (the default is Info)
//  individual tasks can then be more verbose using the previous setting
logLevel := Level.Warn

// only store messages at info and above (the default is Debug)
//   this is the logging level for replaying logging with 'last'
//persistLogLevel := Level.Debug

// only show 10 lines of stack traces
traceLevel := 10