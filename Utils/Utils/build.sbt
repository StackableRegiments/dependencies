name := "common-utils"
version := "0.1.0"
organization := "io.github.stackableregiments"

scalaVersion := "2.11.5"


resolvers ++= Seq(
  "snapshots"     at "http://oss.sonatype.org/content/repositories/snapshots",
  "releases"        at "http://oss.sonatype.org/content/repositories/releases",
  "mavenCentral"  at  "http://mvnrepository.com/artifact"
)

unmanagedResourceDirectories in Test <+= (baseDirectory) { _ / "src/main/webapp" }

scalacOptions ++= Seq("-deprecation", "-unchecked")

libraryDependencies ++= {
  val liftVersion = "2.6.2"

  Seq(
  //scala
      "org.scala-lang" % "scala-library" % scalaVersion.toString,
  //test
		"org.scalatest" %% "scalatest" % "2.2.5" % "test",
    "org.scalaz.stream" %% "scalaz-stream" % "0.7.+",
		"org.specs2" %% "specs2" % "3.3.1" % "test",
		"org.mockito" % "mockito-core" % "1.9.0" % "test",
  //stream handling  
      "commons-io" % "commons-io" % "1.4",
  //for lift
//      "net.liftweb" %% "lift-util" % liftVersion,
      "net.liftweb" %% "lift-webkit" % liftVersion,
//      "net.liftweb" %% "lift-widgets" % liftVersion,
//      "net.liftweb" %% "lift-mapper" % liftVersion,
  //for http
      "org.apache.httpcomponents" % "httpclient" % "4.1",
  //for html parser
      "net.sourceforge.htmlcleaner" % "htmlcleaner" % "2.2"
/*

    "net.liftweb"       %% "lift-webkit"        % liftVersion        % "compile",
    "com.h2database"    % "h2"   % "1.3.167",
    "commons-io" % "commons-io" % "1.4",
    "org.apache.httpcomponents" % "httpclient" % "4.1",
    "net.liftweb"       %% "lift-mapper"        % liftVersion        % "compile",
    "net.liftweb"       %% "lift-wizard"        % liftVersion        % "compile",
    "net.sourceforge.htmlcleaner" % "htmlcleaner" % "2.2",
    "org.eclipse.jetty" % "jetty-webapp"        % "8.1.7.v20120910"  % "container,test",
    "org.eclipse.jetty.orbit" % "javax.servlet" % "3.0.0.v201112011016" % "container,test" artifacts Artifact("javax.servlet", "jar", "jar"),
    "ch.qos.logback"    % "logback-classic"     % "1.0.6",
    "org.specs2"        %% "specs2"             % "1.12.3"           % "test",
    "net.databinder.dispatch" %% "dispatch-core" % "0.9.5",
    "com.novocode" % "junit-interface" % "0.9" % "test",
    "dom4j" % "dom4j" % "1.6.1",
    "org.apache.poi" % "poi" % "3.9",
    "org.apache.poi" % "poi-ooxml" % "3.9",
    "org.apache.poi" % "poi-ooxml-schemas" % "3.9",
    "org.apache.poi" % "ooxml-schemas" % "1.0",
    "org.apache.xmlbeans" % "xmlbeans" % "2.3.0",
    "jcifs" % "jcifs" % "1.3.17",
    "org.apache.commons" % "commons-io" % "1.3.2"
    */
  )
}

// set the main Scala source directory to be <base>/src
scalaSource in Compile := baseDirectory.value / "src"

// set the Scala test source directory to be <base>/test
scalaSource in Test := baseDirectory.value / "test"

// reduce the maximum number of errors shown by the Scala compiler
//    maxErrors := 20

// increase the time between polling for file changes when using continuous execution
pollInterval := 1000

// append several options to the list of options passed to the Java compiler
javacOptions ++= Seq("-source", "1.5", "-target", "1.5")

// append -deprecation to the options passed to the Scala compiler
scalacOptions += "-deprecation"

    // define the statements initially evaluated when entering 'console', 'consoleQuick', or 'consoleProject'
/*
    initialCommands := """
      |import System.{currentTimeMillis => now}
      |def time[T](f: => T): T = {
      |  val start = now
      |  try { f } finally { println("Elapsed: " + (now - start)/1000.0 + " s") }
      |}""".stripMargin,
*/
    // set the initial commands when entering 'console' or 'consoleQuick', but not 'consoleProject'
//    initialCommands in console := "import myproject._"

    // set the main class for packaging the main jar
    // 'run' will still auto-detect and prompt
    // change Compile to Test to set it for the test jar
//    mainClass in (Compile, packageBin) := Some("myproject.MyMain")

    // set the main class for the main 'run' task
    // change Compile to Test to set it for 'test:run'
//    mainClass in (Compile, run) := Some("myproject.MyMain")

    // add <base>/input to the files that '~' triggers on
//    watchSources += baseDirectory.value / "input"

    // define the repository to publish to
    publishTo := Some("sonatype" at "https://oss.sonatype.org/service/local/staging/deploy/maven2")

    // set Ivy logging to be at the highest level
    ivyLoggingLevel := UpdateLogging.Full

    // disable updating dynamic revisions (including -SNAPSHOT versions)
    offline := true

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

    // disable using the Scala version in output paths and artifacts
//    crossPaths := false

    // fork a new JVM for 'run' and 'test:run'
    fork := true

    // fork a new JVM for 'test:run', but not 'run'
    fork in Test := true

    // add a JVM option to use when forking a JVM for 'run'
    javaOptions += "-Xmx2G"

    // only use a single thread for building
//    parallelExecution := false

    // Execute tests in the current project serially
    //   Tests from other projects may still run concurrently.
//    parallelExecution in Test := false

    // set the location of the JDK to use for compiling Java code.
    // if 'fork' is true, this is used for 'run' as well
//    javaHome := Some(file("/usr/lib/jvm/sun-jdk-1.6"))

    // Use Scala from a directory on the filesystem instead of retrieving from a repository
//    scalaHome := Some(file("/home/user/scala/trunk/"))

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
    persistLogLevel := Level.Debug

    // only show 10 lines of stack traces
    traceLevel := 10

    // only show stack traces up to the first sbt stack frame
    traceLevel := 0

    // add SWT to the unmanaged classpath
//    unmanagedJars in Compile += Attributed.blank(file("/usr/share/java/swt.jar"))
/*
    // publish test jar, sources, and docs
    publishArtifact in Test := true

    // disable publishing of main docs
    publishArtifact in (Compile, packageDoc) := false

    // change the classifier for the docs artifact
    artifactClassifier in packageDoc := Some("doc")
*/
    // Copy all managed dependencies to <build-root>/lib_managed/
    //   This is essentially a project-local cache and is different
    //   from the lib_managed/ in sbt 0.7.x.  There is only one
    //   lib_managed/ in the build root (not per-project).
    retrieveManaged := true

credentials += Credentials(Path.userHome / ".ivy2" / "ivy-credentials")

    // Exclude transitive dependencies, e.g., include log4j without including logging via jdmk, jmx, or jms.
libraryDependencies += "log4j" % "log4j" % "1.2.15" excludeAll(
  ExclusionRule(organization = "com.sun.jdmk"),
  ExclusionRule(organization = "com.sun.jmx"),
  ExclusionRule(organization = "javax.jms")
)
