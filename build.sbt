name := "EZOne-server"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  "mysql" % "mysql-connector-java" % "5.1.18",
  "com.github.mumoshu" %% "play2-memcached" % "0.3.0.2",
  "io.spray" %%  "spray-json" % "1.2.5"
)

resolvers ++= Seq("spray" at "http://repo.spray.io/",
"Spy Repository" at "http://files.couchbase.com/maven2")

play.Project.playScalaSettings
