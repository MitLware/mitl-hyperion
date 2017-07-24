lazy val root = (project in file(".")).
  settings(
    name := "hyperion",
    version := "3.4.1",
    scalaVersion := "2.12.0"
  )

// https://mvnrepository.com/artifact/net.sourceforge.collections/collections-generic
libraryDependencies += "net.sourceforge.collections" % "collections-generic" % "4.01"

libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.1"

// https://mvnrepository.com/artifact/junit/junit
libraryDependencies += "junit" % "junit" % "4.12"

// https://mvnrepository.com/artifact/org.typelevel/machinist_2.11
// libraryDependencies += "org.typelevel" % "machinist_2.12" % "0.6.2"

// https://mvnrepository.com/artifact/org.scalacheck/scalacheck_2.11
// libraryDependencies += "org.scalacheck" % "scalacheck_2.12" % "1.13.5" % "test"

// https://mvnrepository.com/artifact/org.spire-math/spire-macros_2.11
// libraryDependencies += "org.spire-math" % "spire-macros_2.12" % "0.13.0"

// https://mvnrepository.com/artifact/org.spire-math/spire_2.11
// libraryDependencies += "org.spire-math" % "spire_2.12" % "0.13.0"

libraryDependencies += "com.novocode" % "junit-interface" % "0.10" % "test"

libraryDependencies += "org.typelevel" %% "cats" % "0.9.0"

val monocleVersion = "1.4.0"

libraryDependencies ++= Seq(
  "com.github.julien-truffaut" %%  "monocle-core"  % monocleVersion,
  "com.github.julien-truffaut" %%  "monocle-macro" % monocleVersion,
  "com.github.julien-truffaut" %%  "monocle-law"   % monocleVersion % "test"
)

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.14"



