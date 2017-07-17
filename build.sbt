lazy val root = (project in file(".")).
  settings(
    name := "hyperion",
    version := "3.4",
    scalaVersion := "2.12.0"
  )

// https://mvnrepository.com/artifact/net.sourceforge.collections/collections-generic
libraryDependencies += "net.sourceforge.collections" % "collections-generic" % "4.01"

// https://mvnrepository.com/artifact/junit/junit
libraryDependencies += "junit" % "junit" % "4.12"

// https://mvnrepository.com/artifact/org.typelevel/machinist_2.11
libraryDependencies += "org.typelevel" % "machinist_2.12" % "0.6.2"

// https://mvnrepository.com/artifact/org.scalacheck/scalacheck_2.11
libraryDependencies += "org.scalacheck" % "scalacheck_2.12" % "1.13.5" % "test"

// https://mvnrepository.com/artifact/org.spire-math/spire-macros_2.11
libraryDependencies += "org.spire-math" % "spire-macros_2.12" % "0.13.0"

// https://mvnrepository.com/artifact/org.spire-math/spire_2.11
libraryDependencies += "org.spire-math" % "spire_2.12" % "0.13.0"

libraryDependencies += "com.novocode" % "junit-interface" % "0.10" % "test"