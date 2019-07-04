name := "ObjToSchematic"

version := "prototype-0.1"

scalaVersion := "2.12.8"

//libraryDependencies += "com.github.vickumar1981" %% "stringdistance" % "1.1.1"
//https://github.com/vickumar1981/stringdistance

mainClass in assembly := Some("Main")
assemblyJarName in assembly := s"${name.value}-${version.value}.jar"
assemblyOutputPath in assembly := file(s"./${name.value}-${version.value}.jar")