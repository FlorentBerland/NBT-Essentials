name := "ObjToSchematic"

version := "prototype-0.1"

scalaVersion := "2.12.8"

mainClass in assembly := Some("Main")
assemblyJarName in assembly := s"${name.value}-${version.value}.jar"
assemblyOutputPath in assembly := file(s"./${name.value}-${version.value}.jar")