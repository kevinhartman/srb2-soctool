# Get SBT / Scala build env image and run assembly (compile soctool to a fat JAR)
docker run --mount src="$(pwd)",target=/opt/workspace,type=bind eed3si9n/sbt:jdk11-alpine \
  'set assemblyOutputPath in assembly := new File("./target/soctool.jar")' \
  assembly