JAVA=java
SBT=${JAVA} -Dsbt.log.noformat=true -Xmx256M -jar sbt-launcher-0.5.1.jar
SCALAC=fsc
SCALA=scala
JARFILES=$(wildcard analysis/lib/*.jar)
TARGETCLASSPATH=analysis/target/classes:analysis/target/test-classes:runtime/target/classes:jndm/target/classes
CLASSPATH=$(subst .jar ,.jar:,${JARFILES}):${TARGETCLASSPATH}

.PHONY: all clean compile test

all: test

clean:
	$(SBT) clean

compile:
	$(SBT) compile test-compile 2>&1 | col -b

test:
	$(SBT) test-compile
	JAVA_HOME="${JAVA_HOME}" \
	JAVA_OPTS="-cp ${CLASSPATH} -Xbootclasspath/p:analysis/lib/javac-0.9.2.jar" \
	${SCALA} -cp ${CLASSPATH} org.scalatest.tools.Runner -o -s \
		ch.ethz.intervals.TestAll	
