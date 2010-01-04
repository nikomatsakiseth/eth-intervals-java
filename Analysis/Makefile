JAVA=java
SBT=${JAVA} -Dsbt.log.noformat=true -Xmx256M -jar sbt-launcher-0.5.1.jar
SCALAC=fsc
SCALA=scala
JARFILES=$(wildcard lib/*.jar)
TARGETCLASSPATH=target/classes:target/test-classes
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
	JAVA_OPTS="-cp ${CLASSPATH} -Xbootclasspath/p:lib/jsr308-all-1.04.jar" \
	${SCALA} -cp ${CLASSPATH} org.scalatest.tools.Runner -oW -s \
		ch.ethz.intervals.TestAll
