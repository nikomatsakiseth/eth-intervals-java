JAVA=java
SBT=${JAVA} -Dsbt.log.noformat=true -Xmx256M -jar sbt-launch-0.7.0.jar
SCALAC=fsc
SCALA=scala
JARFILES=$(wildcard lib/*.jar)
TARGETCLASSPATH=target/classes:target/test-classes
CLASSPATH=$(subst .jar ,.jar:,${JARFILES}):${TARGETCLASSPATH}

.PHONY: all clean compile test

all: test

clean:
	rm -rf debug-logs/*
	$(SBT) clean

compile:
	$(SBT) compile test-compile 2>&1 | col -b
	cp src/ch/ethz/intervals/messages.properties target/classes/ch/ethz/intervals

test: compile
	rm -rf debug-logs/*
	JAVA_HOME="${JAVA_HOME}" \
	JAVA_OPTS="-cp ${CLASSPATH} -Xbootclasspath/p:lib/jsr308-all-1.04.jar" \
	${SCALA} -cp ${CLASSPATH} org.scalatest.tools.Runner -oW -s \
		ch.ethz.intervals.TestAll
