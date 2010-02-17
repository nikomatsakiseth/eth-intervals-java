JAVA=java
SBT=${JAVA} -Dsbt.log.noformat=true -Xmx256M -jar sbt-launch-0.7.0.jar
SCALAC=scalac -make:immediate
SCALA=scala
JARFILES=$(wildcard lib/*.jar)
CLASSPATH=$(subst .jar ,.jar:,${JARFILES}):bin:bin-test

.PHONY: all clean compile test

all: test

clean:
	rm -rf debug-logs bin bin-test

compile:
	mkdir -p bin bin-test
	${SCALAC} -d bin -sourcepath src src/ch/ethz/intervals/IntervalsChecker.scala
	${SCALAC} -d bin-test -sourcepath src:test test/ch/ethz/intervals/TestAll.scala
	rsync -r resources/* bin/
		
test: compile
	rm -rf debug-logs/*
	JAVA_HOME="${JAVA_HOME}" \
	JAVA_OPTS="-cp ${CLASSPATH} -Xbootclasspath/p:lib/jsr308-all-1.04.jar" \
	${SCALA} -cp ${CLASSPATH} org.scalatest.tools.Runner -oW -s \
		ch.ethz.intervals.TestAll
