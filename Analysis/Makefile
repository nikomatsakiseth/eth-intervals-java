JAVA=java
JAVAC=javac
SBT=${JAVA} -Dsbt.log.noformat=true -Xmx256M -jar sbt-launch-0.7.0.jar
SCALAC=scalac -make:changed
SCALA=scala
JARFILES=$(wildcard lib/*.jar)
JARPATH=$(subst .jar ,.jar:,${JARFILES})
CLASSPATH=$(JARPATH):bin:bin-test
PROFILEOPTS=-agentlib:hprof=cpu=samples,depth=6
DEBUGLOGS=../debug-logs

.PHONY: all clean compile test

all: test

clean:
	rm -rf ${DEBUGLOGS} bin bin-test .scala_dependencies

compile:
	mkdir -p bin bin-test
	${SCALAC} -d bin -sourcepath src $$(find src -name '*.scala')
	rsync -r resources/* bin/
	${JAVAC} -d bin-test -sourcepath test $$(find test -name '*.java')
	${SCALAC} -d bin-test -sourcepath test $$(find test -name '*.*')
		
test: compile
	rm -rf ${DEBUGLOGS}
	mkdir -p ${DEBUGLOGS}
	JAVA_HOME="${JAVA_HOME}" \
	JAVA_OPTS="-cp ${CLASSPATH} -Xbootclasspath/p:lib/jsr308-all-1.04.jar" \
	${SCALA} -cp ${CLASSPATH} org.scalatest.tools.Runner -oW \
	    -n ch.ethz.intervals.ActivelyDebugging \
		-s ch.ethz.intervals.TestPlugin
