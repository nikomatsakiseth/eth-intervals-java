JAVA=java
JAVAC=javac
SBT=${JAVA} -Dsbt.log.noformat=true -Xmx256M -jar sbt-launch-0.7.0.jar
SCALAC=scalac -make:changed
SCALA=scala
JARFILES=$(wildcard lib/*.jar)
JARPATH=$(subst .jar ,.jar:,${JARFILES})
CLASSPATH=$(JARPATH):bin:bin-test
LIBDIR=$(CURDIR)/lib
HPROFOPTS=-agentlib:hprof=cpu=samples,depth=6
JPROFILEROPTS=-javaagent:${LIBDIR}/profile.jar -Dprofile.properties=${LIBDIR}/profile.properties
DEBUGLOGS=../debug-logs
STDLIB=java_standard_library.jar

all: test

# ___ Clean ____________________________________________________________
clean:
	rm -rf bin bin-test .scala_dependencies 

# ___ Building the code ________________________________________________
compile:
	mkdir -p bin bin-test
	${JAVAC} -d bin -sourcepath src $$(find src -name '*.java')
	${SCALAC} -deprecation -d bin -sourcepath src $$(find src -name '*.scala')
	
# ___ Running tests ____________________________________________________
# to enable profiling add: ${JPROFILEROPTS} or ${HPROFOPTS} to JAVA_OPTS
# to restrict tests add: -n ch.ethz.intervals.ActivelyDebugging
test: compile
	python test.py $$(find test -name '*.inter')

#test: compile annotated_java_standard_library.jar
#	rm -rf ${DEBUGLOGS}
#	mkdir -p ${DEBUGLOGS}
#	JAVA_HOME="${JAVA_HOME}" \
#	JAVA_OPTS="-cp ${CLASSPATH} -Xbootclasspath/p:lib/jsr308-all-1.04.jar" \
#	${SCALA} -cp ${CLASSPATH} org.scalatest.tools.Runner -c -oW \
#		-n ch.ethz.intervals.ActivelyDebugging \
#		-s ch.ethz.intervals.TestAnalysis \
#		-s ch.ethz.intervals.TestTcEnv \
#		-s ch.ethz.intervals.TestPlugin
		
