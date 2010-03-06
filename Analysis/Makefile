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
	rm -rf ${DEBUGLOGS} bin bin-test .scala_dependencies classpath-*.tar.gz

cleaner: clean
	rm -rf classpath-*.tar.gz *java_standard_library.jar

# ___ Building the Java standard library _______________________________
java_standard_library.jar: 				java_standard_library.sh
	./java_standard_library.sh
annotated_java_standard_library.jar: 	java_standard_library.jar \
										annotated_java_standard_library.sh \
										$(wildcard jdk_annotations/*.jaif) 
	./annotated_java_standard_library.sh

# ___ Building the code ________________________________________________
compile:
	mkdir -p bin bin-test
	${SCALAC} -d bin -sourcepath src $$(find src -name '*.scala')
	rsync -r resources/* bin/
	${JAVAC} -d bin-test -sourcepath test $$(find test -name '*.java')
	${SCALAC} -d bin-test -sourcepath test $$(find test -name '*.*')
	
# ___ Running tests ____________________________________________________
# to enable profiling add: ${JPROFILEROPTS} or ${HPROFOPTS} to JAVA_OPTS
# to restrict tests add: -n ch.ethz.intervals.ActivelyDebugging
test: compile annotated_java_standard_library.jar
	rm -rf ${DEBUGLOGS}
	mkdir -p ${DEBUGLOGS}
	JAVA_HOME="${JAVA_HOME}" \
	JAVA_OPTS="-cp ${CLASSPATH} -Xbootclasspath/p:lib/jsr308-all-1.04.jar" \
	${SCALA} -cp ${CLASSPATH} org.scalatest.tools.Runner -c -oW \
		-s ch.ethz.intervals.TestAnalysis \
		-s ch.ethz.intervals.TestTcEnv \
		-s ch.ethz.intervals.TestPlugin
		
