JAVA=java
JAVAC=javac
SBT=${JAVA} -Dsbt.log.noformat=true -Xmx256M -jar sbt-launch-0.7.0.jar
SCALAC=scalac -make:changed
SCALA=scala
JARFILES=$(wildcard lib/*.jar)
JARPATH=$(subst .jar ,.jar:,${JARFILES})
GENERATEDPY=$(wildcard src/inter/compiler/*Gen.py)
GENERATEDJAVA=$(subst Gen.py,Gen.java,${GENERATEDPY})
CLASSPATH=$(JARPATH):bin
LIBDIR=$(CURDIR)/lib
HPROFOPTS=-agentlib:hprof=cpu=samples,depth=6
JPROFILEROPTS=-javaagent:${LIBDIR}/profile.jar -Dprofile.properties=${LIBDIR}/profile.properties
DEBUGLOGS=../debug-logs
STDLIB=java_standard_library.jar

all: test

# ___ Clean ____________________________________________________________
clean:
	rm -rf bin bin-test .scala_dependencies 
	find test -name '*.test' -exec rm '{}' ';'

# ___ Building the code ________________________________________________
%Gen.java: %Gen.py
	python $< > $@

compile: ${GENERATEDJAVA}
	mkdir -p bin bin-test
	${JAVAC} -cp ${CLASSPATH} -d bin -sourcepath src $$(find src -name '*.java')
	${SCALAC} -cp ${CLASSPATH} -deprecation -d bin -sourcepath src $$(find src -name '*.scala')
	
# ___ Running tests ____________________________________________________
# To keep .test files, add "--preserve" to test.py invocation
test: compile
	rm -rf bin-test/*
	python test.py --preserve $$(find test -name '*.harm')

		