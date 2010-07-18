.PHONY = help clean compile unit test

JAVA=java
JAVAC=javac
SBT=${JAVA} -Dsbt.log.noformat=true -Xmx256M -jar sbt-launch-0.7.0.jar
# -Xfatal-warnings -unchecked
SCALAC=scalac -make:changed 
SCALA=scala
JARFILES=$(wildcard lib/*.jar)
JARPATH=$(subst .jar ,.jar:,${JARFILES})
GENERATEDPY=$(wildcard src/harmonic/compiler/*Gen.py)
GENERATEDJAVA=$(subst Gen.py,Gen.java,${GENERATEDPY})
CLASSPATH=$(JARPATH):bin
LIBDIR=$(CURDIR)/lib
HPROFOPTS=-agentlib:hprof=cpu=samples,depth=6
JPROFILEROPTS=-javaagent:${LIBDIR}/profile.jar -Dprofile.properties=${LIBDIR}/profile.properties
DEBUGLOGS=../debug-logs
STDLIB=java_standard_library.jar

all: compile

help:
	@echo "The following make targets are defined:"
	@echo "clean:    removes all files"
	@echo "compile:  builds the Harmonic source files and Harmonic.jar"
	@echo "unit:     runs the unit test suite, after compiling"
	@echo "test:     runs the harmonic test suite, after compiling"
	@echo
	@echo "compile is the default target."

# ___ Clean ____________________________________________________________
clean:
	rm -rf harmonic.jar bin bin-test .scala_dependencies 
	find test -name '*.test' -exec rm '{}' ';'

# ___ Building the code ________________________________________________
%Gen.java: %Gen.py
	python $< > $@
	
compile: ${GENERATEDJAVA}
	mkdir -p bin bin-test
	${JAVAC} -cp ${CLASSPATH} -d bin -sourcepath src $$(find src -name '*.java')
	${SCALAC} -cp ${CLASSPATH} -deprecation -d bin -sourcepath src $$(find src -name '*.scala')
	ant jar

# ___ Unit tests _______________________________________________________
unit: compile
	${SCALA} -cp ${CLASSPATH} harmonic.compiler.inference.TestNetwork
	
# ___ Running tests ____________________________________________________
# To keep .test files, add "--preserve" to test.py invocation
test-src: compile
	rm -rf bin-test/*
	${JAVAC} -cp ${CLASSPATH} -d bin-test -sourcepath . $$(find test -name '*.java')

test: test-src
	time python test.py --preserve $$(find test -name '*.harm')
	#python test.py --preserve test/bytecode/BytecodeInline.harm
