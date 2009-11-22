#!/bin/bash

rm -f src/ch/ethz/intervals/IntReduction.java
cp src/ch/ethz/intervals/DoubleReduction.java src/ch/ethz/intervals/IntReduction.java
perl -p -i -e 's/Double/Int/g' src/ch/ethz/intervals/IntReduction.java
perl -p -i -e 's/double/int/g' src/ch/ethz/intervals/IntReduction.java
perl -p -i -e 's/PAD = 2/PAD = 4/g' src/ch/ethz/intervals/IntReduction.java
chmod u-w src/ch/ethz/intervals/IntReduction.java

rm -f src/ch/ethz/intervals/LongReduction.java
cp src/ch/ethz/intervals/DoubleReduction.java src/ch/ethz/intervals/LongReduction.java
perl -p -i -e 's/Double/Long/g' src/ch/ethz/intervals/LongReduction.java
perl -p -i -e 's/double/long/g' src/ch/ethz/intervals/LongReduction.java
chmod u-w src/ch/ethz/intervals/LongReduction.java
