#!/bin/bash

DIR=src/ch/ethz/intervals/impl

rm -f ${DIR}/IntReduction.java
cp ${DIR}/DoubleReduction.java ${DIR}/IntReduction.java
perl -p -i -e 's/Double/Int/g' ${DIR}/IntReduction.java
perl -p -i -e 's/double/int/g' ${DIR}/IntReduction.java
perl -p -i -e 's/PAD = 2/PAD = 4/g' ${DIR}/IntReduction.java
chmod u-w ${DIR}/IntReduction.java

rm -f ${DIR}/LongReduction.java
cp ${DIR}/DoubleReduction.java ${DIR}/LongReduction.java
perl -p -i -e 's/Double/Long/g' ${DIR}/LongReduction.java
perl -p -i -e 's/double/long/g' ${DIR}/LongReduction.java
chmod u-w ${DIR}/LongReduction.java
