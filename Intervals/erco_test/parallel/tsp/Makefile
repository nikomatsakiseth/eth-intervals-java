#
# Copyright (C) 1998 by ETHZ/INF/CS
# All rights reserved
# 
# $Id: Makefile 9 2001-02-21 11:23:59Z praun $
#
# 03/02/99  cvp
#
#

MAKEDIR = $(ERCO_HOME)/makefiles
include $(MAKEDIR)/test.make
include $(MAKEDIR)/common.make

.PHONY : all
all: this_all

.PHONY : clean
clean: this_clean

JAVA_FILES =\
	sa/macro/tsp/PrioQElement.java\
	sa/macro/tsp/TourElement.java\
	sa/macro/tsp/Tsp.java\
        sa/macro/tsp/TspSolver.java

CLASS_FILES =\
	sa/macro/tsp/PrioQElement.class\
	sa/macro/tsp/TourElement.class\
	sa/macro/tsp/Tsp.class\
        sa/macro/tsp/TspSolver.class

EXE = Tsp
MAINCLASS = sa.macro.tsp.Tsp

.PHONY : this_all
this_all:this_erco this_rd this_rdstats this_gcj

.PHONY : this_test
this_test: TEST_EXE = ./$(EXE)
this_test: TEST_DIR = ./
this_test: TEST_ARGS = tspfiles/map16c 2
this_test: test

.PHONY : this_ctstats
this_ctstats: ASM_FILES = $(CLASS_FILES:.class=.rd.s)
this_ctstats: TARGET = $(EXE)$(ERCO_RD_TAG)
this_ctstats: ctstats

.PHONY : this_erco
this_erco: ASM_FILES = $(CLASS_FILES:.class=.s)
this_erco: TARGET = $(EXE)$(ERCO_TAG)
this_erco: LDFLAGS = --main=$(MAINCLASS) -o $(EXE)$(ERCO_TAG)
this_erco: $(JAVA_FILES:.java=.class) $(CLASS_FILES:.class=.s) $(EXE)$(ERCO_TAG)

.PHONY : this_rd
this_rd: ASM_FILES = $(CLASS_FILES:.class=.rd.s)
this_rd: TARGET = $(EXE)$(ERCO_RD_TAG)
this_rd: LDFLAGS = --main=$(MAINCLASS) -o $(EXE)$(ERCO_RD_TAG)
this_rd: $(JAVA_FILES:.java=.class) $(CLASS_FILES:.class=.rd.s) $(EXE)$(ERCO_RD_TAG)

.PHONY : this_rdstats
this_rdstats: ASM_FILES = $(CLASS_FILES:.class=.rdstats.s)
this_rdstats: TARGET = $(EXE)$(ERCO_RDSTATS_TAG)
this_rdstats: LDFLAGS = --main=$(MAINCLASS) -o $(EXE)$(ERCO_RDSTATS_TAG)
this_rdstats: $(JAVA_FILES:.java=.class) $(CLASS_FILES:.class=.rdstats.s) $(EXE)$(ERCO_RDSTATS_TAG)

.PHONY : this_gcj 
this_gcj: LDFLAGS = --main=$(MAINCLASS) -o $(EXE)$(GCJ_TAG)
this_gcj: TARGET = $(EXE)$(GCJ_TAG)
this_gcj: $(JAVA_FILES:.java=.class) $(EXE)$(GCJ_TAG)

.PHONY : this_javap
this_javap: $(CLASS_FILES:.class=.jp)

.PHONY : this_clean
this_clean:
	@@$(RM) $(CLASS_FILES:.class=.s) ;\
	$(RM) $(CLASS_FILES:.class=.out); \
	$(RM) $(CLASS_FILES:.class=.rd.s) ;\
	$(RM) $(CLASS_FILES:.class=.rd.out); \
	$(RM) $(CLASS_FILES:.class=.rdstats.s) ;\
	$(RM) $(CLASS_FILES:.class=.rdstats.out); \
	$(RM) $(CLASS_FILES:.class=.jp) ;\
	$(RM) $(CLASS_FILES:.class=.sym) ;\
	$(RM) \
		$(EXE)$(ERCO_TAG) \
		$(EXE)$(ERCO_RD_TAG)\
		$(EXE)$(ERCO_RDSTATS_TAG)\
		$(EXE)$(GCJ_TAG) ;\
	$(RM) *.o






