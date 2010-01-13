#
# Copyright (C) 1998 by ETHZ/INF/CS
# All rights reserved
# 
# $Id: Makefile 98 2001-03-18 17:39:24Z praun $
#
# 03/02/99  cvp
#
#

MAKEDIR = $(ERCO_HOME)/makefiles
include $(MAKEDIR)/common.make
include $(MAKEDIR)/test.make

.PHONY : clean 
clean: hedc_clean cleansymbols 
	@@$(RM) *.o

##################################################
# hedc benchmark
##################################################

HEDC_JAVA_FILES =\
	DateFormatter.java\
	Messages.java\
	RandomDate.java\
	FormFiller.java\
	MetaSearchResult.java\
	Task.java\
	MetaSearch.java\
	PooledExecutorWithInvalidate.java\
	TaskFactory.java\
	MetaSearchImpl.java\
	Rag.java\
	Tester.java\
	MetaSearchRequest.java\
	SohoSynoptic.java\
	ethz/util/EtcUtil.java\
	ethz/util/BufferedCopy.java\
	ethz/util/Generator.java\
	ethz/util/SystemProperties.java\
	ethz/util/ObservableProperties.java\
	ethz/util/BaseProperties.java\
	ethz/util/Messages.java\
	ethz/util/PropertyMonitoring.java\
	regexp/Dot.java\
	regexp/Literal.java\
	regexp/Regexp.java\
	regexp/Result.java\
	regexp/State.java\
	regexp/Multi.java\
	regexp/RegexpCompiler.java\
	regexp/GroupReference.java\
	regexp/ContextMatch.java\
	regexp/MalformedRegexpException.java\
	regexp/NoSuchMatchException.java\
	regexp/CharClass.java\
	regexp/Group.java\
	regexp/Alternatives.java\
	java/util/BasicMapEntry.java\
	java/util/HashMap.java\
	java/util/Map.java\
	java/util/AbstractSequentialList.java\
	java/util/LinkedList.java\
	java/util/AbstractSet.java\
	java/util/Set.java\
	java/util/TreeMap.java\
	java/util/SortedMap.java\
	java/util/AbstractMap.java\
	java/util/Iterator.java\
	java/util/List.java\
	java/util/AbstractList.java\
	java/util/AbstractCollection.java\
	java/util/Collection.java\
	java/util/SubList.java\
	java/util/Comparator.java\
	java/util/ListIterator.java\
	EDU/oswego/cs/dl/util/concurrent/LinkedQueue.java\
	EDU/oswego/cs/dl/util/concurrent/LinkedNode.java\
	EDU/oswego/cs/dl/util/concurrent/Channel.java\
	EDU/oswego/cs/dl/util/concurrent/SynchronousChannel.java\
	EDU/oswego/cs/dl/util/concurrent/Puttable.java\
	EDU/oswego/cs/dl/util/concurrent/Takable.java\
	EDU/oswego/cs/dl/util/concurrent/Semaphore.java\
	EDU/oswego/cs/dl/util/concurrent/WaiterPreferenceSemaphore.java\
	EDU/oswego/cs/dl/util/concurrent/BoundedChannel.java\
	EDU/oswego/cs/dl/util/concurrent/Sync.java

HEDC_CLASS_FILES =\
	DateFormatter.class\
	ethz/util/EtcUtil.class\
	ethz/util/BufferedCopy.class\
	ethz/util/Generator.class\
	ethz/util/SystemProperties.class\
	ethz/util/ObservableProperties.class\
	ethz/util/BaseProperties.class\
	ethz/util/Messages.class\
	ethz/util/PropertyMonitoring.class\
	Messages.class\
	RandomDate.class\
	FormFiller.class\
	MetaSearchResult.class\
	Task.class\
	MetaSearch.class\
	PooledExecutorWithInvalidate.class\
	TaskFactory.class\
	MetaSearchImpl.class\
	Rag.class\
	Tester.class\
	MetaSearchRequest.class\
	SohoSynoptic.class\
	EDU/oswego/cs/dl/util/concurrent/LinkedQueue.class\
	EDU/oswego/cs/dl/util/concurrent/LinkedNode.class\
	EDU/oswego/cs/dl/util/concurrent/Channel.class\
	EDU/oswego/cs/dl/util/concurrent/SynchronousChannel.class\
	EDU/oswego/cs/dl/util/concurrent/Puttable.class\
	EDU/oswego/cs/dl/util/concurrent/Takable.class\
	EDU/oswego/cs/dl/util/concurrent/Semaphore.class\
	EDU/oswego/cs/dl/util/concurrent/WaiterPreferenceSemaphore.class\
	EDU/oswego/cs/dl/util/concurrent/BoundedChannel.class\
	EDU/oswego/cs/dl/util/concurrent/Sync.class\
	regexp/Dot.class\
	regexp/Literal.class\
	regexp/Regexp.class\
	regexp/Result.class\
	regexp/State.class\
	regexp/StringState.class\
	regexp/CharArrayState.class\
	regexp/Multi.class\
	regexp/RegexpCompiler.class\
	regexp/SuccessRegexp.class\
	regexp/GroupReference.class\
	regexp/CompilerState.class\
	regexp/ContextMatch.class\
	regexp/MalformedRegexpException.class\
	regexp/NoSuchMatchException.class\
	regexp/CharClass.class\
	regexp/Group.class\
	regexp/Alternatives.class\
	java/util/BasicMapEntry.class\
	java/util/HashMap.class\
	java/util/Map.class\
	java/util/AbstractSequentialList.class\
	java/util/LinkedList.class\
	java/util/AbstractSet.class\
	java/util/Set.class\
	java/util/TreeMap.class\
	java/util/SortedMap.class\
	java/util/AbstractMap.class\
	java/util/Iterator.class\
	java/util/List.class\
	java/util/AbstractList.class\
	java/util/AbstractCollection.class\
	java/util/Collection.class\
	java/util/SubList.class\
	java/util/Comparator.class\
	java/util/ListIterator.class

HEDC_INNER_CLASS_FILES =\
	FormFiller$(DOLLAR_SUBST)Filter.class\
	MetaSearchResult$(DOLLAR_SUBST)MetaSearchResultIterator.class\
	PooledExecutorWithInvalidate$(DOLLAR_SUBST)BlockedExecutionHandler.class\
	PooledExecutorWithInvalidate$(DOLLAR_SUBST)DiscardWhenBlocked.class\
	PooledExecutorWithInvalidate$(DOLLAR_SUBST)RunWhenBlocked.class\
	PooledExecutorWithInvalidate$(DOLLAR_SUBST)WaitWhenBlocked.class\
	PooledExecutorWithInvalidate$(DOLLAR_SUBST)Worker.class\
	Rag$(DOLLAR_SUBST)RagIterator.class\
	SohoSynoptic$(DOLLAR_SUBST)SohoIterator.class\
	java/util/Map$(DOLLAR_SUBST)Entry.class\
	java/util/HashMap$(DOLLAR_SUBST)Entry.class\
	java/util/HashMap$(DOLLAR_SUBST)1.class\
	java/util/HashMap$(DOLLAR_SUBST)2.class\
	java/util/HashMap$(DOLLAR_SUBST)3.class\
	java/util/HashMap$(DOLLAR_SUBST)HashIterator.class\
	java/util/LinkedList$(DOLLAR_SUBST)Entry.class\
	java/util/LinkedList$(DOLLAR_SUBST)LinkedListItr.class\
	java/util/TreeMap$(DOLLAR_SUBST)Node.class\
	java/util/TreeMap$(DOLLAR_SUBST)1.class\
	java/util/TreeMap$(DOLLAR_SUBST)2.class\
	java/util/TreeMap$(DOLLAR_SUBST)3.class\
	java/util/TreeMap$(DOLLAR_SUBST)VerifyResult.class\
	java/util/TreeMap$(DOLLAR_SUBST)TreeIterator.class\
	java/util/TreeMap$(DOLLAR_SUBST)SubMap.class\
	java/util/TreeMap$(DOLLAR_SUBST)4.class\
	java/util/AbstractMap$(DOLLAR_SUBST)1.class\
	java/util/AbstractMap$(DOLLAR_SUBST)2.class\
	java/util/AbstractMap$(DOLLAR_SUBST)3.class\
	java/util/AbstractMap$(DOLLAR_SUBST)4.class\
	java/util/SubList$(DOLLAR_SUBST)1.class\
	java/util/AbstractList$(DOLLAR_SUBST)AbstractListItr.class

#	FormFiller$(DOLLAR_SUBST)1.class\  # only created, when compiling with JDK1.3, not with JDK1.2.2

HEDC_EXE = Tester
HEDC_MAINCLASS = Tester

.PHONY : hedc
hedc: hedc_erco hedc_rd hedc_rdstats hedc_gcj

.PHONY : hedc_classes
hedc_classes: $(HEDC_JAVA_FILES:.java=.class)

.PHONY : hedc_erco
hedc_erco: ASM_FILES = $(HEDC_CLASS_FILES:.class=.s) $(subst $(DOLLAR_SUBST),\$$, $(HEDC_INNER_CLASS_FILES:.class=.s))
hedc_erco: TARGET = $(HEDC_EXE)$(ERCO_TAG)
hedc_erco: LDFLAGS = --main=$(HEDC_MAINCLASS) -o $(HEDC_EXE)$(ERCO_TAG)
hedc_erco: $(HEDC_JAVA_FILES:.java=.class) $(HEDC_CLASS_FILES:.class=.s) $(HEDC_INNER_CLASS_FILES:.class=.si) $(HEDC_EXE)$(ERCO_TAG)

.PHONY : hedc_rd
hedc_rd: ASM_FILES = $(HEDC_CLASS_FILES:.class=.rd.s) $(subst $(DOLLAR_SUBST),$$\, $(HEDC_INNER_CLASS_FILES:.class=.s))
hedc_rd: TARGET = $(HEDC_EXE)$(ERCO_RD_TAG)
hedc_rd: LDFLAGS = --main=$(HEDC_MAINCLASS) -o $(HEDC_EXE)$(ERCO_RD_TAG)
hedc_rd: $(HEDC_JAVA_FILES:.java=.class) $(HEDC_CLASS_FILES:.class=.rd.s) $(HEDC_INNER_CLASS_FILES:.class=.rd.si) $(HEDC_EXE)$(ERCO_RD_TAG)

.PHONY : hedc_rdstats
hedc_rdstats: ASM_FILES = $(HEDC_CLASS_FILES:.class=.rdstats.s) $(subst $(DOLLAR_SUBST),$$\, $(HEDC_INNER_CLASS_FILES:.class=.s))
hedc_rdstats: TARGET = $(HEDC_EXE)$(ERCO_RDSTATS_TAG)
hedc_rdstats: LDFLAGS = --main=$(HEDC_MAINCLASS) -o $(HEDC_EXE)$(ERCO_RDSTATS_TAG)
hedc_rdstats: $(HEDC_JAVA_FILES:.java=.class) $(HEDC_CLASS_FILES:.class=.rdstats.s) $(HEDC_INNER_CLASS_FILES:.class=.rdstats.si) $(HEDC_EXE)$(ERCO_RDSTATS_TAG)

.PHONY : hedc_gcj
hedc_gcj: CLASS_FILES = $(HEDC_CLASS_FILES) $(subst $(DOLLAR_SUBST),$$\, $(HEDC_INNER_CLASS_FILES))
hedc_gcj: LDFLAGS = --main=$(HEDC_MAINCLASS) -o $(HEDC_EXE)$(GCJ_TAG)
hedc_gcj: TARGET = $(HEDC_EXE)$(GCJ_TAG)
hedc_gcj: $(HEDC_CLASS_FILES) $(subst $(DOLLAR_SUBST),$$, $(HEDC_INNER_CLASS_FILES)) $(HEDC_EXE)$(GCJ_TAG)

.PHONY : hedc_test
hedc_test: TEST_EXE = ./Main
hedc_test: TEST_DIR = ./COM/hedc
hedc_test: TEST_ARGS =
hedc_test: test

.PHONY : hedc_ctstats
hedc_ctstats: ASM_FILES = $(HEDC_CLASS_FILES:.class=.rd.s) $(subst $(DOLLAR_SUBST),$$\, $(HEDC_INNER_CLASS_FILES:.class=.s))
hedc_ctstats: TARGET = $(HEDC_EXE)$(ERCO_RD_TAG)
hedc_ctstats: ctstats

.PHONY : hedc_javap
hedc_javap: $(HEDC_CLASS_FILES:.class=.jp)

.PHONY : hedc_clean
hedc_clean:
	@@$(RM) $(HEDC_CLASS_FILES:.class=.s) ;\
	$(RM) $(HEDC_CLASS_FILES:.class=.out) ;\
	$(RM) $(HEDC_CLASS_FILES:.class=.rd.s) ;\
	$(RM) $(HEDC_CLASS_FILES:.class=.rd.out) ;\
	$(RM) $(HEDC_CLASS_FILES:.class=.rdstats.s) ;\
	$(RM) $(HEDC_CLASS_FILES:.class=.rdstats.out) ;\
	$(RM) $(HEDC_CLASS_FILES:.class=.jp);\
	$(RM) $(HEDC_CLASS_FILES:.class=.sym);\
	$(RM) $(subst $(DOLLAR_SUBST),\$$, $(HEDC_INNER_CLASS_FILES:.class=.s)) ;\
	$(RM) $(subst $(DOLLAR_SUBST),\$$, $(HEDC_INNER_CLASS_FILES:.class=.si)) ;\
	$(RM) $(subst $(DOLLAR_SUBST),\$$, $(HEDC_INNER_CLASS_FILES:.class=.out)) ;\
	$(RM) $(subst $(DOLLAR_SUBST),\$$, $(HEDC_INNER_CLASS_FILES:.class=.rd.s)) ;\
	$(RM) $(subst $(DOLLAR_SUBST),\$$, $(HEDC_INNER_CLASS_FILES:.class=.rd.si)) ;\
	$(RM) $(subst $(DOLLAR_SUBST),\$$, $(HEDC_INNER_CLASS_FILES:.class=.rd.out)) ;\
	$(RM) $(subst $(DOLLAR_SUBST),\$$, $(HEDC_INNER_CLASS_FILES:.class=.rdstats.s)) ;\
	$(RM) $(subst $(DOLLAR_SUBST),\$$, $(HEDC_INNER_CLASS_FILES:.class=.rdstats.si)) ;\
	$(RM) $(subst $(DOLLAR_SUBST),\$$, $(HEDC_INNER_CLASS_FILES:.class=.rdstats.out)) ;\
	$(RM) $(subst $(DOLLAR_SUBST),\$$, $(HEDC_INNER_CLASS_FILES:.class=.jp)) ;\
	$(RM) $(subst $(DOLLAR_SUBST),\$$, $(HEDC_INNER_CLASS_FILES:.class=.sym)) ;\
	$(RM) \
		$(HEDC_EXE).*.out\
		$(HEDC_EXE)$(ERCO_RDSTATS_TAG)\
		$(HEDC_EXE)$(ERCO_RD_TAG)\
		$(HEDC_EXE)$(ERCO_TAG)\
		$(HEDC_EXE)$(GCJ_TAG)

