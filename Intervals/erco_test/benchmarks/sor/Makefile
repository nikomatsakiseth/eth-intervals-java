#
# Copyright (C) 2000 by ETHZ/INF/CS
# All rights reserved
#
# 01/06/00  cvp
#

.PHONY: clean

help:
	@echo "Targets are 'clean'

clean:
	@echo "Removing assembly, object, dot and ps files ..."
	@find . -name '*.s'   | xargs -i rm -rf {}
	@find . -name '*.o'   | xargs -i rm -rf {}
	@find . -name '*.dot' | xargs -i rm -rf {}
	@find . -name '*.ps'  | xargs -i rm -rf {}
	@find . -name '*.out' | xargs -i rm -rf {}
	@find . -name '*.si'  | xargs -i rm -rf {}
	@find . -name '*.exe' | xargs -i rm -rf {}
	@find . -name '*~'    | xargs -i rm -rf {}
	@rm -rf a.out core __mainEntry.c

clean_class:
	@find . -name '*.class'  | xargs -i rm -rf {}

distclean: clean
