CWD = $(shell pwd -P)
ROOT = $(realpath $(CWD)/../..)
PROJECT = kazoo_documents

TEST_EBINS = $(ROOT)/applications/crossbar/ebin
SRC_FILES := $(sort $(wildcard src/*.erl.src))
ERL_FILES := $(SRC_FILES:.erl.src=.erl)

SOURCES := $(sort $(wildcard src/*.erl) $(ERL_FILES))
COMPILE_MOAR = $(ERL_FILES)

all: compile

compile: compile-autogen $(ERL_FILES)

compile-test-direct: compile-autogen $(ERL_FILES)

compile-autogen: $(addsuffix -compile,$(SRC_FILES))

clean: clean-autogen clean-patch

clean-autogen: $(addsuffix -clean,$(SRC_FILES))

%-clean: %
	$(eval ERL := $(<:.erl.src=.erl))
	@-cmp -s $(ERL) $<; [[ $$? -eq 0 ]] && rm $(ERL) || :

%-compile: %
	$(eval ERL := $(<:.erl.src=.erl))
	$(shell cp -n $< $(ERL))

diff:
	@$(foreach ERL,$(ERL_FILES),cmp --silent $(ERL) $(ERL:.erl=.erl.src) || diff -c $(ERL) $(ERL).src;) true

include $(ROOT)/make/kz.mk

DIFF_FILES := $(shell git diff --name-only $(BASE_BRANCH) $(SRC_FILES))
APPLY_FILES := $(strip $(subst kzd,patch-kzd,$(DIFF_FILES)))

.PHONY=apply-diff
apply-diff: $(APPLY_FILES)
	$(MAKE) clean-patch

.PHONY=clean-patch
clean-patch:
	$(if $(wildcard src/*.patch), rm src/*.patch)
	$(if $(wildcard src/*.orig), rm src/*.orig)

patch-%:
	@echo "patching $(basename $*)"
	@git diff $(BASE_BRANCH) -- $(ROOT)/$* > $(ROOT)/$*.patch
	@patch -p1 $(ROOT)/$(basename $*) $(ROOT)/$*.patch
