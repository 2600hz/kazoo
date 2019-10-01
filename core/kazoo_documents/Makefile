ROOT = ../..
PROJECT = kazoo_documents

TEST_EBINS = $(ROOT)/applications/crossbar/ebin
SRC_FILES := $(sort $(wildcard src/*.erl.src))
ERL_FILES := $(SRC_FILES:.erl.src=.erl)

SOURCES := $(sort $(wildcard src/*.erl) $(ERL_FILES))
COMPILE_MOAR = $(ERL_FILES)

all: compile

compile: compile-autogen $(ERL_FILES)

compile-test: compile-autogen $(ERL_FILES)

compile-autogen: $(addsuffix -compile,$(SRC_FILES))

clean: clean-autogen

clean-autogen: $(addsuffix -clean,$(SRC_FILES))

%-clean: %
	$(eval ERL := $(<:.erl.src=.erl))
	@-cmp -s $(ERL) $<; [[ $$? -eq 0 ]] && rm $(ERL) || :

%-compile: %
	$(eval ERL := $(<:.erl.src=.erl))
	$(shell cp --no-clobber $< $(ERL))

diff:
	@$(foreach ERL,$(ERL_FILES),cmp --silent $(ERL) $(ERL:.erl=.erl.src) || diff -c $(ERL) $(ERL).src;) true

eunit: kz-test

test: kz-test

kz-test:
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_stdlib/
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_config/
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_fixturedb/
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_data/
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_schemas/

include $(ROOT)/make/kz.mk
