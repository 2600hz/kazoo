ROOT = ../..
PROJECT = kazoo_documents

TEST_EBINS = $(ROOT)/applications/crossbar/ebin
SRC_FILES := $(sort $(wildcard src/*.erl.src))
ERL_FILES := $(SRC_FILES:.erl.src=.erl)

SOURCES = $(wildcard src/*.erl) $(ERL_FILES)
COMPILE_MOAR = $(ERL_FILES)

all: compile

compile: $(ERL_FILES)

clean: clean-autogen

clean-autogen:
	@$(foreach ERL,$(ERL_FILES),cmp --silent $(ERL) $(ERL:.erl=.erl.src) && rm $(ERL);) true

%.erl: %.erl.src
	@$(cp -n $< $@) true

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
