ROOT = ../..
PROJECT = kazoo_documents

TEST_EBINS = $(ROOT)/applications/crossbar/ebin
SRC_FILES := $(wildcard src/*.erl.src)
ERL_FILES := $(SRC_FILES:.erl.src=.erl)

COMPILE_MOAR = $(ERL_FILES)

all: compile

compile: $(ERL_FILES)

clean: clean-autogen

clean-autogen:
	@$(foreach ERL,$(ERL_FILES),cmp --silent $(ERL) $(ERL:.erl=.erl.src) && rm $(ERL);) true

%.erl: %.erl.src
	@cp -n $< $@

eunit: kz-test

test: kz-test

kz-test:
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_stdlib/
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_config/

include $(ROOT)/make/kz.mk
