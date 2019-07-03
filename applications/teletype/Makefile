CWD = $(shell pwd -P)
ROOT = $(realpath $(CWD)/../..)
PROJECT = teletype

all: compile

compile: default_modules

default_modules: src/teletype_default_modules.hrl

src/teletype_default_modules.hrl:
	@./priv/templates_to_header.bash

clean: clean-default_modules

clean-default_modules:
	rm -f src/teletype_default_modules.hrl

compile-test-direct: default_modules

include $(ROOT)/make/kz.mk
