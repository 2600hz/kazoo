ROOT = ../..
PROJECT = teletype

all: compile

compile: default_modules

default_modules: src/teletype_default_modules.hrl

src/teletype_default_modules.hrl:
	@./priv/templates_to_header.bash

clean: clean-default_modules

clean-default_modules:
	rm -f src/teletype_default_modules.hrl

compile-test: default_modules compile-extra

compile-extra:
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_data

include $(ROOT)/make/kz.mk
