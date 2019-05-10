ROOT = ../..
PROJECT = kazoo_data

SOURCES = src/kz_data.erl $(wildcard src/*.erl)

ERLC_OPTS += +'{lager_extra_sinks, [data]}'

all: compile

compile-test: compile-also

compile-also:
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_fixturedb
	$(MAKE) compile-test -C $(ROOT)/core/kazoo
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_attachments
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_bindings
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_caches
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_config
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_stdlib


-include $(ROOT)/make/kz.mk
