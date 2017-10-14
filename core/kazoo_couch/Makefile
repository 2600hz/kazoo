ROOT = ../..
PROJECT = kazoo_couch

all: compile

compile: compile-first

compile-test: compile-first-test

compile-first:
	$(MAKE) compile -C $(ROOT)/core/kazoo_data/

compile-first-test:
	$(MAKE) compile -C $(ROOT)/core/kazoo_data/

include $(ROOT)/make/kz.mk
