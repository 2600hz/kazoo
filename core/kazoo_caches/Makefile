CWD = $(shell pwd -P)
ROOT = $(realpath $(CWD)/../..)
PROJECT = kazoo_caches

all: compile

compile-test: compile-others

compile-others:
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_amqp
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_bindings

include $(ROOT)/make/kz.mk
