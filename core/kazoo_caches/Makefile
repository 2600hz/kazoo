ROOT = ../..
PROJECT = kazoo_caches

all: compile

compile-test: compile-others

compile-others:
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_amqp

include $(ROOT)/make/kz.mk
