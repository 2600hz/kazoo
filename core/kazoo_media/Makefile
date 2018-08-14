ROOT = ../..
PROJECT = kazoo_media

all: compile

include $(ROOT)/make/kz.mk

compile-test: compile-also

compile-also:
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_config/
