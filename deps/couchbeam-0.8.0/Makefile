ROOT = ../..
PROJECT = couchbeam

EBINS = $(shell find $(ROOT)/deps/couchbeam-* -maxdepth 2 -name ebin) \
	$(shell find $(ROOT)/deps/ibrowse-* -maxdepth 2 -name ebin)

all: compile

-include $(ROOT)/make/kz.mk
