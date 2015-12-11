ROOT = ../..
PROJECT = whistle_couch

EBINS = $(shell find $(ROOT)/core/whistle_number_manager-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/deps/couchbeam-* -maxdepth 2 -name ebin -print)

SOURCES = src/wh_gen_changes.erl src/*.erl

all: compile

-include $(ROOT)/make/kz.mk
