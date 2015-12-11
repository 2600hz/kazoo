ROOT = ../..
PROJECT = erlydtl

EBINS = $(shell find $(ROOT)/deps/merl-* -maxdepth 2 -name ebin -print)

ERLC_OPTS += -Iinclude

CLEAN_MOAR = clean-parser

all: compile

-include $(ROOT)/make/kz.mk

clean-parser:
	$(if $(wildcard src/erlydtl_parser.erl), rm src/erlydtl_parser.erl)
