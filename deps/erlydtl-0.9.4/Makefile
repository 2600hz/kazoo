ROOT = ../..
PROJECT = erlydtl

EBINS = $(shell find $(ROOT)/deps/merl-* -maxdepth 2 -name ebin)

CLEAN_MOAR = clean-parser

all: compile

-include $(ROOT)/make/kz.mk

clean-parser:
	$(if $(wildcard src/erlydtl_parser.erl), rm src/erlydtl_parser.erl)
