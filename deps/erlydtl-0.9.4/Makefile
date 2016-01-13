ROOT = ../..
PROJECT = erlydtl

EBINS = $(shell find $(ROOT)/deps/merl-* -maxdepth 2 -name ebin)

COMPILE_MOAR = src/erlydtl_parser.erl
CLEAN_MOAR = clean-parser

all: compile

-include $(ROOT)/make/kz.mk

src/erlydtl_parser.erl: src/erlydtl_parser.yrl
	erlc -o src/ $^

clean-parser:
	$(if $(wildcard src/erlydtl_parser.erl), rm src/erlydtl_parser.erl)
