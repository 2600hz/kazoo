ROOT = ../..
PROJECT = zucchini

COMPILE_MOAR = src/zucchini_lexer.erl src/zucchini_parser.erl
CLEAN_MOAR = clean-lexer-parser

all: compile

-include $(ROOT)/make/kz.mk

src/zucchini_lexer.erl: src/zucchini_lexer.xrl
	erlc -o src/ $^

src/zucchini_parser.erl: src/zucchini_parser.yrl
	erlc -o src/ $^

clean-lexer-parser:
	$(if $(wildcard src/zucchini_lexer.erl), rm src/zucchini_lexer.erl)
	$(if $(wildcard src/zucchini_parser.erl), rm src/zucchini_parser.erl)
