ROOT = ../..
PROJECT = gen_smtp

COMPILE_MOAR = src/smtp_rfc822_parse.erl
CLEAN_MOAR = clean-lexer-parser

all: compile

-include $(ROOT)/make/kz.mk

src/smtp_rfc822_parse.erl: src/smtp_rfc822_parse.yrl
	erlc -o src/ $^

clean-lexer-parser:
	$(if $(wildcard src/smtp_rfc822_parse.erl), rm src/smtp_rfc822_parse.erl)
