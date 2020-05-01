CWD = $(shell pwd -P)
ROOT = $(realpath $(CWD)/../..)
PROJECT = braintree

SOURCES = $(wildcard src/*.erl) $(wildcard src/*/*.erl)

ERLC_OPTS = -Iinclude \
	-pa $(ROOT)/applications/crossbar/ebin

ELIBS = $(ROOT)/applications/crossbar:$(ROOT)/core/

all: compile

include $(ROOT)/make/kz.mk
