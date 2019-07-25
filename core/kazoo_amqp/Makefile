CWD = $(shell pwd -P)
ROOT = $(realpath $(CWD)/../..)
PROJECT = kazoo_amqp

SOURCES = src/gen_listener.erl $(wildcard src/*.erl) $(wildcard src/*/*.erl)
COMPILE_MOAR = src/gen_listener.erl

all: compile

include $(ROOT)/make/kz.mk
