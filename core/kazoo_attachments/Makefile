CWD = $(shell pwd -P)
ROOT = $(realpath $(CWD)/../..)
PROJECT = kazoo_attachments

SOURCES = src/gen_attachment.erl $(wildcard src/*.erl)

all: compile

include $(ROOT)/make/kz.mk
