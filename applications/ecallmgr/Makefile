CWD = $(shell pwd -P)
ROOT = $(realpath $(CWD)/../..)
PROJECT = ecallmgr

all: compile

compile: ebin/fs_dist.beam

SOURCES = $(filter-out src/fs_dist.erl, $(wildcard src/*.erl) $(wildcard src/*/*.erl))

include $(ROOT)/make/kz.mk

ebin/fs_dist.beam: src/fs_dist.erl
	erlc -v -o ebin/ src/fs_dist.erl
