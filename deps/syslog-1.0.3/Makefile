ROOT = ../..
PROJECT = syslog

COMPILE_MOAR = priv/syslog_drv.so

all: compile

-include $(ROOT)/make/kz.mk


USR_INCLUDE := $(wildcard /usr/lib/erlang/erts-*/include)
ifeq ($(USR_INCLUDE),)
	USR_INCLUDE = $(wildcard $(shell dirname `which erl`)/../erts-*/include)
endif
ifeq ($(USR_INCLUDE),)
	USR_INCLUDE = /usr/lib/erlang/usr/include
endif

ERL_INTERFACE := $(shell find $USR_INCLUDE/../../ -type d -name 'erl_interface-*' -print)
ifeq ($(ERL_INTERFACE),)
	ERL_INTERFACE := $(wildcard $(USR_INCLUDE)/../../lib/erl_interface-*)
endif

priv/syslog_drv.so: c_src/syslog_drv.c
	@mkdir -p priv/
	gcc -I$(USR_INCLUDE) -I$(ERL_INTERFACE)/include -L$(ERL_INTERFACE)/lib -fpic -shared c_src/syslog_drv.c -o priv/syslog_drv.so -lerl_interface -lei
