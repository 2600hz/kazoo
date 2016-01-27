ROOT = ../..
PROJECT = registrar

all: compile

# Target to be built by `compile` and `compile-test`
COMPILE_MOAR = priv/comp128.so
CLEAN_MOAR = clean-so

-include $(ROOT)/make/kz.mk


ERL_INCLUDES := $(wildcard $(shell dirname `which erl`)/../erts-*/include)
ifeq ($(ERL_INCLUDES),)
	ERL_INCLUDES = /usr/lib/erlang/usr/include
endif

priv/comp128.so: c_src/comp128.c
	gcc -I$(ERL_INCLUDES) -fpic -shared c_src/comp128.c -o priv/comp128.so

clean-so:
	$(if $(wildcard priv/comp128.so), rm priv/comp128.so)
