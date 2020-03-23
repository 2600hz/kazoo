ROOT = ../..
PROJECT = kazoo_amqp

ERLC_OPTS += +'{lager_extra_sinks, [amqp]}'

SOURCES = src/gen_listener.erl $(wildcard src/*.erl) $(wildcard src/*/*.erl)
COMPILE_MOAR = src/gen_listener.erl

all: compile

include $(ROOT)/make/kz.mk
