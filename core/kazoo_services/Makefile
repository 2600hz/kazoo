CWD = $(shell pwd -P)
ROOT = $(realpath $(CWD)/../..)
PROJECT = kazoo_services

ERLC_OPTS += +'{lager_extra_sinks, [dev]}'

all: compile

include $(ROOT)/make/kz.mk
