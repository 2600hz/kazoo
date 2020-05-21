ROOT = ../..
PROJECT = acdc

all: compile

include $(ROOT)/make/kz.mk
TMPVAR := $(ERLC_OPTS)
ERLC_OPTS = $(filter-out -Werror, $(TMPVAR))
