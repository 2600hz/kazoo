CWD = $(shell pwd -P)
ROOT = $(realpath $(CWD)/../..)
PROJECT = callflow

all: schemas compile

schemas:
	@bash priv/scripts/check-schemas.bash

include $(ROOT)/make/kz.mk
