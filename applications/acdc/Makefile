ROOT = ../..
PROJECT = acdc

all: compile

TEST_OPTS = -config test/test.config

# Test suite depends on .app
test: ebin/$(PROJECT).app compile-test
eunit: ebin/$(PROJECT).app compile-test

include $(ROOT)/make/kz.mk
