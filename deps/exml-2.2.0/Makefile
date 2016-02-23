ROOT = ../..
REBAR = $(ROOT)/utils/rebar/rebar

INCLUDE_FILES=$(shell escript tools/get_included_files_h.erl)
CFLAGS_LINUX = -shared
CFLAGS_DARWIN = -undefined dynamic_lookup
CFLAGS_REST = -lexpat -fPIC
ifeq ($(shell uname), Darwin)
	CFLAGS = $(CFLAGS_DARWIN) $(INCLUDE_FILES) $(CFLAGS_REST)
else
	CFLAGS = $(CFLAGS_LINUX) $(INCLUDE_FILES) $(CFLAGS_REST)
endif
SO = priv/exml_event.so priv/exml_escape.so

all: compile

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

# test-deps: rebar
# 	./rebar -C rebar.test.config get-deps

# test-compile: rebar test-deps
# 	./rebar -C rebar.test.config compile

# .PHONY: test
# test: test-compile
# 	./rebar -C rebar.test.config skip_deps=true eunit


shared_libs: $(SO)

priv/%.so: c_src/%.c
	@mkdir -p priv
	cc -o $@ $^ $(CFLAGS)

shared_clean:
	-rm $(SO)
	-rm -r priv
