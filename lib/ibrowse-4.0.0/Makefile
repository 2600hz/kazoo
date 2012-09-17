ROOT = ../..
REBAR = $(ROOT)/bin/rebar

IBROWSE_VSN = $(shell sed -n 's/.*{vsn,.*"\(.*\)"}.*/\1/p' src/ibrowse.app.src)

all: app

app: compile

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

install: all
	mkdir -p $(DESTDIR)/lib/ibrowse-$(IBROWSE_VSN)/
	cp -r ebin $(DESTDIR)/lib/ibrowse-$(IBROWSE_VSN)/

test: all
	$(REBAR) eunit
	(cd test; make) 
	erl -noshell -pa ebin -pa test -s ibrowse -s ibrowse_test unit_tests \
	-s ibrowse_test verify_chunked_streaming \
	-s ibrowse_test test_chunked_streaming_once \
	-s erlang halt

