include vsn.mk

all:
	(cd src ; make)

clean:
	(cd src ; make clean)

install: all
	mkdir -p $(DESTDIR)/lib/ibrowse-$(IBROWSE_VSN)/
	cp -r ebin $(DESTDIR)/lib/ibrowse-$(IBROWSE_VSN)/

test: all
	erl -noshell -pa ebin -s ibrowse -s ibrowse_test unit_tests \
	-s ibrowse_test verify_chunked_streaming \
	-s ibrowse_test test_chunked_streaming_once \
	-s erlang halt

