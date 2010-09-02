include vsn.mk

all:
	(cd src ; make)

clean:
	(cd src ; make clean)

install: all
	mkdir -p $(DESTDIR)/lib/ibrowse-$(IBROWSE_VSN)/
	cp -r ebin $(DESTDIR)/lib/ibrowse-$(IBROWSE_VSN)/
