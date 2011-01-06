PKGNAME=dynamic_compile
VERSION=0.1
LIBDIR=`erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
ERL_SOURCES := $(wildcard src/*.erl)
ERL_OBJECTS := $(ERL_SOURCES:%.erl=./%.beam)

all: app $(ERL_OBJECTS)

app:
	sh ebin/$(PKGNAME).app.in $(VERSION)
	
clean:
	rm -rf ebin/*.beam erl_crash.dump
	
install:
	mkdir -p $(prefix)/$(LIBDIR)/dynamic_compile-$(VERSION)/ebin
	for i in ebin/*; do install $$i $(prefix)/$(LIBDIR)/dynamic_compile-$(VERSION)/$$i ; done
	rm -rf ebin/*.beam erl_crash.dump ebin/*.app

package: clean
	@mkdir $(PKGNAME)-$(VERSION)/ && cp -rf ebin Makefile src $(PKGNAME)-$(VERSION)
	@COPYFILE_DISABLE=true tar zcf $(PKGNAME)-$(VERSION).tgz $(PKGNAME)-$(VERSION)
	@rm -rf $(PKGNAME)-$(VERSION)/
	
./%.beam: %.erl
	@mkdir -p ebin
	erlc +debug_info -I include -o ebin $<