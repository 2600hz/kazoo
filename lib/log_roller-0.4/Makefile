VERSION=0.4
PKGNAME=log_roller

all: rel

emake: app
	erl -make
	
rel: templates release/$(PKGNAME).tar.gz
	
app:
	sh ebin/log_roller.app.in $(VERSION)
	sh ebin/log_roller_server.app.in $(VERSION)
	
templates: emake
	erl -pa ebin -eval 'log_roller_server:compile_templates()' -s init stop -noshell
	
release/$(PKGNAME).rel release/$(PKGNAME).script release/$(PKGNAME).tar.gz:
	mkdir -p release
	escript build_rel.escript $(PKGNAME)

test: compile
	prove t/*.t

clean:
	rm -rf $(wildcard ebin/*.beam) erl_crash.dump *.boot *.rel *.script ebin/*.app release