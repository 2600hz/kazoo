ROOT = .

MAKEDIRS = deps/Makefile \
	   core/Makefile \
	   applications/Makefile

.PHONY: $(MAKEDIRS)

all : compile

compile: ACTION = all
compile: $(MAKEDIRS)

$(MAKEDIRS):
	$(MAKE) -C $(@D) $(ACTION)

clean : ACTION = clean
clean : $(MAKEDIRS)
	rm -f test/*.beam
	rm -f *crash.dump

test : clean app eunit

eunit :
	@$(REBAR) eunit skip_deps=true

build-plt :
	@$(DIALYZER) --build_plt --output_plt $(ROOT)/.platform_dialyzer.plt \
		--apps erts kernel stdlib crypto public_key ssl
