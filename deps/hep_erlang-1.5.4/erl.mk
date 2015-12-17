APP = $(patsubst src/%.app.src,%,$(wildcard src/*.app.src))

### DEBUG-APP -- Load compiled code into a new REPL

.PHONY: debug-app
debug-app: ERLCFLAGS += +debug_info +export_all
debug-app: app
	erl -pz ebin/ $(patsubst %,-pz %,$(wildcard deps/*/ebin/)) -eval 'c:l($(APP)).'

### DEPS -- Fetches & compiles deps recursively then moves every dep to deps/

.PHONY: deps
deps: $(patsubst dep_%,deps/%/,$(filter dep_%,$(.VARIABLES)))
	$(if $(wildcard deps/*/deps/), \
	    mv -v deps/*/deps/* deps/ 2>/dev/null ; rmdir deps/*/deps/)

deps/%/:
	@bash -c "( mkdir -p deps && \
	    curl -fsSLo $@.zip '$(word 1,$(dep_$*))/archive/$(word 2,$(dep_$*)).zip' && \
	    unzip -q $@.zip -d deps && mv $@-* $@ && \
	    echo 'curl -fsSLo $@ $(word 1,$(dep_$*))/archive/$(word 2,$(dep_$*)).zip' \
	) || ( git clone --no-checkout -- $(word 1,$(dep_$*)) $@ && \
	    cd $@ && git checkout --quiet $(word 2,$(dep_$*)) && cd - )"
	@bash -c "if [[ -f $@/Makefile ]]; \
	then echo '$(MAKE) -C $@' ; \
	           $(MAKE) -C $@  ; \
	else echo 'cd $@ && rebar get-deps compile && cd -' ; \
	           cd $@ && rebar get-deps compile && cd -  ; fi"

### APP -- Compiles src/ into ebin/

.PHONY: app
app: ebin/$(APP).app \
     $(foreach ext, erl xrl yrl S core, \
         $(patsubst src/%.$(ext), ebin/%.beam, $(wildcard src/*.$(ext)))) \
     $(patsubst templates/%.dtl,  ebin/%_dtl.beam,$(wildcard templates/*.dtl))

ebin/%.app: src/%.app.src               | ebin/
	@erl -noshell \
	     -eval 'case file:consult("$<") of {ok,_} -> ok ; {error,{_,_,M}} -> io:format("$<: ~s~s\n",M), halt(1) end.' \
	     -s init stop
	cp $< $@

ebin/%.beam: pz_deps = $(addprefix -pz , $(shell echo deps/*/ebin/))
ebin/%.beam: include_files = $(wildcard include/*.hrl)
ebin/%.beam: include_dirs = -I include/ -I deps/

ebin/%.beam: $(include_files) src/%.erl | ebin/
	erlc -v -o ebin/ $(pz_deps) $(ERLCFLAGS) $(include_dirs) $<

ebin/%.beam: $(include_files) src/%.xrl | ebin/
	erlc    -o ebin/ $(pz_deps) $(ERLCFLAGS) $<
	erlc    -o ebin/ $(pz_deps) $(ERLCFLAGS) $(include_dirs) ebin/$*.erl

ebin/%.beam: $(include_files) src/%.yrl | ebin/
	erlc    -o ebin/ $(pz_deps) $(ERLCFLAGS) $<
	erlc    -o ebin/ $(pz_deps) $(ERLCFLAGS) $(include_dirs) ebin/$*.erl

ebin/%.beam: $(include_files) src/%.S   | ebin/
	erlc -v -o ebin/ $(pz_deps) $(ERLCFLAGS) $(include_dirs) +from_asm $<

ebin/%.beam: $(include_files) src/%.core| ebin/
	erlc -v -o ebin/ $(pz_deps) $(ERLCFLAGS) $(include_dirs) +from_core $<

ebin/%_dtl.beam: templates/%.dtl        | ebin/
	$(if $(wildcard deps/erlydtl/),, \
	    $(error Error compiling $<: deps/erlydtl/ not found))
	@erl -noshell -pz ebin/ $(pz_deps) \
	     -eval 'io:format("Compiling ErlyDTL template: $< -> $@\n").' \
	     -eval 'erlydtl:compile("$<", $*_dtl, [{out_dir,"ebin/"},{auto_escape,false}]).' \
	     -s init stop

ebin/: deps
	$(if $(wildcard $@),,mkdir $@)

### EUNIT -- Compiles (into ebin/) & run EUnit tests (test/*_test.erl files)

.PHONY: eunit
eunit: $(patsubst test/%_tests.erl, eunit.%, $(wildcard test/*_tests.erl))

eunit.%: first_flags = -o ebin/ $(patsubst %,-pz %,$(wildcard deps/*/ebin/))
eunit.%: include_files = $(wildcard include/*.hrl)
eunit.%: include_dirs = -I include/ -I deps/

#eunit.%: ebin/%_tests.beam #FIXME
eunit.%: $(include_files)               | app
	erlc -v $(first_flags) -DTEST=1 -DEUNIT $(ERLCFLAGS) $(include_dirs) test/$*_tests.erl
	@erl -noshell -pz ebin/ $(patsubst %,-pz %,$(wildcard deps/*/ebin/)) \
	     -eval 'io:format("Module $*_tests:\n"), eunit:test($*_tests).' \
	     -s init stop
.PHONY: eunit.%

#ebin/%_tests.beam: test/%_tests.erl     | app #FIXME so that this can be used instead of the erlc line above
#	erlc -v $(first_flags) -DTEST=1 -DEUNIT $(ERLCFLAGS) $(include_dirs) $<
#.PRECIOUS: ebin/%_tests.beam

### CT -- Compiles (into ebin/) & run Common Test tests (test/*_SUITE.erl)

.PHONY: ct
ct: $(patsubst test/%_SUITE.erl, ct.%, $(wildcard test/*_SUITE.erl))

ct.%: first_flags = -o ebin/ $(patsubst %,-pz %,$(wildcard deps/*/ebin/))
ct.%: include_files = $(wildcard include/*.hrl)
ct.%: include_dirs = -I include/ -I deps/

ct.%: ebin/%_SUITE.beam                 | logs/
#	FIXME use -no_auto_compile and ebin/%_SUITE.beam properly.
	@ct_run -noshell -dir test/ -logdir logs/ \
	        -pz ebin/ $(patsubst %,-pz %,$(realpath $(wildcard deps/*/ebin/))) \
	        -suite $*_SUITE || true
.PHONY: ct.%

#FIXME make ct depend on target app, and in a parallel-safe way.
#ebin/%_SUITE.beam: test/%_SUITE.erl     | ebin/ app <-- this blocks if no ebin/ (try swapping them?)
ebin/%_SUITE.beam: $(include_files) test/%_SUITE.erl    | ebin/
	erlc -v $(first_flags) $(ERLCFLAGS) $(include_dirs) $<
.PRECIOUS: ebin/%_SUITE.beam

logs/:
	mkdir $@

### ESCRIPT -- Create a stand-alone EScript executable

escript: app clean-escript #FIXME: PHONYness & when in deps/
	@bash -c 'find deps/ -name "*.app.src" -exec basename {} \; 2>/dev/null | cut -d. -f1 | while read dep; do e=deps/$$dep/$$dep; [[ -f $$e && -x $$e ]] && rm -v $$e || true; done'
	$(info Creating escript: ./$(APP))
	@erl -noshell \
	     -eval 'AccF = fun (F, Acc) -> case re:run(F, "(/\\..+|^\\./(deps/[^/]+/)?(test|doc)/)", [{capture,none}]) of match -> Acc; nomatch -> [F|Acc] end end, escript:create("$(APP)", [ {shebang,default}, {comment,""}, {emu_args,"-escript main $(APP)"}, {archive, [{case File of "./deps/"++File1 -> File1; _ -> "$(APP)/" ++ File -- "./" end,element(2,file:read_file(File))} || File <- filelib:fold_files(".", ".+", true, AccF, []) ], []} ]).' \
	     -s init stop
	chmod u+x ./$(APP)

### DOCS -- Compiles the app's documentation into doc/

.PHONY: docs #FIXME depend on target app
docs: $(foreach ext,app.src erl xrl yrl S core, $(wildcard src/*.$(ext))) \
                                                $(wildcard doc/overview.edoc)
	@erl -noshell \
	     -eval 'io:format("Compiling documentation for $(APP).\n").' \
	     -eval 'edoc:application($(APP), ".", [$(EDOC_OPTS)]).' \
	     -s init stop

### CLEAN-DOCS -- Removes generated stuff from doc/

.PHONY: clean-docs
clean-docs:
	$(if $(wildcard doc/*.css),     rm doc/*.css)
	$(if $(wildcard doc/*.html),    rm doc/*.html)
	$(if $(wildcard doc/*.png),     rm doc/*.png)
	$(if $(wildcard doc/edoc-info), rm doc/edoc-info)
	@bash -c '[[ -d doc/ ]] && [[ ''doc/*'' = "`echo doc/*`" ]] && rmdir doc/ || true' #FIXME use wildcard, notdir, … instead of shell

### CLEAN-ESCRIPT -- Removes ./$(APP) if it exists

.PHONY: clean-escript
clean-escript:
	$(if $(wildcard $(APP)), rm ./$(APP))

### CLEAN-EBIN -- Removes ebin/ if it exists

.PHONY: clean-ebin
clean-ebin:
	$(if $(wildcard ebin/),rm -r ebin/)

### CLEAN-DEPS -- Removes deps/ if it exists

.PHONY: clean-deps
clean-deps:
	$(if $(wildcard deps/),rm -rf deps/)
