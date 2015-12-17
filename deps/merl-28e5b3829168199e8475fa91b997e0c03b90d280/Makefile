# simple Makefile
VSN=0.9
ERLC_FLAGS=
SOURCES=$(wildcard src/*.erl)
HEADERS=$(wildcard include/*.hrl)
OBJECTS=$(SOURCES:src/%.erl=ebin/%.beam)
DOC_OPTS={def,{version,\"$(VSN)\"}}

all: $(OBJECTS) #test
ebin/%.beam: src/%.erl $(HEADERS) Makefile
	erlc -pz ./priv -pa ./ebin $(ERLC_FLAGS) -o ebin/ $<

# additional dependencies due to the parse transform
ebin/merl_tests.beam ebin/merl_build.beam: \
	ebin/merl_transform.beam ebin/merl.beam

# special rules and dependencies to apply the transform to itself
ebin/merl_transform.beam: ebin/merl.beam priv/merl_transform.beam
priv/merl_transform.beam: src/merl_transform.erl $(HEADERS) Makefile
	erlc -DMERL_NO_TRANSFORM $(ERLC_FLAGS) -o priv/ $<

clean:
	-rm -f priv/merl_transform.beam
	-rm -f $(OBJECTS)
	(cd examples && make clean)

test:
	erl -noshell -pa ebin \
	 -eval 'eunit:test("ebin",[])' \
	 -s init stop

release: clean
	$(MAKE) ERLC_FLAGS="$(ERLC_FLAGS) -DNOTEST"

docs:
	erl -pa ./ebin -noshell -eval "edoc:application(merl, \".\", [$(DOC_OPTS)])" -s init stop

.PHONY: examples
examples:
	(cd examples && make)
