ERLC_FLAGS=+debug_info
SOURCES=merl_build.erl lisp.erl lispc.erl basic.erl basicc.erl
INCLUDES=../include
HEADERS=$(INCLUDES)/merl.hrl
OBJECTS=$(SOURCES:%.erl=%.beam)

all: $(OBJECTS) test

%.beam: %.erl $(HEADERS) Makefile
	erlc -pa ../ebin $(ERLC_FLAGS) -o ./ $<

# additional dependencies due to the parse transform
lispc.beam basicc.beam: ../ebin/merl_transform.beam ../ebin/merl.beam

clean:
	-rm -f $(OBJECTS)

test:
	erl -noshell -pa ../ebin \
	 -eval 'eunit:test([lisp, lispc, basic, basicc],[])' \
	 -s init stop
