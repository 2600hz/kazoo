
%.beam: %.erl
	erlc -o t/ $<

all: deps
	@mkdir -p ebin
	@./rebar compile

deps:
	@rebar get-deps

check: t/etap.beam t/util.beam
	@prove t/*.t

check_verbose: t/etap.beam t/util.beam
	@prove -v t/*.t

clean:
	@./rebar clean
	@rm -f t/*.beam
