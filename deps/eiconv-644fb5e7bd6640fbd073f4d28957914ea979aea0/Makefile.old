REBAR := $(shell which rebar 2>/dev/null || echo ./rebar)
REBAR_URL := https://github.com/downloads/basho/rebar/rebar

all: compile

./rebar:
	erl -noshell -s inets start -s ssl start \
        -eval '{ok, saved_to_file} = httpc:request(get, {"$(REBAR_URL)", []}, [], [{stream, "./rebar"}])' \
        -s inets stop -s init stop
	chmod +x ./rebar

compile: rebar
	$(REBAR) compile

test: compile
	$(REBAR) eunit

clean: rebar
	$(REBAR) clean

distclean: 
	rm $(REBAR)
