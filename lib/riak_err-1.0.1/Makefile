all: app

app: compile

compile:
	./rebar compile

clean:
	./rebar clean

test: all
	./rebar eunit

build_plt: all
	./rebar build-plt

analyze: all
	./rebar dialyze

doc: all
	./rebar doc

xref: all
	./rebar xref
