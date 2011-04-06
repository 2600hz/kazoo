.PHONY: default all compile dialyze tests doc clean distclean rebuild retest

default: compile

all: compile doc

compile:
	./rebar compile

dialyze: compile
	./rebar dialyze

tests:
	./rebar eunit

doc:
	./rebar doc

clean:
	./clean_temp.sh

distclean: clean
	./rebar clean

rebuild:
	./rebar clean
	./rebar compile

retest:
	rm -rf .eunit
	./rebar eunit
