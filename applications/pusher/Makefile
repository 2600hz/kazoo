NAME=pusher
ROOT=../..
#REBAR=$(ROOT)/utils/rebar/rebar
REBAR=./rebar
OTP_PLT=~/.otp.plt
PRJ_PLT=$(NAME).plt

.PHONY: test

all: compile xref

generate: compile xref
	@rm -rf ./rel/$(NAME)
	@$(REBAR) generate

escriptize: compile xref
	@$(REBAR) escriptize

compile: get-deps
	@$(REBAR) compile

xref: compile
	@$(REBAR) xref skip_deps=true

get-deps:
	@$(REBAR) get-deps

update-deps:
	@$(REBAR) update-deps

clean:
	@$(REBAR) clean

test: xref
	@$(REBAR) eunit skip_deps=true

dialyze: $(OTP_PLT) compile $(PRJ_PLT)
	@dialyzer --plt $(PRJ_PLT) -r ./subapps/*/ebin

$(OTP_PLT):
	@dialyzer --build_plt --output_plt $(OTP_PLT) --apps erts \
		kernel stdlib crypto mnesia sasl common_test eunit ssl \
		asn1 compiler syntax_tools inets

$(PRJ_PLT):
	@dialyzer --add_to_plt --plt $(OTP_PLT) --output_plt $(PRJ_PLT) \
	-r ./deps/*/ebin ./subapps/*/ebin

console:
	@./rel/$(NAME)/bin/$(NAME) console

develop:
	@./rel/$(NAME)/bin/$(NAME) develop

tags:
	@find . -name "*.[e,h]rl" -print | etags -
