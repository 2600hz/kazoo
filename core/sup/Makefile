ROOT = ../..
PROJECT = sup

ESCRIPT = sup
CLEAN_MOAR = clean-escript

all: compile $(ESCRIPT)

$(ESCRIPT): src/sup.erl
	@$(REBAR) escriptize

clean-escript:
	@rm -f $(ESCRIPT)

include $(ROOT)/make/kz.mk
