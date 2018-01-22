ROOT = ../..
PROJECT = sup

ESCRIPT = sup
CLEAN_MOAR = clean-escript

all: compile $(ESCRIPT)

$(ESCRIPT):
	@$(REBAR) escriptize

clean-escript:
	@rm -f $(ESCRIPT)

include $(ROOT)/make/kz.mk
