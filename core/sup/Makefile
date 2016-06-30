ROOT = ../..
PROJECT = sup

all: $(PROJECT)

$(PROJECT): ebin
	@$(REBAR) escriptize

ebin: $(SOURCES)
	@$(REBAR) compile

include $(ROOT)/make/kz.mk
