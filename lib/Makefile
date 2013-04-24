ROOT = ..
REBAR = $(ROOT)/bin/rebar

MAKEDIRS = */Makefile

.PHONY: all compile clean $(MAKEDIRS)

all: compile

compile: ACTION = all
compile: $(MAKEDIRS)
	$(REBAR) compile

clean: ACTION = clean
clean: $(MAKEDIRS)
	rm -rf basho-lager_syslog/ lager-1.0.0/ syslog-0.0.1/
	$(REBAR) clean

$(MAKEDIRS):
	$(MAKE) -C $(@D) $(ACTION)
