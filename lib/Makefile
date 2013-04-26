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
	rm -rf erlang-syslog-1.0.1/ lager-2.0.0rc1/ lager_syslog-1.2.2/
	$(REBAR) clean

$(MAKEDIRS):
	$(MAKE) -C $(@D) $(ACTION)
