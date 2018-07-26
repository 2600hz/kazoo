.PHONY = splchk splchk-changed

KAZOO_DICT = .aspell.en.pws
KAZOO_REPL = .aspell.en.prepl

$(ROOT)/$(KAZOO_DICT):
	@$(file >$(ROOT)/$(KAZOO_DICT),personal_ws-1.1 en 0 utf-8)

$(ROOT)/$(KAZOO_REPL):
	@$(file >$(ROOT)/$(KAZOO_REPL),personal_repl-1.1 en 0 utf-8)

ifeq ($(wildcard doc),)
splchk: $(ROOT)/$(KAZOO_DICT) $(ROOT)/$(KAZOO_REPL)
else
splchk: $(ROOT)/$(KAZOO_DICT) $(ROOT)/$(KAZOO_REPL) $(addsuffix .chk,$(basename $(shell find doc -wholename "doc/mkdocs*" -prune -o -name "*.md" )))
endif

ifeq ($(wildcard "priv/couchdb/schemas"),)
splchk-json: $(ROOT)/$(KAZOO_DICT) $(ROOT)/$(KAZOO_REPL)
else
splchk-json: $(ROOT)/$(KAZOO_DICT) $(ROOT)/$(KAZOO_REPL) $(addsuffix .chk,$(basename $(shell find . -name *.json -wholename "*/schemas/*")))
endif

splchk-changed: $(ROOT)/$(KAZOO_DICT) $(ROOT)/$(KAZOO_REPL) $(addsuffix .chk,$(basename $(CHANGED)))

%.chk: %.md
	@aspell --home-dir=$(ROOT) --personal=$(KAZOO_DICT) --repl=$(KAZOO_REPL) --lang=en -x check $<

%.chk: %.json
	@aspell --home-dir=$(ROOT) --personal=$(KAZOO_DICT) --repl=$(KAZOO_REPL) --lang=en -x check $<
