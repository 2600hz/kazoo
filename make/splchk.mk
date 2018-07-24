.PHONY = splchk splchk-changed

KAZOO_DICT = .aspell.en.pws
KAZOO_REPL = .aspell.en.prepl

$(ROOT)/$(KAZOO_DICT):
	@$(file >$(ROOT)/$(KAZOO_DICT),personal_ws-1.1 en 0 utf-8)

$(ROOT)/$(KAZOO_REPL):
	@$(file >$(ROOT)/$(KAZOO_REPL),personal_repl-1.1 en 0 utf-8)

splchk: $(ROOT)/$(KAZOO_DICT) $(ROOT)/$(KAZOO_REPL) $(addsuffix .chk,$(basename $(shell find doc -name "*.md" -path "doc/mkdocs*" -prune)))

splchk-changed: $(ROOT)/$(KAZOO_DICT) $(ROOT)/$(KAZOO_REPL) $(addsuffix .chk,$(basename $(CHANGED)))

splchk-json: $(ROOT)/$(KAZOO_DICT) $(ROOT)/$(KAZOO_REPL) $(addsuffix .chk,$(basename $(shell find . -name *.json -wholename "*/schemas/*")))

%.chk: %.md
	@echo Spellchecking $<
	aspell --home-dir=$(ROOT) --personal=$(KAZOO_DICT) --repl=$(KAZOO_REPL) --lang=en -x check $<

%.chk: %.json
	@echo Spellchecking $<
	aspell --home-dir=$(ROOT) --personal=$(KAZOO_DICT) --repl=$(KAZOO_REPL) --lang=en -x check $<
