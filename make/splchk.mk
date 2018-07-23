.PHONY = splchk splchk-changed

KAZOO_DICT = $(ROOT)/.aspell.en.pws
KAZOO_REPL = $(ROOT)/.aspell.en.prepl

$(KAZOO_DICT):
	@$(file >$(KAZOO_DICT),personal_ws-1.1 en 0 utf-8)

splchk-changed: $(KAZOO_DICT) $(addsuffix .chk,$(basename $(CHANGED)))
splchk: $(KAZOO_DICT) $(addsuffix .chk,$(basename $(wildcard doc/*.md)))
splchk-json: $(KAZOO_DICT)

%.chk: %.md
	@echo Spellchecking $<
	aspell --home-dir=$(ROOT) --personal=$(basename $(KAZOO_DICT)) --repl=$(basename $(KAZOO_REPL)) --lang=en --jargon=kazoo -x check $<
