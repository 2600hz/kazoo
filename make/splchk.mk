.PHONY = splchk splchk-changed

KAZOO_DICT = $(ROOT)/.aspell.en-kazoo.pws

$(KAZOO_DICT):
	@$(file >$(KAZOO_DICT),personal_ws-1.1 en 0 utf-8)

splchk-changed: $(KAZOO_DICT) $(addsuffix .chk,$(basename $(CHANGED)))
splchk: $(KAZOO_DICT) $(addsuffix .chk,$(basename $(wildcard doc/*.md)))

%.chk: %.md
	@echo Spellchecking $< $@
	aspell --home-dir=$(ROOT) --personal=$(KAZOO_DICT) --lang=en --jargon=kazoo check $<
	@$(if $(wildcard doc/*.bak), rm doc/*.bak)
