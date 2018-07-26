.PHONY = splchk splchk-changed splchk-json splchk-code

KAZOO_DICT = .aspell.en.pws
KAZOO_REPL = .aspell.en.prepl

$(ROOT)/$(KAZOO_DICT):
	@$(file >$(ROOT)/$(KAZOO_DICT),personal_ws-1.1 en 0 utf-8)

$(ROOT)/$(KAZOO_REPL):
	@$(file >$(ROOT)/$(KAZOO_REPL),personal_repl-1.1 en 0 utf-8)

splchk-init: $(ROOT)/$(KAZOO_DICT) $(ROOT)/$(KAZOO_REPL)

ifeq ($(wildcard doc),)
splchk: splchk-init
else
splchk: splchk-init $(addsuffix .chk,$(basename $(shell find doc -wholename "doc/mkdocs*" -prune -o -name "*.md" )))
endif

JSON := $(wildcard "priv/couchdb/schemas/*.json")
ifeq ($(JSON),)
splchk-json: splchk-init
	@echo nothing doing
	@echo $(wildcard "priv/couchdb/schemas/*.json")
else
splchk-json: splchk-init $(addsuffix .chk,$(basename $(wildcard "priv/couchdb/schemas/*.json")))
	@echo $(wildcard "priv/couchdb/schemas/*.json")
endif

ESCRIPTS := $(shell find scripts -name *.escript)
SRC := $(shell find src -name *.*rl)
CODE := $(SRC) $(ESCRIPTS)
ifeq ($(CODE),)
splchk-code: splchk-init
else
splchk-code: splchk-init $(addsuffix .chk,$(basename $(CODE)))
endif

splchk-changed: splchk-init $(addsuffix .chk,$(basename $(CHANGED)))

%.chk: %.md
	@aspell --home-dir=$(ROOT) --personal=$(KAZOO_DICT) --repl=$(KAZOO_REPL) --lang=en -x check $<

%.chk: %.json
	aspell --home-dir=$(ROOT) --personal=$(KAZOO_DICT) --repl=$(KAZOO_REPL) --lang=en -x check $<

%.chk: %.erl
	@aspell --add-filter-path=$(ROOT) --mode=erlang --home-dir=$(ROOT) --personal=$(KAZOO_DICT) --repl=$(KAZOO_REPL) --lang=en -x check $<

%.chk: %.escript
	@aspell --add-filter-path=$(ROOT) --mode=erlang --home-dir=$(ROOT) --personal=$(KAZOO_DICT) --repl=$(KAZOO_REPL) --lang=en -x check $<

%.chk: %.hrl
	@aspell --add-filter-path=$(ROOT) --mode=erlang --home-dir=$(ROOT) --personal=$(KAZOO_DICT) --repl=$(KAZOO_REPL) --lang=en -x check $<
