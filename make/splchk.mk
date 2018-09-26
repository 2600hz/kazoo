.PHONY = splchk splchk-docs splchk-changed splchk-json splchk-code

KAZOO_DICT = .aspell.en.pws
KAZOO_REPL = .aspell.en.prepl

$(ROOT)/$(KAZOO_DICT):
	@$(file >$(ROOT)/$(KAZOO_DICT),personal_ws-1.1 en 0 utf-8)

$(ROOT)/$(KAZOO_REPL):
	@$(file >$(ROOT)/$(KAZOO_REPL),personal_repl-1.1 en 0 utf-8)

splchk-init: $(ROOT)/$(KAZOO_DICT) $(ROOT)/$(KAZOO_REPL)

splchk: splchk-changed

ifeq ($(wildcard $(CURDIR)/doc/*.md),)
splchk-docs:: splchk-init
else
DOCS := $(shell find $(CURDIR)/doc -name "*.md" -not -path "$(CURDIR)/doc/mkdocs/docs*" -not path "$(CURDIR)/doc/ref")
splchk-docs:: splchk-init $(addsuffix .chk,$(basename $(DOCS)))
endif

ifneq ($(wildcard $(CURDIR)/priv/templates/*),)
TEMPLATES := $(shell find $(CURDIR)/priv/templates -type f)
splchk-docs:: splchk-init $(addsuffix .chk,$(basename $(TEMPLATES)))
endif

ifneq ($(wildcard $(CURDIR)/test/rendered-templates/*),)
RENDERED_TEMPLATES := $(shell find $(CURDIR)/test/rendered-templates -type f)
splchk-docs:: splchk-init $(addsuffix .chk,$(basename $(RENDERED_TEMPLATES)))
endif

ifneq ($(wildcard $(CURDIR)/priv/*/templates/*),)
TEMPLATES := $(shell find $(CURDIR)/priv/*/templates/ -type f)
splchk-docs:: splchk-init $(addsuffix .chk,$(basename $(TEMPLATES)))
endif

JSON := $(wildcard $(CURDIR)/priv/couchdb/schemas/*.json)
ifeq ($(JSON),)
splchk-json: splchk-init
else
splchk-json: splchk-init $(addsuffix .chk,$(basename $(JSON)))
endif

ESCRIPTS := $(wildcard $(CURDIR)/scripts/*.escript)
SRC := $(wildcard $(CURDIR)/src/*.*rl) $(wildcard $(CURDIR)/src/*/*.erl) $(wildcard $(CURDIR)/include/*.hrl)
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
	@aspell --home-dir=$(ROOT) --personal=$(KAZOO_DICT) --repl=$(KAZOO_REPL) --lang=en -x check $<

%.chk: %.erl
	@aspell --add-filter-path=$(ROOT) --mode=erlang --home-dir=$(ROOT) --personal=$(KAZOO_DICT) --repl=$(KAZOO_REPL) --lang=en -x check $<

%.chk: %.escript
	@aspell --add-filter-path=$(ROOT) --mode=erlang --home-dir=$(ROOT) --personal=$(KAZOO_DICT) --repl=$(KAZOO_REPL) --lang=en -x check $<

%.chk: %.hrl
	@aspell --add-filter-path=$(ROOT) --mode=erlang --home-dir=$(ROOT) --personal=$(KAZOO_DICT) --repl=$(KAZOO_REPL) --lang=en -x check $<

%.chk: %.html
	@aspell --add-filter-path=$(ROOT) --mode=html --home-dir=$(ROOT) --personal=$(KAZOO_DICT) --repl=$(KAZOO_REPL) --lang=en -x check $<

%.chk: %.text
	@aspell --add-filter-path=$(ROOT) --home-dir=$(ROOT) --personal=$(KAZOO_DICT) --repl=$(KAZOO_REPL) --lang=en -x check $<

%.chk: %.tmpl
	@aspell --add-filter-path=$(ROOT) --home-dir=$(ROOT) --personal=$(KAZOO_DICT) --repl=$(KAZOO_REPL) --lang=en -x check $<

%.chk: Makefile
	@aspell --add-filter-path=$(ROOT) --mode=erlang --home-dir=$(ROOT) --personal=$(KAZOO_DICT) --repl=$(KAZOO_REPL) --lang=en -x check $<
