# Copyright (c) 2013, Loïc Hoguin <essen@ninenines.eu>
#
# Permission to use, copy, modify, and/or distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

# Project.

PROJECT ?= $(notdir $(CURDIR))

# Packages database file.

PKG_FILE ?= $(CURDIR)/.erlang.mk.packages.v1
export PKG_FILE

PKG_FILE_URL ?= https://raw.github.com/extend/erlang.mk/master/packages.v1.txt

define get_pkg_file
	wget -O $(PKG_FILE) $(PKG_FILE_URL)
endef

# Verbosity and tweaks.

V ?= 0

appsrc_verbose_0 = @echo " APP   " $(PROJECT).app.src;
appsrc_verbose = $(appsrc_verbose_$(V))

erlc_verbose_0 = @echo " ERLC  " $(filter %.erl %.core,$(?F));
erlc_verbose = $(erlc_verbose_$(V))

xyrl_verbose_0 = @echo " XYRL  " $(filter %.xrl %.yrl,$(?F));
xyrl_verbose = $(xyrl_verbose_$(V))

dtl_verbose_0 = @echo " DTL   " $(filter %.dtl,$(?F));
dtl_verbose = $(dtl_verbose_$(V))

gen_verbose_0 = @echo " GEN   " $@;
gen_verbose = $(gen_verbose_$(V))

.PHONY: all clean-all app clean deps clean-deps docs clean-docs \
	build-tests tests build-plt dialyze

# Deps directory.

DEPS_DIR ?= $(CURDIR)/deps
export DEPS_DIR

REBAR_DEPS_DIR = $(DEPS_DIR)
export REBAR_DEPS_DIR

ALL_DEPS_DIRS = $(addprefix $(DEPS_DIR)/,$(DEPS))
ALL_TEST_DEPS_DIRS = $(addprefix $(DEPS_DIR)/,$(TEST_DEPS))

# Application.

ERLC_OPTS ?= -Werror +debug_info +warn_export_all +warn_export_vars \
	+warn_shadow_vars +warn_obsolete_guard # +bin_opt_info +warn_missing_spec
COMPILE_FIRST ?=
COMPILE_FIRST_PATHS = $(addprefix src/,$(addsuffix .erl,$(COMPILE_FIRST)))

all: deps app

clean-all: clean clean-deps clean-docs
	$(gen_verbose) rm -rf .$(PROJECT).plt $(DEPS_DIR) logs

app: ebin/$(PROJECT).app
	$(eval MODULES := $(shell find ebin -name \*.beam \
		| sed 's/ebin\///;s/\.beam/,/' | sed '$$s/.$$//'))
	$(appsrc_verbose) cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(MODULES)\]}/' \
		> ebin/$(PROJECT).app

define compile_erl
	$(erlc_verbose) ERL_LIBS=$(DEPS_DIR) erlc -v $(ERLC_OPTS) -o ebin/ \
		-pa ebin/ -I include/ $(COMPILE_FIRST_PATHS) $(1)
endef

define compile_xyrl
	$(xyrl_verbose) erlc -v -o ebin/ $(1)
	$(xyrl_verbose) erlc $(ERLC_OPTS) -o ebin/ ebin/*.erl
	@rm ebin/*.erl
endef

define compile_dtl
	$(dtl_verbose) erl -noshell -pa ebin/ $(DEPS_DIR)/erlydtl/ebin/ -eval ' \
		Compile = fun(F) -> \
			Module = list_to_atom( \
				string:to_lower(filename:basename(F, ".dtl")) ++ "_dtl"), \
			erlydtl_compiler:compile(F, Module, [{out_dir, "ebin/"}]) \
		end, \
		_ = [Compile(F) || F <- string:tokens("$(1)", " ")], \
		init:stop()'
endef

ebin/$(PROJECT).app: src/*.erl $(wildcard src/*.core) \
		$(wildcard src/*.xrl) $(wildcard src/*.yrl) \
		$(wildcard templates/*.dtl)
	@mkdir -p ebin/
	$(if $(strip $(filter %.erl %.core,$?)), \
		$(call compile_erl,$(filter %.erl %.core,$?)))
	$(if $(strip $(filter %.xrl %.yrl,$?)), \
		$(call compile_xyrl,$(filter %.xrl %.yrl,$?)))
	$(if $(strip $(filter %.dtl,$?)), \
		$(call compile_dtl,$(filter %.dtl,$?)))

clean:
	$(gen_verbose) rm -rf ebin/ test/*.beam erl_crash.dump

# Dependencies.

define get_dep
	@mkdir -p $(DEPS_DIR)
ifeq (,$(findstring pkg://,$(word 1,$(dep_$(1)))))
	git clone -n -- $(word 1,$(dep_$(1))) $(DEPS_DIR)/$(1)
else
	@if [ ! -f $(PKG_FILE) ]; then $(call get_pkg_file); fi
	git clone -n -- `awk 'BEGIN { FS = "\t" }; \
		$$$$1 == "$(subst pkg://,,$(word 1,$(dep_$(1))))" { print $$$$2 }' \
		$(PKG_FILE)` $(DEPS_DIR)/$(1)
endif
	cd $(DEPS_DIR)/$(1) ; git checkout -q $(word 2,$(dep_$(1)))
endef

define dep_target
$(DEPS_DIR)/$(1):
	$(call get_dep,$(1))
endef

$(foreach dep,$(DEPS),$(eval $(call dep_target,$(dep))))

deps: $(ALL_DEPS_DIRS)
	@for dep in $(ALL_DEPS_DIRS) ; do \
		if [ -f $$dep/Makefile ] ; then \
			$(MAKE) -C $$dep ; \
		else \
			echo "include $(CURDIR)/erlang.mk" | $(MAKE) -f - -C $$dep ; \
		fi ; \
	done

clean-deps:
	@for dep in $(ALL_DEPS_DIRS) ; do $(MAKE) -C $$dep clean; done

# Documentation.

docs: clean-docs
	$(gen_verbose) erl -noshell \
		-eval 'edoc:application($(PROJECT), ".", []), init:stop().'

clean-docs:
	$(gen_verbose) rm -f doc/*.css doc/*.html doc/*.png doc/edoc-info

# Tests.

$(foreach dep,$(TEST_DEPS),$(eval $(call dep_target,$(dep))))

build-test-deps: $(ALL_TEST_DEPS_DIRS)
	@for dep in $(ALL_TEST_DEPS_DIRS) ; do $(MAKE) -C $$dep; done

build-tests: build-test-deps
	$(gen_verbose) ERL_LIBS=$(DEPS_DIR) erlc -v $(ERLC_OPTS) -o test/ \
		$(wildcard test/*.erl test/*/*.erl) -pa ebin/

CT_RUN = ct_run \
	-no_auto_compile \
	-noshell \
	-pa ebin $(DEPS_DIR)/*/ebin \
	-dir test \
	-logdir logs
#	-cover test/cover.spec

CT_SUITES ?=
CT_SUITES_FULL = $(addsuffix _SUITE,$(CT_SUITES))

tests: ERLC_OPTS += -DTEST=1 +'{parse_transform, eunit_autoexport}'
tests: clean deps app build-tests
	@mkdir -p logs/
	@$(CT_RUN) -suite $(CT_SUITES_FULL)
	$(gen_verbose) rm -f test/*.beam

# Dialyzer.

PLT_APPS ?=
DIALYZER_OPTS ?= -Werror_handling -Wrace_conditions \
	-Wunmatched_returns # -Wunderspecs

build-plt: deps app
	@dialyzer --build_plt --output_plt .$(PROJECT).plt \
		--apps erts kernel stdlib $(PLT_APPS) $(ALL_DEPS_DIRS)

dialyze:
	@dialyzer --src src --plt .$(PROJECT).plt --no_native $(DIALYZER_OPTS)

# Packages.

$(PKG_FILE):
	@$(call get_pkg_file)

pkg-list: $(PKG_FILE)
	@cat $(PKG_FILE) | awk 'BEGIN { FS = "\t" }; { print \
		"Name:\t\t" $$1 "\n" \
		"Repository:\t" $$2 "\n" \
		"Website:\t" $$3 "\n" \
		"Description:\t" $$4 "\n" }'

ifdef q
pkg-search: $(PKG_FILE)
	@cat $(PKG_FILE) | grep -i ${q} | awk 'BEGIN { FS = "\t" }; { print \
		"Name:\t\t" $$1 "\n" \
		"Repository:\t" $$2 "\n" \
		"Website:\t" $$3 "\n" \
		"Description:\t" $$4 "\n" }'
else
pkg-search:
	@echo "Usage: make pkg-search q=STRING"
endif
