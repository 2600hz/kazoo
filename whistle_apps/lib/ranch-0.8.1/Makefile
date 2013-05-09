# See LICENSE for licensing information.

PROJECT = ranch
ERLC_OPTS = -Werror +debug_info +warn_export_all # +bin_opt_info +warn_missing_spec

DEPS_DIR ?= $(CURDIR)/deps
export DEPS_DIR

# Makefile tweaks.

V ?= 0

appsrc_verbose_0 = @echo " APP   " $(PROJECT).app.src;
appsrc_verbose = $(appsrc_verbose_$(V))

erlc_verbose_0 = @echo " ERLC  " $(?F);
erlc_verbose = $(erlc_verbose_$(V))

gen_verbose_0 = @echo " GEN   " $@;
gen_verbose = $(gen_verbose_$(V))

.PHONY: all clean-all app clean docs clean-docs tests build-plt dialyze

# Application.

all: app

clean-all: clean clean-docs
	$(gen_verbose) rm -rf .$(PROJECT).plt $(DEPS_DIR) logs

MODULES = $(shell ls src/*.erl | sed 's/src\///;s/\.erl/,/' | sed '$$s/.$$//')

app: ebin/$(PROJECT).app
	$(appsrc_verbose) cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(MODULES)\]}/' \
		> ebin/$(PROJECT).app

ebin/$(PROJECT).app: src/*.erl
	@mkdir -p ebin/
	$(erlc_verbose) erlc -v $(ERLC_OPTS) -o ebin/ -pa ebin/ \
		src/$(PROJECT)_transport.erl $?

clean:
	$(gen_verbose) rm -rf ebin/ test/*.beam erl_crash.dump

# Documentation.

docs: clean-docs
	$(gen_verbose) erl -noshell \
		-eval 'edoc:application($(PROJECT), ".", []), init:stop().'

clean-docs:
	$(gen_verbose) rm -f doc/*.css doc/*.html doc/*.png doc/edoc-info

# Tests.

CT_RUN = ct_run \
	-pa ebin $(DEPS_DIR)/*/ebin \
	-dir test \
	-logdir logs \
	-cover test/cover.spec

tests: clean app
	@mkdir -p logs/
	@$(CT_RUN) -suite acceptor_SUITE

# Dialyzer.

build-plt: app
	@dialyzer --build_plt --output_plt .$(PROJECT).plt \
		--apps erts kernel stdlib crypto public_key ssl

dialyze:
	@dialyzer --src src --plt .$(PROJECT).plt \
		-Werror_handling -Wrace_conditions -Wunmatched_returns # -Wunderspecs
