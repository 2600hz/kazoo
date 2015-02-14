# Copyright (c) 2013-2014, Loïc Hoguin <essen@ninenines.eu>
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

.PHONY: all deps app rel docs tests clean distclean help erlang-mk

ERLANG_MK_VERSION = 1

# Core configuration.

PROJECT ?= $(notdir $(CURDIR))
PROJECT := $(strip $(PROJECT))

# Verbosity.

V ?= 0

gen_verbose_0 = @echo " GEN   " $@;
gen_verbose = $(gen_verbose_$(V))

# Core targets.

all:: deps app rel

clean::
	$(gen_verbose) rm -f erl_crash.dump

distclean:: clean

help::
	@printf "%s\n" \
		"erlang.mk (version $(ERLANG_MK_VERSION)) is distributed under the terms of the ISC License." \
		"Copyright (c) 2013-2014 Loïc Hoguin <essen@ninenines.eu>" \
		"" \
		"Usage: [V=1] make [target]" \
		"" \
		"Core targets:" \
		"  all         Run deps, app and rel targets in that order" \
		"  deps        Fetch dependencies (if needed) and compile them" \
		"  app         Compile the project" \
		"  rel         Build a release for this project, if applicable" \
		"  docs        Build the documentation for this project" \
		"  tests       Run the tests for this project" \
		"  clean       Delete temporary and output files from most targets" \
		"  distclean   Delete all temporary and output files" \
		"  help        Display this help and exit" \
		"" \
		"The target clean only removes files that are commonly removed." \
		"Dependencies and releases are left untouched." \
		"" \
		"Setting V=1 when calling make enables verbose mode."

# Core functions.

ifeq ($(shell which wget 2>/dev/null | wc -l), 1)
define core_http_get
	wget --no-check-certificate -O $(1) $(2)|| rm $(1)
endef
else
define core_http_get
	erl -noshell -eval 'ssl:start(), inets:start(), case httpc:request(get, {"$(2)", []}, [{autoredirect, true}], []) of {ok, {{_, 200, _}, _, Body}} -> case file:write_file("$(1)", Body) of ok -> ok; {error, R1} -> halt(R1) end; {error, R2} -> halt(R2) end, halt(0).'
endef
endif

# Automated update.

ERLANG_MK_BUILD_CONFIG ?= build.config
ERLANG_MK_BUILD_DIR ?= .erlang.mk.build

erlang-mk:
	git clone https://github.com/ninenines/erlang.mk $(ERLANG_MK_BUILD_DIR)
	if [ -f $(ERLANG_MK_BUILD_CONFIG) ]; then cp $(ERLANG_MK_BUILD_CONFIG) $(ERLANG_MK_BUILD_DIR); fi
	cd $(ERLANG_MK_BUILD_DIR) && make
	cp $(ERLANG_MK_BUILD_DIR)/erlang.mk ./erlang.mk
	rm -rf $(ERLANG_MK_BUILD_DIR)

# Copyright (c) 2013-2014, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: distclean-deps distclean-pkg pkg-list pkg-search

# Configuration.

DEPS_DIR ?= $(CURDIR)/deps
export DEPS_DIR

REBAR_DEPS_DIR = $(DEPS_DIR)
export REBAR_DEPS_DIR

ALL_DEPS_DIRS = $(addprefix $(DEPS_DIR)/,$(DEPS))

ifeq ($(filter $(DEPS_DIR),$(subst :, ,$(ERL_LIBS))),)
ifeq ($(ERL_LIBS),)
	ERL_LIBS = $(DEPS_DIR)
else
	ERL_LIBS := $(ERL_LIBS):$(DEPS_DIR)
endif
endif
export ERL_LIBS

PKG_FILE2 ?= $(CURDIR)/.erlang.mk.packages.v2
export PKG_FILE2

PKG_FILE_URL ?= https://raw.githubusercontent.com/ninenines/erlang.mk/master/packages.v2.tsv

# Core targets.

deps:: $(ALL_DEPS_DIRS)
	@for dep in $(ALL_DEPS_DIRS) ; do \
		if [ -f $$dep/GNUmakefile ] || [ -f $$dep/makefile ] || [ -f $$dep/Makefile ] ; then \
			$(MAKE) -C $$dep ; \
		else \
			echo "include $(CURDIR)/erlang.mk" | ERLC_OPTS=+debug_info $(MAKE) -f - -C $$dep ; \
		fi ; \
	done

distclean:: distclean-deps distclean-pkg

# Deps related targets.

define dep_fetch
	if [ "$$$$VS" = "git" ]; then \
		git clone -n -- $$$$REPO $(DEPS_DIR)/$(1); \
		cd $(DEPS_DIR)/$(1) && git checkout -q $$$$COMMIT; \
	elif [ "$$$$VS" = "hg" ]; then \
		hg clone -U $$$$REPO $(DEPS_DIR)/$(1); \
		cd $(DEPS_DIR)/$(1) && hg update -q $$$$COMMIT; \
	else \
		echo "Unknown or invalid dependency: $(1). Please consult the erlang.mk README for instructions." >&2; \
		exit 78; \
	fi
endef

define dep_target
$(DEPS_DIR)/$(1):
	@mkdir -p $(DEPS_DIR)
ifeq (,$(dep_$(1)))
	@if [ ! -f $(PKG_FILE2) ]; then $(call core_http_get,$(PKG_FILE2),$(PKG_FILE_URL)); fi
	@DEPPKG=$$$$(awk 'BEGIN { FS = "\t" }; $$$$1 == "$(1)" { print $$$$2 " " $$$$3 " " $$$$4 }' $(PKG_FILE2);); \
	VS=$$$$(echo $$$$DEPPKG | cut -d " " -f1); \
	REPO=$$$$(echo $$$$DEPPKG | cut -d " " -f2); \
	COMMIT=$$$$(echo $$$$DEPPKG | cut -d " " -f3); \
	$(call dep_fetch,$(1))
else
	@VS=$(word 1,$(dep_$(1))); \
	REPO=$(word 2,$(dep_$(1))); \
	COMMIT=$(word 3,$(dep_$(1))); \
	$(call dep_fetch,$(1))
endif
endef

$(foreach dep,$(DEPS),$(eval $(call dep_target,$(dep))))

distclean-deps:
	$(gen_verbose) rm -rf $(DEPS_DIR)

# Packages related targets.

$(PKG_FILE2):
	@$(call core_http_get,$(PKG_FILE2),$(PKG_FILE_URL))

pkg-list: $(PKG_FILE2)
	@cat $(PKG_FILE2) | awk 'BEGIN { FS = "\t" }; { print \
		"Name:\t\t" $$1 "\n" \
		"Repository:\t" $$3 "\n" \
		"Website:\t" $$5 "\n" \
		"Description:\t" $$6 "\n" }'

ifdef q
pkg-search: $(PKG_FILE2)
	@cat $(PKG_FILE2) | grep -i ${q} | awk 'BEGIN { FS = "\t" }; { print \
		"Name:\t\t" $$1 "\n" \
		"Repository:\t" $$3 "\n" \
		"Website:\t" $$5 "\n" \
		"Description:\t" $$6 "\n" }'
else
pkg-search:
	$(error Usage: make pkg-search q=STRING)
endif

ifeq ($(PKG_FILE2),$(CURDIR)/.erlang.mk.packages.v2)
distclean-pkg:
	$(gen_verbose) rm -f $(PKG_FILE2)
endif

help::
	@printf "%s\n" "" \
		"Package-related targets:" \
		"  pkg-list              List all known packages" \
		"  pkg-search q=STRING   Search for STRING in the package index"

# Copyright (c) 2013-2014, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: clean-app

# Configuration.

ERLC_OPTS ?= -Werror +debug_info +warn_export_vars +warn_shadow_vars \
	+warn_obsolete_guard # +bin_opt_info +warn_export_all +warn_missing_spec
COMPILE_FIRST ?=
COMPILE_FIRST_PATHS = $(addprefix src/,$(addsuffix .erl,$(COMPILE_FIRST)))
ERLC_EXCLUDE ?=
ERLC_EXCLUDE_PATHS = $(addprefix src/,$(addsuffix .erl,$(ERLC_EXCLUDE)))

ERLC_MIB_OPTS ?=
COMPILE_MIB_FIRST ?=
COMPILE_MIB_FIRST_PATHS = $(addprefix mibs/,$(addsuffix .mib,$(COMPILE_MIB_FIRST)))

# Verbosity.

appsrc_verbose_0 = @echo " APP   " $(PROJECT).app.src;
appsrc_verbose = $(appsrc_verbose_$(V))

erlc_verbose_0 = @echo " ERLC  " $(filter-out $(patsubst %,%.erl,$(ERLC_EXCLUDE)),\
	$(filter %.erl %.core,$(?F)));
erlc_verbose = $(erlc_verbose_$(V))

xyrl_verbose_0 = @echo " XYRL  " $(filter %.xrl %.yrl,$(?F));
xyrl_verbose = $(xyrl_verbose_$(V))

mib_verbose_0 = @echo " MIB   " $(filter %.bin %.mib,$(?F));
mib_verbose = $(mib_verbose_$(V))

# Core targets.

app:: erlc-include ebin/$(PROJECT).app
	$(eval MODULES := $(shell find ebin -type f -name \*.beam \
		| sed "s/ebin\//'/;s/\.beam/',/" | sed '$$s/.$$//'))
	@if [ -z "$$(grep -E '^[^%]*{modules,' src/$(PROJECT).app.src)" ]; then \
		echo "Empty modules entry not found in $(PROJECT).app.src. Please consult the erlang.mk README for instructions." >&2; \
		exit 1; \
	fi
	$(eval GITDESCRIBE := $(shell git describe --dirty --abbrev=7 --tags --always --first-parent 2>/dev/null || true))
	$(appsrc_verbose) cat src/$(PROJECT).app.src \
		| sed "s/{modules,[[:space:]]*\[\]}/{modules, \[$(MODULES)\]}/" \
		| sed "s/{id,[[:space:]]*\"git\"}/{id, \"$(GITDESCRIBE)\"}/" \
		> ebin/$(PROJECT).app

define compile_erl
	$(erlc_verbose) erlc -v $(ERLC_OPTS) -o ebin/ \
		-pa ebin/ -I include/ $(filter-out $(ERLC_EXCLUDE_PATHS),\
		$(COMPILE_FIRST_PATHS) $(1))
endef

define compile_xyrl
	$(xyrl_verbose) erlc -v -o ebin/ $(1)
	$(xyrl_verbose) erlc $(ERLC_OPTS) -o ebin/ ebin/*.erl
	@rm ebin/*.erl
endef

define compile_mib
	$(mib_verbose) erlc -v $(ERLC_MIB_OPTS) -o priv/mibs/ \
		-I priv/mibs/ $(COMPILE_MIB_FIRST_PATHS) $(1)
	$(mib_verbose) erlc -o include/ -- priv/mibs/*.bin
endef

ifneq ($(wildcard src/),)
ebin/$(PROJECT).app::
	@mkdir -p ebin/

ifneq ($(wildcard mibs/),)
ebin/$(PROJECT).app:: $(shell find mibs -type f -name \*.mib)
	@mkdir -p priv/mibs/ include
	$(if $(strip $?),$(call compile_mib,$?))
endif

ebin/$(PROJECT).app:: $(shell find src -type f -name \*.erl) \
		$(shell find src -type f -name \*.core)
	$(if $(strip $?),$(call compile_erl,$?))

ebin/$(PROJECT).app:: $(shell find src -type f -name \*.xrl) \
		$(shell find src -type f -name \*.yrl)
	$(if $(strip $?),$(call compile_xyrl,$?))
endif

clean:: clean-app

# Extra targets.

erlc-include:
	-@if [ -d ebin/ ]; then \
		find include/ src/ -type f -name \*.hrl -newer ebin -exec touch $(shell find src/ -type f -name "*.erl") \; 2>/dev/null || printf ''; \
	fi

clean-app:
	$(gen_verbose) rm -rf ebin/ priv/mibs/ \
		$(addprefix include/,$(addsuffix .hrl,$(notdir $(basename $(wildcard mibs/*.mib)))))

# Copyright (c) 2014, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: bootstrap bootstrap-lib bootstrap-rel new list-templates

# Core targets.

help::
	@printf "%s\n" "" \
		"Bootstrap targets:" \
		"  bootstrap          Generate a skeleton of an OTP application" \
		"  bootstrap-lib      Generate a skeleton of an OTP library" \
		"  bootstrap-rel      Generate the files needed to build a release" \
		"  new t=TPL n=NAME   Generate a module NAME based on the template TPL" \
		"  list-templates     List available templates"

# Bootstrap templates.

bs_appsrc = "{application, $(PROJECT), [" \
	"	{description, \"\"}," \
	"	{vsn, \"0.1.0\"}," \
	"	{id, \"git\"}," \
	"	{modules, []}," \
	"	{registered, []}," \
	"	{applications, [" \
	"		kernel," \
	"		stdlib" \
	"	]}," \
	"	{mod, {$(PROJECT)_app, []}}," \
	"	{env, []}" \
	"]}."
bs_appsrc_lib = "{application, $(PROJECT), [" \
	"	{description, \"\"}," \
	"	{vsn, \"0.1.0\"}," \
	"	{id, \"git\"}," \
	"	{modules, []}," \
	"	{registered, []}," \
	"	{applications, [" \
	"		kernel," \
	"		stdlib" \
	"	]}" \
	"]}."
bs_Makefile = "PROJECT = $(PROJECT)" \
	"include erlang.mk"
bs_app = "-module($(PROJECT)_app)." \
	"-behaviour(application)." \
	"" \
	"-export([start/2])." \
	"-export([stop/1])." \
	"" \
	"start(_Type, _Args) ->" \
	"	$(PROJECT)_sup:start_link()." \
	"" \
	"stop(_State) ->" \
	"	ok."
bs_relx_config = "{release, {$(PROJECT)_release, \"1\"}, [$(PROJECT)]}." \
	"{extended_start_script, true}." \
	"{sys_config, \"rel/sys.config\"}." \
	"{vm_args, \"rel/vm.args\"}."
bs_sys_config = "[" \
	"]."
bs_vm_args = "-name $(PROJECT)@127.0.0.1" \
	"-setcookie $(PROJECT)" \
	"-heart"
# Normal templates.
tpl_supervisor = "-module($(n))." \
	"-behaviour(supervisor)." \
	"" \
	"-export([start_link/0])." \
	"-export([init/1])." \
	"" \
	"start_link() ->" \
	"	supervisor:start_link({local, ?MODULE}, ?MODULE, [])." \
	"" \
	"init([]) ->" \
	"	Procs = []," \
	"	{ok, {{one_for_one, 1, 5}, Procs}}."
tpl_gen_server = "-module($(n))." \
	"-behaviour(gen_server)." \
	"" \
	"%% API." \
	"-export([start_link/0])." \
	"" \
	"%% gen_server." \
	"-export([init/1])." \
	"-export([handle_call/3])." \
	"-export([handle_cast/2])." \
	"-export([handle_info/2])." \
	"-export([terminate/2])." \
	"-export([code_change/3])." \
	"" \
	"-record(state, {" \
	"})." \
	"" \
	"%% API." \
	"" \
	"-spec start_link() -> {ok, pid()}." \
	"start_link() ->" \
	"	gen_server:start_link(?MODULE, [], [])." \
	"" \
	"%% gen_server." \
	"" \
	"init([]) ->" \
	"	{ok, \#state{}}." \
	"" \
	"handle_call(_Request, _From, State) ->" \
	"	{reply, ignored, State}." \
	"" \
	"handle_cast(_Msg, State) ->" \
	"	{noreply, State}." \
	"" \
	"handle_info(_Info, State) ->" \
	"	{noreply, State}." \
	"" \
	"terminate(_Reason, _State) ->" \
	"	ok." \
	"" \
	"code_change(_OldVsn, State, _Extra) ->" \
	"	{ok, State}."
tpl_cowboy_http = "-module($(n))." \
	"-behaviour(cowboy_http_handler)." \
	"" \
	"-export([init/3])." \
	"-export([handle/2])." \
	"-export([terminate/3])." \
	"" \
	"-record(state, {" \
	"})." \
	"" \
	"init(_, Req, _Opts) ->" \
	"	{ok, Req, \#state{}}." \
	"" \
	"handle(Req, State=\#state{}) ->" \
	"	{ok, Req2} = cowboy_req:reply(200, Req)," \
	"	{ok, Req2, State}." \
	"" \
	"terminate(_Reason, _Req, _State) ->" \
	"	ok."
tpl_cowboy_loop = "-module($(n))." \
	"-behaviour(cowboy_loop_handler)." \
	"" \
	"-export([init/3])." \
	"-export([info/3])." \
	"-export([terminate/3])." \
	"" \
	"-record(state, {" \
	"})." \
	"" \
	"init(_, Req, _Opts) ->" \
	"	{loop, Req, \#state{}, 5000, hibernate}." \
	"" \
	"info(_Info, Req, State) ->" \
	"	{loop, Req, State, hibernate}." \
	"" \
	"terminate(_Reason, _Req, _State) ->" \
	"	ok."
tpl_cowboy_rest = "-module($(n))." \
	"" \
	"-export([init/3])." \
	"-export([content_types_provided/2])." \
	"-export([get_html/2])." \
	"" \
	"init(_, _Req, _Opts) ->" \
	"	{upgrade, protocol, cowboy_rest}." \
	"" \
	"content_types_provided(Req, State) ->" \
	"	{[{{<<\"text\">>, <<\"html\">>, '*'}, get_html}], Req, State}." \
	"" \
	"get_html(Req, State) ->" \
	"	{<<\"<html><body>This is REST!</body></html>\">>, Req, State}."
tpl_cowboy_ws = "-module($(n))." \
	"-behaviour(cowboy_websocket_handler)." \
	"" \
	"-export([init/3])." \
	"-export([websocket_init/3])." \
	"-export([websocket_handle/3])." \
	"-export([websocket_info/3])." \
	"-export([websocket_terminate/3])." \
	"" \
	"-record(state, {" \
	"})." \
	"" \
	"init(_, _, _) ->" \
	"	{upgrade, protocol, cowboy_websocket}." \
	"" \
	"websocket_init(_, Req, _Opts) ->" \
	"	Req2 = cowboy_req:compact(Req)," \
	"	{ok, Req2, \#state{}}." \
	"" \
	"websocket_handle({text, Data}, Req, State) ->" \
	"	{reply, {text, Data}, Req, State};" \
	"websocket_handle({binary, Data}, Req, State) ->" \
	"	{reply, {binary, Data}, Req, State};" \
	"websocket_handle(_Frame, Req, State) ->" \
	"	{ok, Req, State}." \
	"" \
	"websocket_info(_Info, Req, State) ->" \
	"	{ok, Req, State}." \
	"" \
	"websocket_terminate(_Reason, _Req, _State) ->" \
	"	ok."
tpl_ranch_protocol = "-module($(n))." \
	"-behaviour(ranch_protocol)." \
	"" \
	"-export([start_link/4])." \
	"-export([init/4])." \
	"" \
	"-type opts() :: []." \
	"-export_type([opts/0])." \
	"" \
	"-record(state, {" \
	"	socket :: inet:socket()," \
	"	transport :: module()" \
	"})." \
	"" \
	"start_link(Ref, Socket, Transport, Opts) ->" \
	"	Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts])," \
	"	{ok, Pid}." \
	"" \
	"-spec init(ranch:ref(), inet:socket(), module(), opts()) -> ok." \
	"init(Ref, Socket, Transport, _Opts) ->" \
	"	ok = ranch:accept_ack(Ref)," \
	"	loop(\#state{socket=Socket, transport=Transport})." \
	"" \
	"loop(State) ->" \
	"	loop(State)."

# Plugin-specific targets.

bootstrap:
ifneq ($(wildcard src/),)
	$(error Error: src/ directory already exists)
endif
	@printf "%s\n" $(bs_Makefile) > Makefile
	@mkdir src/
	@printf "%s\n" $(bs_appsrc) > src/$(PROJECT).app.src
	@printf "%s\n" $(bs_app) > src/$(PROJECT)_app.erl
	$(eval n := $(PROJECT)_sup)
	@printf "%s\n" $(tpl_supervisor) > src/$(PROJECT)_sup.erl

bootstrap-lib:
ifneq ($(wildcard src/),)
	$(error Error: src/ directory already exists)
endif
	@printf "%s\n" $(bs_Makefile) > Makefile
	@mkdir src/
	@printf "%s\n" $(bs_appsrc_lib) > src/$(PROJECT).app.src

bootstrap-rel:
ifneq ($(wildcard relx.config),)
	$(error Error: relx.config already exists)
endif
ifneq ($(wildcard rel/),)
	$(error Error: rel/ directory already exists)
endif
	@printf "%s\n" $(bs_relx_config) > relx.config
	@mkdir rel/
	@printf "%s\n" $(bs_sys_config) > rel/sys.config
	@printf "%s\n" $(bs_vm_args) > rel/vm.args

new:
ifeq ($(wildcard src/),)
	$(error Error: src/ directory does not exist)
endif
ifndef t
	$(error Usage: make new t=TEMPLATE n=NAME)
endif
ifndef tpl_$(t)
	$(error Unknown template)
endif
ifndef n
	$(error Usage: make new t=TEMPLATE n=NAME)
endif
	@printf "%s\n" $(tpl_$(t)) > src/$(n).erl

list-templates:
	@echo Available templates: $(sort $(patsubst tpl_%,%,$(filter tpl_%,$(.VARIABLES))))

# Copyright (c) 2014, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: clean-c_src distclean-c_src-env
# todo

# Configuration.

C_SRC_DIR = $(CURDIR)/c_src
C_SRC_ENV ?= $(C_SRC_DIR)/env.mk
C_SRC_OUTPUT ?= $(CURDIR)/priv/$(PROJECT).so

# System type and C compiler/flags.

UNAME_SYS := $(shell uname -s)
ifeq ($(UNAME_SYS), Darwin)
	CC ?= cc
	CFLAGS ?= -O3 -std=c99 -arch x86_64 -finline-functions -Wall -Wmissing-prototypes
	CXXFLAGS ?= -O3 -arch x86_64 -finline-functions -Wall
	LDFLAGS ?= -arch x86_64 -flat_namespace -undefined suppress
else ifeq ($(UNAME_SYS), FreeBSD)
	CC ?= cc
	CFLAGS ?= -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes
	CXXFLAGS ?= -O3 -finline-functions -Wall
else ifeq ($(UNAME_SYS), Linux)
	CC ?= gcc
	CFLAGS ?= -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes
	CXXFLAGS ?= -O3 -finline-functions -Wall
endif

CFLAGS += -fPIC -I $(ERTS_INCLUDE_DIR) -I $(ERL_INTERFACE_INCLUDE_DIR)
CXXFLAGS += -fPIC -I $(ERTS_INCLUDE_DIR) -I $(ERL_INTERFACE_INCLUDE_DIR)

LDLIBS += -L $(ERL_INTERFACE_LIB_DIR) -lerl_interface -lei
LDFLAGS += -shared

# Verbosity.

c_verbose_0 = @echo " C     " $(?F);
c_verbose = $(c_verbose_$(V))

cpp_verbose_0 = @echo " CPP   " $(?F);
cpp_verbose = $(cpp_verbose_$(V))

link_verbose_0 = @echo " LD    " $(@F);
link_verbose = $(link_verbose_$(V))

# Targets.

ifeq ($(wildcard $(C_SRC_DIR)),)
else ifneq ($(wildcard $(C_SRC_DIR)/Makefile),)
app::
	$(MAKE) -C $(C_SRC_DIR)

clean::
	$(MAKE) -C $(C_SRC_DIR) clean

else
SOURCES := $(shell find $(C_SRC_DIR) -type f \( -name "*.c" -o -name "*.C" -o -name "*.cc" -o -name "*.cpp" \))
OBJECTS = $(addsuffix .o, $(basename $(SOURCES)))

COMPILE_C = $(c_verbose) $(CC) $(CFLAGS) $(CPPFLAGS) -c
COMPILE_CPP = $(cpp_verbose) $(CXX) $(CXXFLAGS) $(CPPFLAGS) -c

app:: $(C_SRC_ENV) $(C_SRC_OUTPUT)

$(C_SRC_OUTPUT): $(OBJECTS)
	@mkdir -p priv/
	$(link_verbose) $(CC) $(OBJECTS) $(LDFLAGS) $(LDLIBS) -o $(C_SRC_OUTPUT)

%.o: %.c
	$(COMPILE_C) $(OUTPUT_OPTION) $<

%.o: %.cc
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

%.o: %.C
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

%.o: %.cpp
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

$(C_SRC_ENV):
	@erl -noshell -noinput -eval "file:write_file(\"$(C_SRC_ENV)\", \
		io_lib:format( \
			\"ERTS_INCLUDE_DIR ?= ~s/erts-~s/include/~n\" \
			\"ERL_INTERFACE_INCLUDE_DIR ?= ~s~n\" \
			\"ERL_INTERFACE_LIB_DIR ?= ~s~n\", \
			[code:root_dir(), erlang:system_info(version), \
			code:lib_dir(erl_interface, include), \
			code:lib_dir(erl_interface, lib)])), \
		erlang:halt()."

clean:: clean-c_src

clean-c_src:
	$(gen_verbose) rm -f $(C_SRC_OUTPUT) $(OBJECTS)

distclean:: distclean-c_src-env

distclean-c_src-env:
	$(gen_verbose) rm -f $(C_SRC_ENV)

-include $(C_SRC_ENV)
endif

# Copyright (c) 2013-2014, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: build-ct-deps build-ct-suites tests-ct clean-ct distclean-ct

# Configuration.

CT_OPTS ?=
ifneq ($(wildcard test/),)
	CT_SUITES ?= $(sort $(subst _SUITE.erl,,$(shell find test -type f -name \*_SUITE.erl -exec basename {} \;)))
else
	CT_SUITES ?=
endif

TEST_ERLC_OPTS ?= +debug_info +warn_export_vars +warn_shadow_vars +warn_obsolete_guard
TEST_ERLC_OPTS += -DTEST=1 -DEXTRA=1 +'{parse_transform, eunit_autoexport}'

# Core targets.

tests:: tests-ct

clean:: clean-ct

distclean:: distclean-ct

help::
	@printf "%s\n" "" \
		"All your common_test suites have their associated targets." \
		"A suite named http_SUITE can be ran using the ct-http target."

# Plugin-specific targets.

ALL_TEST_DEPS_DIRS = $(addprefix $(DEPS_DIR)/,$(TEST_DEPS))

CT_RUN = ct_run \
	-no_auto_compile \
	-noshell \
	-pa $(realpath ebin) $(DEPS_DIR)/*/ebin \
	-dir test \
	-logdir logs

$(foreach dep,$(TEST_DEPS),$(eval $(call dep_target,$(dep))))

build-ct-deps: $(ALL_TEST_DEPS_DIRS)
	@for dep in $(ALL_TEST_DEPS_DIRS) ; do $(MAKE) -C $$dep; done

build-ct-suites: build-ct-deps
	$(gen_verbose) erlc -v $(TEST_ERLC_OPTS) -I include/ -o test/ \
		$(wildcard test/*.erl test/*/*.erl) -pa ebin/

tests-ct: ERLC_OPTS = $(TEST_ERLC_OPTS)
tests-ct: clean deps app build-ct-suites
	@if [ -d "test" ] ; \
	then \
		mkdir -p logs/ ; \
		$(CT_RUN) -suite $(addsuffix _SUITE,$(CT_SUITES)) $(CT_OPTS) ; \
	fi
	$(gen_verbose) rm -f test/*.beam

define ct_suite_target
ct-$(1): ERLC_OPTS = $(TEST_ERLC_OPTS)
ct-$(1): clean deps app build-ct-suites
	@if [ -d "test" ] ; \
	then \
		mkdir -p logs/ ; \
		$(CT_RUN) -suite $(addsuffix _SUITE,$(1)) $(CT_OPTS) ; \
	fi
	$(gen_verbose) rm -f test/*.beam
endef

$(foreach test,$(CT_SUITES),$(eval $(call ct_suite_target,$(test))))

clean-ct:
	$(gen_verbose) rm -rf test/*.beam

distclean-ct:
	$(gen_verbose) rm -rf logs/

# Copyright (c) 2013-2014, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: plt distclean-plt dialyze

# Configuration.

DIALYZER_PLT ?= $(CURDIR)/.$(PROJECT).plt
export DIALYZER_PLT

PLT_APPS ?=
DIALYZER_DIRS ?= --src -r src
DIALYZER_OPTS ?= -Werror_handling -Wrace_conditions \
	-Wunmatched_returns # -Wunderspecs

# Core targets.

distclean:: distclean-plt

help::
	@printf "%s\n" "" \
		"Dialyzer targets:" \
		"  plt         Build a PLT file for this project" \
		"  dialyze     Analyze the project using Dialyzer"

# Plugin-specific targets.

$(DIALYZER_PLT): deps app
	@dialyzer --build_plt --apps erts kernel stdlib $(PLT_APPS) $(ALL_DEPS_DIRS)

plt: $(DIALYZER_PLT)

distclean-plt:
	$(gen_verbose) rm -f $(DIALYZER_PLT)

ifneq ($(wildcard $(DIALYZER_PLT)),)
dialyze:
else
dialyze: $(DIALYZER_PLT)
endif
	@dialyzer --no_native $(DIALYZER_DIRS) $(DIALYZER_OPTS)

# Copyright (c) 2013-2014, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: distclean-edoc

# Configuration.

EDOC_OPTS ?=

# Core targets.

docs:: distclean-edoc
	$(gen_verbose) erl -noshell \
		-eval 'edoc:application($(PROJECT), ".", [$(EDOC_OPTS)]), init:stop().'

distclean:: distclean-edoc

# Plugin-specific targets.

distclean-edoc:
	$(gen_verbose) rm -f doc/*.css doc/*.html doc/*.png doc/edoc-info

# Copyright (c) 2014, Juan Facorro <juan@inaka.net>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: elvis distclean-elvis

# Configuration.

ELVIS_CONFIG ?= $(CURDIR)/elvis.config

ELVIS ?= $(CURDIR)/elvis
export ELVIS

ELVIS_URL ?= https://github.com/inaka/elvis/releases/download/0.2.3/elvis
ELVIS_CONFIG_URL ?= https://github.com/inaka/elvis/releases/download/0.2.3/elvis.config
ELVIS_OPTS ?=

# Core targets.

help::
	@printf "%s\n" "" \
		"Elvis targets:" \
		"  elvis       Run Elvis using the local elvis.config or download the default otherwise"

ifneq ($(wildcard $(ELVIS_CONFIG)),)
rel:: distclean-elvis
endif

distclean:: distclean-elvis

# Plugin-specific targets.

$(ELVIS):
	@$(call core_http_get,$(ELVIS_CONFIG),$(ELVIS_CONFIG_URL))
	@$(call core_http_get,$(ELVIS),$(ELVIS_URL))
	@chmod +x $(ELVIS)

elvis: $(ELVIS)
	@$(ELVIS) rock -c $(ELVIS_CONFIG) $(ELVIS_OPTS)

distclean-elvis:
	$(gen_verbose) rm -rf $(ELVIS)

# Copyright (c) 2013-2014, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

# Verbosity.

dtl_verbose_0 = @echo " DTL   " $(filter %.dtl,$(?F));
dtl_verbose = $(dtl_verbose_$(V))

# Core targets.

define compile_erlydtl
	$(dtl_verbose) erl -noshell -pa ebin/ $(DEPS_DIR)/erlydtl/ebin/ -eval ' \
		Compile = fun(F) -> \
			Module = list_to_atom( \
				string:to_lower(filename:basename(F, ".dtl")) ++ "_dtl"), \
			erlydtl:compile(F, Module, [{out_dir, "ebin/"}]) \
		end, \
		_ = [Compile(F) || F <- string:tokens("$(1)", " ")], \
		init:stop()'
endef

ifneq ($(wildcard src/),)
ebin/$(PROJECT).app:: $(shell find templates -type f -name \*.dtl 2>/dev/null)
	$(if $(strip $?),$(call compile_erlydtl,$?))
endif

# Copyright (c) 2014 Dave Cottlehuber <dch@skunkwerks.at>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: distclean-escript escript

# Configuration.

ESCRIPT_NAME ?= $(PROJECT)
ESCRIPT_COMMENT ?= This is an -*- erlang -*- file

ESCRIPT_BEAMS ?= "ebin/*", "deps/*/ebin/*"
ESCRIPT_SYS_CONFIG ?= "rel/sys.config"
ESCRIPT_EMU_ARGS ?= -pa . \
	-noshell -noinput  \
	-sasl errlog_type error \
	-escript main $(ESCRIPT_NAME)
ESCRIPT_SHEBANG ?= /usr/bin/env escript
ESCRIPT_STATIC ?= "deps/*/priv/**", "priv/**"

# Core targets.

distclean:: distclean-escript

help::
	@printf "%s\n" "" \
		"Escript targets:" \
		"  escript     Build an executable escript archive" \

# Plugin-specific targets.

# Based on https://github.com/synrc/mad/blob/master/src/mad_bundle.erl
# Copyright (c) 2013 Maxim Sokhatsky, Synrc Research Center
# Modified MIT License, https://github.com/synrc/mad/blob/master/LICENSE :
# Software may only be used for the great good and the true happiness of all
# sentient beings.
define ESCRIPT_RAW
'Read = fun(F) -> {ok, B} = file:read_file(filename:absname(F)), B end,'\
'Files = fun(L) -> A = lists:concat([filelib:wildcard(X)||X<- L ]),'\
'  [F || F <- A, not filelib:is_dir(F) ] end,'\
'Squash = fun(L) -> [{filename:basename(F), Read(F) } || F <- L ] end,'\
'Zip = fun(A, L) -> {ok,{_,Z}} = zip:create(A, L, [{compress,all},memory]), Z end,'\
'Ez = fun(Escript) ->'\
'  Static = Files([$(ESCRIPT_STATIC)]),'\
'  Beams = Squash(Files([$(ESCRIPT_BEAMS), $(ESCRIPT_SYS_CONFIG)])),'\
'  Archive = Beams ++ [{ "static.gz", Zip("static.gz", Static)}],'\
'  escript:create(Escript, [ $(ESCRIPT_OPTIONS)'\
'    {archive, Archive, [memory]},'\
'    {shebang, "$(ESCRIPT_SHEBANG)"},'\
'    {comment, "$(ESCRIPT_COMMENT)"},'\
'    {emu_args, " $(ESCRIPT_EMU_ARGS)"}'\
'  ]),'\
'  file:change_mode(Escript, 8#755)'\
'end,'\
'Ez("$(ESCRIPT_NAME)").'
endef
ESCRIPT_COMMAND = $(subst ' ',,$(ESCRIPT_RAW))

escript:: distclean-escript deps app
	$(gen_verbose) erl -noshell -eval $(ESCRIPT_COMMAND) -s init stop

distclean-escript:
	$(gen_verbose) rm -f $(ESCRIPT_NAME)

# Copyright (c) 2013-2014, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: relx-rel distclean-relx-rel distclean-relx

# Configuration.

RELX_CONFIG ?= $(CURDIR)/relx.config

RELX ?= $(CURDIR)/relx
export RELX

RELX_URL ?= https://github.com/erlware/relx/releases/download/v1.0.2/relx
RELX_OPTS ?=
RELX_OUTPUT_DIR ?= _rel

ifeq ($(firstword $(RELX_OPTS)),-o)
	RELX_OUTPUT_DIR = $(word 2,$(RELX_OPTS))
else
	RELX_OPTS += -o $(RELX_OUTPUT_DIR)
endif

# Core targets.

ifneq ($(wildcard $(RELX_CONFIG)),)
rel:: distclean-relx-rel relx-rel
endif

distclean:: distclean-relx-rel distclean-relx

# Plugin-specific targets.

define relx_fetch
	$(call core_http_get,$(RELX),$(RELX_URL))
	chmod +x $(RELX)
endef

$(RELX):
	@$(call relx_fetch)

relx-rel: $(RELX)
	@$(RELX) -c $(RELX_CONFIG) $(RELX_OPTS)

distclean-relx-rel:
	$(gen_verbose) rm -rf $(RELX_OUTPUT_DIR)

distclean-relx:
	$(gen_verbose) rm -rf $(RELX)

# Copyright (c) 2014, M Robert Martin <rob@version2beta.com>
# This file is contributed to erlang.mk and subject to the terms of the ISC License.

.PHONY: shell

# Configuration.

SHELL_PATH ?= -pa ../$(PROJECT)/ebin $(DEPS_DIR)/*/ebin
SHELL_OPTS ?=

ALL_SHELL_DEPS_DIRS = $(addprefix $(DEPS_DIR)/,$(SHELL_DEPS))

# Core targets

help::
	@printf "%s\n" "" \
		"Shell targets:" \
		"  shell              Run an erlang shell with SHELL_OPTS or reasonable default"

# Plugin-specific targets.

$(foreach dep,$(SHELL_DEPS),$(eval $(call dep_target,$(dep))))

build-shell-deps: $(ALL_SHELL_DEPS_DIRS)
	@for dep in $(ALL_SHELL_DEPS_DIRS) ; do $(MAKE) -C $$dep ; done

shell: build-shell-deps
	$(gen_verbose) erl $(SHELL_PATH) $(SHELL_OPTS)
