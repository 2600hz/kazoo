## 3rd party dependencies
DEPS ?= amqp_client \
	amqp_dist \
	couchbeam \
	cowboy \
	eflame \
	eiconv \
	erlang_localtime \
	erlazure \
	erlcloud \
	erlsom \
	erlydtl \
	gen_smtp \
	getopt \
	gproc \
	inet_cidr \
	jesse \
	jiffy \
	lager \
	lager_syslog \
	meck \
	meta \
	nklib \
	plists \
	poolboy \
	proper \
	ra \
	ranch \
	recon \
	reloader \
	syslog \
	yamerl \
	zucchini

# BUILD_DEPS = parse_trans
IGNORE_DEPS = hamcrest

ifeq ($(CIRCLECI),true)
    DEPS += coveralls
    dep_coveralls = git https://github.com/markusn/coveralls-erl 1.4.0
    DEPS += proper
endif

dep_amqp_client = git https://github.com/2600hz/erlang-amqp_client.git v3.7.14

dep_amqp_dist = git https://github.com/2600hz/erlang-amqp_dist.git faa54e490f3083a323d3a95c53bb9493644478dd

# dep_certifi = hex 0.3.0
# Used by hackney, let it pull in certifi

# dep_chatterbox = hex 0.7.0
# used by apns4erl

dep_couchbeam = git https://github.com/2600hz/erlang-couchbeam 86d946646ad9dee1dee305b15d5c59e4c8e60cc2 # latest commit SHA to 2600hz-kazoo5 branch
# adds _show querying

### https://github.com/benoitc/couchbeam/pull/158 - _list functions fix
### https://github.com/benoitc/couchbeam/pull/164 - allow 202 in put_attachment
### https://github.com/benoitc/couchbeam/pull/165 - fetch couchdb config
### https://github.com/benoitc/couchbeam/pull/166 - no crash when getting body
### https://github.com/benoitc/couchbeam/pull/174 - view_cleanup content-type header

dep_cowboy = git https://github.com/2600hz/erlang-cowboy 2.6.3

dep_eflame = git https://github.com/slfritchie/eflame 7b0bb1a7e8c8482a59421a3a50ae69d49af59d52
# used by kz_tracers

dep_eiconv = git https://github.com/zotonic/eiconv 1.0.0
# used by gen_smtp

dep_erlang_localtime = git https://github.com/2600hz/erlang-localtime
# used by kazoo_documents, teletype, notify, crossbar, callflow

dep_erlazure = git https://github.com/2600hz/erlang-erlazure.git 88e0417251983ab4d8a2a2606c732906eecd5007
# used by kazoo_attachments, merged in lazedo/erlazure add-start-link changes

dep_erlcloud = git https://github.com/2600hz/erlang-erlcloud 3.2.7
# used by kazoo_attachments and a crossbar test (cb_storage_tests)

## Code reloaders for dev VMs, uncomment if desired
# dep_fs_event = git https://github.com/jamhed/fs_event 783400da08c2b55c295dbec81df0d926960c0346
# dep_fs_sync = git https://github.com/jamhed/fs_sync 2cf85cf5861221128f020c453604d509fd37cd53

dep_gen_smtp = git https://github.com/2600hz/erlang-gen_smtp 3d294c57153787027bea4011eeba268fee5d7165
# used by teletype, notify, and fax
# SHA fixes spec for gen_smto_client:send/3

dep_getopt = git https://github.com/2600hz/erlang-getopt v1.0.1
# used in some scripts/ and sup

dep_gproc = git https://github.com/2600hz/erlang-gproc 0.8.0
# used by kazoo_events, webseq, konami, acdc, ecallmgr, callflow

# dep_horse = git https://github.com/ninenines/horse 4dc81d47c3116b38af673481402f34ce03f8936b
# used by kazoo_stdlib in some test modules
# uncomment if you want to do simple perf testing of code

dep_inet_cidr = git https://github.com/2600hz/erlang-inet_cidr.git 1.0.2
# used by kz_network_utils

dep_jesse = git https://github.com/2600hz/jesse 1.5-rc13
# used by kazoo_schemas primarily

dep_jiffy = git https://github.com/2600hz/jiffy master  ## utf8 decode
# includes changes from lazedo/utf8
# used by kz_json, nklib, jesse, lager, maybe couchbeam if compiled

dep_lager = git https://github.com/2600hz/erlang-lager 3.6.10
# used everywhere

dep_lager_syslog = git https://github.com/2600hz/erlang-lager_syslog 3.0.3

dep_meck = git https://github.com/2600hz/erlang-meck 0.8.13
# used in tests for kazoo_voicemail, crossbar, teletype, and other deps

dep_meta = git https://github.com/2600hz/erlang-meta 0.1.3
# appears unused

dep_nklib = git https://github.com/2600hz/erlang-nklib v0.4.1
# used by kzsip_uri and cb_registrations

# dep_parse_trans = git https://github.com/lazedo/parse_trans
# appears unused

dep_plists = git https://github.com/2600hz/erlang-plists 1.0.0
# used by a handful of core apps

dep_proper = git https://github.com/2600hz/erlang-proper v1.3
# used by kazoo_proper, knm, kazoo_caches, kazoo_bindings, kz_util_tests, kazoo_token_buckets, kazoo_stdlib
# used by apps hotornot and callflow

dep_recon = git https://github.com/2600hz/erlang-recon 2.4.0

dep_ra = git https://github.com/2600hz/erlang-ra.git v1.0.4

dep_ranch = git https://github.com/2600hz/erlang-ranch 1.7.1

dep_reloader = git https://github.com/2600hz/erlang-reloader de1e6c74204b61ccf3b3652f05c6a7dec9e8257d
# Development-related for reloading beam files
# see rel/dev.vm.args and fixture_shell in make/kz.mk
# commit adds makefile for compile/clean

dep_syslog = git https://github.com/2600hz/erlang-syslog bbad537a1cb5e4f37e672d2e2665659e850662d0

# dep_wsock = git https://github.com/madtrick/wsock 1.1.7
# appears unused

dep_yamerl = git https://github.com/2600hz/erlang-yamerl v0.7.0
# used by kazoo_ast to create OpenAPI 3

dep_zucchini = git https://github.com/2600hz/erlang-zucchini 0.1.0
# INI file parser
# used by kazoo_config_init
