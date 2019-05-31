## 3rd party dependencies
DEPS = amqp_client \
	apns \
	couchbeam \
	cowboy \
	ecsv \
	eflame \
	erlang_localtime \
	erlazure \
	erlcloud \
	erlsom \
	erlydtl \
	escalus \
	exml \
	fcm \
	folsom \
	gen_smtp \
	getopt \
	gproc \
	hep \
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
	ranch \
	recon \
	reloader \
	syslog \
	trie \
	zucchini

# BUILD_DEPS = parse_trans
IGNORE_DEPS = hamcrest

ifeq ($(USER),travis)
    DEPS += coveralls
    dep_coveralls = git https://github.com/markusn/coveralls-erl 1.4.0
    DEPS += proper
endif

dep_amqp_client = hex 3.7.14
dep_apns = git https://github.com/2600hz/erlang-apns4erl.git aba1fa96a4abbbb2c1628ad5d604f482aad4d12f # latest commit SHA to 2600hz branch

# dep_certifi = hex 0.3.0
# Used by hackney, let it pull in certifi

# dep_chatterbox = hex 0.7.0
# used by apns4erl

dep_couchbeam = git https://github.com/2600hz/erlang-couchbeam 28fce6c340de83f4792d45224c29ec729b8e264d # latest commit SHA to 2600hz branch
### https://github.com/benoitc/couchbeam/pull/158 - _list functions fix
### https://github.com/benoitc/couchbeam/pull/164 - allow 202 in put_attachment
### https://github.com/benoitc/couchbeam/pull/165 - fetch couchdb config
### https://github.com/benoitc/couchbeam/pull/166 - no crash when getting body
### https://github.com/benoitc/couchbeam/pull/174 - view_cleanup content-type header

dep_cowboy = git https://github.com/ninenines/cowboy 2.6.3

# dep_detergent = git https://github.com/pap/detergent e86dfeded3e4f9f3f9278c6a1aea802079d38b54
# used by the 'dth' app which is so badly deprecated

dep_ecsv = git https://github.com/2600hz/ecsv 2600hz
# merged lazedo/ecsv-1 and rcouch/master
# looks like cb_rates is only user of this, kz_csv may be workable

dep_eflame = git https://github.com/slfritchie/eflame 7b0bb1a7e8c8482a59421a3a50ae69d49af59d52
# used by kz_tracers

# dep_eiconv = git https://github.com/zotonic/eiconv
# used by gen_smtp

dep_erlang_localtime = git https://github.com/lazedo/erlang_localtime 0bb26016380cd7df5d30aa0ef284ae252b5bae31
# used by kazoo_documents, teletype, notify, crossbar, callflow

dep_erlazure = git https://github.com/lazedo/erlazure.git add-start-link
# used by kazoo_attachments

dep_erlcloud = git https://github.com/erlcloud/erlcloud 3.2.7
# used by kazoo_attachments and a crossbar test (cb_storage_tests)

dep_escalus = git https://github.com/esl/escalus 0de0463c345a1ade6fccfb9aadad719b58a1cef5
# used by fax cloud printer i think?

dep_exml = git https://github.com/paulgray/exml 2.2.1
# used by fax cloud printer

dep_fcm = git https://github.com/softwarejoint/fcm-erlang.git b2f68a4c6f0f59475597a35e2dc9be13d9ba2910
# Firebase cloud messaging
# used by pusher

dep_folsom = git https://github.com/boundary/folsom 0.8.2
# used by hangups

## Code reloaders for dev VMs, uncomment if desired
# dep_fs_event = git https://github.com/jamhed/fs_event 783400da08c2b55c295dbec81df0d926960c0346
# dep_fs_sync = git https://github.com/jamhed/fs_sync 2cf85cf5861221128f020c453604d509fd37cd53

dep_gen_smtp = hex 0.13.0
# used by teletype, notify, and fax

dep_getopt = hex 1.0.1
# used in some scripts/ and sup

dep_gproc = hex 0.8.0
# used by kazoo_events, webseq, konami, acdc, ecallmgr, callflow

dep_hep = git https://github.com/2600hz/hep-erlang a8f62dbc756fd3d4be1e2f0a6cec47a23f6cd18a
# Homer encapsulation protocol
# merged lazedo/hep changes

# dep_horse = git https://github.com/ninenines/horse 4dc81d47c3116b38af673481402f34ce03f8936b
# used by kazoo_stdlib in some test modules
# uncomment if you want to do simple perf testing of code

dep_inet_cidr = git https://github.com/benoitc/inet_cidr.git 1.0.2
# used by kz_network_utils

dep_jesse = git https://github.com/2600hz/jesse 1.5-rc13
# used by kazoo_schemas primarily

dep_jiffy = git https://github.com/2600hz/jiffy master  ## utf8 decode
# includes changes from lazedo/utf8
# used by kz_json, nklib, jesse, lager, maybe couchbeam if compiled

dep_lager = git https://github.com/erlang-lager/lager 3.6.10
# used everywhere

dep_lager_syslog = git https://github.com/erlang-lager/lager_syslog 3.0.3

dep_meck = git https://github.com/eproxus/meck 0.8.13
# used in tests for kazoo_voicemail, crossbar, teletype, and other deps

dep_meta = git https://github.com/efcasado/meta 0.1.3
# appears unused

dep_nklib = git https://github.com/NetComposer/nklib v0.4.1
# used by kzsip_uri and cb_registrations

# dep_parse_trans = git https://github.com/lazedo/parse_trans
# appears unused

dep_plists = hex 1.0.0
# used by a handful of core apps

dep_proper = git https://github.com/proper-testing/proper v1.3
# used by kazoo_proper, knm, kazoo_caches, kazoo_bindings, kz_util_tests, kazoo_token_buckets, kazoo_stdlib
# used by apps hotornot and callflow

dep_recon = hex 2.4.0

dep_ranch = git https://github.com/ninenines/ranch 1.7.1

dep_reloader = hex 0.1.0
# Development-related for reloading beam files
# see rel/dev.vm.args and fixture_shell in make/kz.mk

dep_syslog = git https://github.com/2600hz/erlang-syslog bbad537a1cb5e4f37e672d2e2665659e850662d0

dep_trie = hex 1.7.5
# used by hotornot

# dep_wsock = git https://github.com/madtrick/wsock 1.1.7
# appears unused

dep_zucchini = hex 0.1.0
# INI file parser
# used by kazoo_config_init
