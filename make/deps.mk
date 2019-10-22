DEPS = amqp_client \
	apns \
	certifi \
	chatterbox \
	couchbeam \
	cowboy \
	detergent \
	ecsv \
	eflame \
	eiconv \
	erlang_localtime \
	erlazure \
	erlcloud \
	erlsom \
	erlydtl \
	escalus \
	exml \
	fcm \
	folsom \
	fs_event \
	fs_sync \
	gen_smtp \
	getopt \
	gproc \
	hep \
	horse \
	inet_cidr \
	jesse \
	jiffy \
	lager \
	lager_syslog \
	meck \
	nklib \
	plists \
	poolboy \
	proper \
	recon \
	reloader \
	syslog \
	trie \
	wsock \
	zucchini

BUILD_DEPS = parse_trans
IGNORE_DEPS = hamcrest

ifeq ($(CIRCLECI),true)
    DEPS += coveralls
    dep_coveralls = git https://github.com/markusn/coveralls-erl 1.4.0
    DEPS += proper
endif

dep_amqp_client = hex 3.7.8
dep_apns = git https://github.com/2600hz/erlang-apns4erl.git aba1fa96a4abbbb2c1628ad5d604f482aad4d12f # latest commit SHA to 2600hz branch
dep_certifi = hex 0.3.0
dep_chatterbox = hex 0.7.0
dep_cowboy = git https://github.com/ninenines/cowboy 2.2.0
dep_detergent = git https://github.com/pap/detergent e86dfeded3e4f9f3f9278c6a1aea802079d38b54
dep_eflame = git https://github.com/slfritchie/eflame 7b0bb1a7e8c8482a59421a3a50ae69d49af59d52
dep_eiconv = git https://github.com/zotonic/eiconv
dep_escalus = git https://github.com/esl/escalus 0de0463c345a1ade6fccfb9aadad719b58a1cef5
dep_exml = git https://github.com/paulgray/exml 2.2.1
dep_jiffy = git https://github.com/lazedo/jiffy utf8  ## utf8 decode
dep_meck = git https://github.com/eproxus/meck
dep_nklib = git https://github.com/NetComposer/nklib v0.4.1
dep_plists = hex 1.0.0

dep_erlcloud = git https://github.com/erlcloud/erlcloud 3.2.4

dep_erlazure = git https://github.com/lazedo/erlazure.git add-start-link

dep_couchbeam = git https://github.com/2600hz/erlang-couchbeam 28fce6c340de83f4792d45224c29ec729b8e264d # latest commit SHA to 2600hz branch
###dep_couchbeam = git https://github.com/benoitc/couchbeam 1.4.1
### waiting for pull requests
### https://github.com/benoitc/couchbeam/pull/158
### https://github.com/benoitc/couchbeam/pull/164
### https://github.com/benoitc/couchbeam/pull/165
### https://github.com/benoitc/couchbeam/pull/166
### https://github.com/benoitc/couchbeam/pull/174

dep_jesse = git https://github.com/2600hz/jesse 1.5-rc13
##dep_jesse = git https://github.com/for-GET/jesse 1.5.0-rc2

dep_lager = git https://github.com/erlang-lager/lager 3.5.2
dep_trie = git https://github.com/okeuday/trie v1.5.4
dep_fs_event = git https://github.com/jamhed/fs_event 783400da08c2b55c295dbec81df0d926960c0346
dep_fs_sync = git https://github.com/jamhed/fs_sync 2cf85cf5861221128f020c453604d509fd37cd53
dep_inet_cidr = git https://github.com/icehess/inet_cidr.git
### PR opened upstream ###
dep_erlang_localtime = git https://github.com/lazedo/erlang_localtime 0bb26016380cd7df5d30aa0ef284ae252b5bae31

### need to update upstream ###
dep_hep = git https://github.com/lazedo/hep 1.5.4
dep_ecsv = git https://github.com/lazedo/ecsv ecsv-1

### for scripts/dev-start-*.sh
dep_reloader = git https://github.com/lazedo/reloader v0.1

### build
dep_parse_trans = git https://github.com/lazedo/parse_trans

dep_horse = git https://github.com/ninenines/horse

dep_proper = git https://github.com/manopapad/proper v1.2

dep_syslog = git https://github.com/2600hz/erlang-syslog bbad537a1cb5e4f37e672d2e2665659e850662d0

dep_fcm = git https://github.com/softwarejoint/fcm-erlang.git b2f68a4c6f0f59475597a35e2dc9be13d9ba2910

dep_gen_smtp = git https://github.com/2600hz/erlang-gen_smtp f82a135bf5ce6dc8ca29bd4e30cbdc98cd089ee2
## pinning gen_smtp because upstream made some breaking changes (using maps in some options)
