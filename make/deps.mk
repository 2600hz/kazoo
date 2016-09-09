DEPS = lager eiconv gen_smtp amqp_client cowboy jesse jiffy certifi couchbeam wsock zucchini \
       erlsom erlydtl exml escalus folsom detergent erlang_localtime \
       nklib gproc poolboy syslog lager_syslog eflame hep ecsv reloader \
       proper recon getopt fs_sync eunit inet_cidr

BUILD_DEPS = parse_trans

dep_escalus = hex 2.6.4
dep_exml = git https://github.com/paulgray/exml 2.2.1
dep_eiconv = git https://github.com/zotonic/eiconv
dep_certifi = hex 0.3.0
dep_amqp_client_commit = rabbitmq_v3_6_0
dep_eflame = git https://github.com/slfritchie/eflame 7b0bb1a7e8c8482a59421a3a50ae69d49af59d52
dep_detergent = git https://github.com/pap/detergent e86dfeded3e4f9f3f9278c6a1aea802079d38b54
dep_jiffy = hex 0.14.7
dep_nklib = git https://github.com/NetComposer/nklib

dep_couchbeam = git https://github.com/lazedo/couchbeam bde6256afc1e9048c53db9b5fef4f8b1d83ebc7c
###dep_couchbeam = git https://github.com/benoitc/couchbeam 1.3.1
### waiting for pull request
### https://github.com/benoitc/couchbeam/pull/149
### after that one is merged, another will follow

##dep_jesse = git https://github.com/for-GET/jesse 1.4.0
## pull request pending
## https://github.com/for-GET/jesse/pull/26
## other to come
## there are some commits after 1.4.0 that will go into 1.5.0
## that break a loot of stuff. please be careful if updating to 1.5.0
## from for-GET/jesse 
dep_jesse = git https://github.com/lazedo/jesse 1.4.1

dep_lager = git https://github.com/basho/lager 3.2.1
dep_fs_sync = git https://github.com/jamhed/fs_sync
dep_inet_cidr = git https://github.com/benoitc/inet_cidr.git
### PR opened upstream ###
dep_erlang_localtime = git https://github.com/lazedo/erlang_localtime 0bb26016380cd7df5d30aa0ef284ae252b5bae31

### need to update upstream ###
dep_hep = git https://github.com/lazedo/hep 1.5.4
dep_ecsv = git https://github.com/lazedo/ecsv ecsv-1

### for scripts/dev-start-*.sh
dep_reloader = git https://github.com/oinksoft/reloader

### build
dep_parse_trans = git https://github.com/lazedo/parse_trans
