-module(kapps_config_migrate_test).

-include_lib("eunit/include/eunit.hrl").

-define(WHAPPS_CONTROLLER
       ,<<"{
  \"_id\": \"whapps_controller\",
   \"_rev\": \"23-37a711250e044cb040d05385b74e5329\",
   \"whistle_apps@some.server.com\": {
       \"whapps\": [
           \"sysconf\",
           \"reorder\",
           \"pivot\",
           \"registrar\",
           \"stepswitch\",
           \"media_mgr\",
           \"callflow\",
           \"cdr\",
           \"jonny5\"
       ],
          \"zone\": \"zone_QA1\",
       \"console_log_level\": \"debug\",
       \"error_log_level\": \"debug\",
       \"syslog_log_level\": \"debug\",
       \"cookie\": \"CERT_BIGCOUCH\"
   },
   \"whistle_apps@other.server.com\": {
       \"whapps\": [
           \"sysconf\",
           \"reorder\",
           \"pivot\",
           \"registrar\",
           \"stepswitch\",
           \"media_mgr\",
           \"callflow\",
           \"cdr\",
           \"jonny5\"
       ],
       \"zone\": \"zone_QA2\",
       \"console_log_level\": \"debug\",
       \"error_log_level\": \"debug\",
       \"syslog_log_level\": \"debug\",
       \"cookie\": \"CERT_BIGCOUCH\"
   },
   \"whistle_apps@different.server.com\": {
       \"whapps\": [
           \"crossbar\"
       ],
       \"zone\": \"zone_QA1\",
       \"console_log_level\": \"debug\",
       \"error_log_level\": \"debug\",
       \"syslog_log_level\": \"debug\",
       \"cookie\": \"CERT_BIGCOUCH\"
   },
   \"whistle_apps@foo.server.com\": {
       \"whapps\": [
           \"crossbar\"
       ],
       \"zone\": \"zone_QA2\",
       \"console_log_level\": \"debug\",
       \"error_log_level\": \"debug\",
       \"syslog_log_level\": \"debug\",
       \"cookie\": \"CERT_BIGCOUCH\"
   },
   \"whistle_apps@bar.server.com\": {
       \"whapps\": [
           \"crossbar\",
           \"callflow\",
           \"fax\",
           \"teletype\",
           \"conference\"
       ],
       \"zone\": \"zone_QA1\",
       \"console_log_level\": \"debug\",
       \"error_log_level\": \"debug\",
       \"syslog_log_level\": \"debug\",
       \"cookie\": \"CERT_BIGCOUCH\"
   },
   \"whistle_apps@baz.server.com\": {
       \"whapps\": [
           \"crossbar\",
           \"callflow\",
           \"fax\",
           \"teletype\",
           \"conference\"
       ],
       \"zone\": \"zone_QA2\",
       \"console_log_level\": \"debug\",
       \"error_log_level\": \"debug\",
       \"syslog_log_level\": \"debug\",
       \"cookie\": \"CERT_BIGCOUCH\"
   },
   \"whistle_apps@fizz.server.com\": {
       \"whapps\": [
           \"crossbar\"
       ],
       \"zone\": \"zone_QA1\",
       \"console_log_level\": \"debug\",
       \"error_log_level\": \"debug\",
       \"syslog_log_level\": \"debug\",
       \"cookie\": \"CERT_BIGCOUCH\"
   },
   \"whistle_apps@buzz\": {
       \"whapps\": [
           \"crossbar\"
       ],
       \"zone\": \"zone_QA1\",
       \"console_log_level\": \"debug\",
       \"error_log_level\": \"debug\",
       \"syslog_log_level\": \"debug\",
       \"cookie\": \"CERT_BIGCOUCH\"
   },
   \"whistle_apps@fizzbuzz.server.com\": {
       \"whapps\": [
           \"sysconf\",
           \"reorder\",
           \"pivot\",
           \"registrar\",
           \"stepswitch\",
           \"media_mgr\",
           \"callflow\",
           \"cdr\",
           \"jonny5\",
           \"callflow\",
           \"fax\",
           \"teletype\",
           \"conference\"
       ],
       \"zone\": \"zone_QA3\",
       \"console_log_level\": \"debug\",
       \"error_log_level\": \"debug\",
       \"syslog_log_level\": \"debug\",
       \"cookie\": \"CERT_BIGCOUCH\"
   },
   \"whistle_apps@hello.server.com\": {
       \"whapps\": [
           \"sysconf\",
           \"reorder\",
           \"pivot\",
           \"registrar\",
           \"stepswitch\",
           \"media_mgr\",
           \"callflow\",
           \"cdr\",
           \"jonny5\",
           \"callflow\",
           \"fax\",
           \"teletype\",
           \"conference\"
       ],
       \"zone\": \"zone_QA4\",
       \"console_log_level\": \"debug\",
       \"error_log_level\": \"debug\",
       \"syslog_log_level\": \"debug\",
       \"cookie\": \"CERT_BIGCOUCH\"
   },
   \"whistle_apps@world.server.com\": {
       \"whapps\": [
           \"crossbar\",
           \"webhooks\"
       ],
       \"zone\": \"zone_QA3\",
       \"console_log_level\": \"debug\",
       \"error_log_level\": \"debug\",
       \"syslog_log_level\": \"debug\",
       \"cookie\": \"CERT_BIGCOUCH\"
   },
   \"whistle_apps@printf.server.com\": {
       \"whapps\": [
           \"crossbar\"
       ],
       \"zone\": \"zone_QA4\",
       \"console_log_level\": \"debug\",
       \"error_log_level\": \"debug\",
       \"syslog_log_level\": \"debug\",
       \"cookie\": \"CERT_BIGCOUCH\"
   },
   \"default\": {
       \"whapps\": [
           \"sysconf\",
           \"reorder\",
           \"pivot\",
           \"registrar\",
           \"stepswitch\",
           \"media_mgr\",
           \"callflow\",
           \"cdr\",
           \"jonny5\"
       ],
       \"console_log_level\": \"info\",
       \"error_log_level\": \"error\",
       \"syslog_log_level\": \"info\",
       \"cookie\": \"CERT_BIGCOUCH\"
   }
}">>).

-define(KAPPS_CONTROLLER
       ,<<"{
  \"_id\": \"kapps_controller\",
   \"kazoo_apps@some.server.com\": {
       \"kapps\": [
           \"sysconf\",
           \"reorder\",
           \"pivot\",
           \"registrar\",
           \"stepswitch\",
           \"media_mgr\",
           \"callflow\",
           \"cdr\",
           \"jonny5\"
       ],
          \"zone\": \"zone_QA1\",
       \"console_log_level\": \"debug\",
       \"error_log_level\": \"debug\",
       \"syslog_log_level\": \"debug\",
       \"cookie\": \"CERT_BIGCOUCH\"
   },
   \"kazoo_apps@other.server.com\": {
       \"kapps\": [
           \"sysconf\",
           \"reorder\",
           \"pivot\",
           \"registrar\",
           \"stepswitch\",
           \"media_mgr\",
           \"callflow\",
           \"cdr\",
           \"jonny5\"
       ],
       \"zone\": \"zone_QA2\",
       \"console_log_level\": \"debug\",
       \"error_log_level\": \"debug\",
       \"syslog_log_level\": \"debug\",
       \"cookie\": \"CERT_BIGCOUCH\"
   },
   \"kazoo_apps@different.server.com\": {
       \"kapps\": [
           \"crossbar\"
       ],
       \"zone\": \"zone_QA1\",
       \"console_log_level\": \"debug\",
       \"error_log_level\": \"debug\",
       \"syslog_log_level\": \"debug\",
       \"cookie\": \"CERT_BIGCOUCH\"
   },
   \"kazoo_apps@foo.server.com\": {
       \"kapps\": [
           \"crossbar\"
       ],
       \"zone\": \"zone_QA2\",
       \"console_log_level\": \"debug\",
       \"error_log_level\": \"debug\",
       \"syslog_log_level\": \"debug\",
       \"cookie\": \"CERT_BIGCOUCH\"
   },
   \"kazoo_apps@bar.server.com\": {
       \"kapps\": [
           \"crossbar\",
           \"callflow\",
           \"fax\",
           \"teletype\",
           \"conference\"
       ],
       \"zone\": \"zone_QA1\",
       \"console_log_level\": \"debug\",
       \"error_log_level\": \"debug\",
       \"syslog_log_level\": \"debug\",
       \"cookie\": \"CERT_BIGCOUCH\"
   },
   \"kazoo_apps@baz.server.com\": {
       \"kapps\": [
           \"crossbar\",
           \"callflow\",
           \"fax\",
           \"teletype\",
           \"conference\"
       ],
       \"zone\": \"zone_QA2\",
       \"console_log_level\": \"debug\",
       \"error_log_level\": \"debug\",
       \"syslog_log_level\": \"debug\",
       \"cookie\": \"CERT_BIGCOUCH\"
   },
   \"kazoo_apps@fizz.server.com\": {
       \"kapps\": [
           \"crossbar\"
       ],
       \"zone\": \"zone_QA1\",
       \"console_log_level\": \"debug\",
       \"error_log_level\": \"debug\",
       \"syslog_log_level\": \"debug\",
       \"cookie\": \"CERT_BIGCOUCH\"
   },
   \"kazoo_apps@buzz\": {
       \"kapps\": [
           \"crossbar\"
       ],
       \"zone\": \"zone_QA1\",
       \"console_log_level\": \"debug\",
       \"error_log_level\": \"debug\",
       \"syslog_log_level\": \"debug\",
       \"cookie\": \"CERT_BIGCOUCH\"
   },
   \"kazoo_apps@fizzbuzz.server.com\": {
       \"kapps\": [
           \"sysconf\",
           \"reorder\",
           \"pivot\",
           \"registrar\",
           \"stepswitch\",
           \"media_mgr\",
           \"callflow\",
           \"cdr\",
           \"jonny5\",
           \"callflow\",
           \"fax\",
           \"teletype\",
           \"conference\"
       ],
       \"zone\": \"zone_QA3\",
       \"console_log_level\": \"debug\",
       \"error_log_level\": \"debug\",
       \"syslog_log_level\": \"debug\",
       \"cookie\": \"CERT_BIGCOUCH\"
   },
   \"kazoo_apps@hello.server.com\": {
       \"kapps\": [
           \"sysconf\",
           \"reorder\",
           \"pivot\",
           \"registrar\",
           \"stepswitch\",
           \"media_mgr\",
           \"callflow\",
           \"cdr\",
           \"jonny5\",
           \"callflow\",
           \"fax\",
           \"teletype\",
           \"conference\"
       ],
       \"zone\": \"zone_QA4\",
       \"console_log_level\": \"debug\",
       \"error_log_level\": \"debug\",
       \"syslog_log_level\": \"debug\",
       \"cookie\": \"CERT_BIGCOUCH\"
   },
   \"kazoo_apps@world.server.com\": {
       \"kapps\": [
           \"crossbar\",
           \"webhooks\"
       ],
       \"zone\": \"zone_QA3\",
       \"console_log_level\": \"debug\",
       \"error_log_level\": \"debug\",
       \"syslog_log_level\": \"debug\",
       \"cookie\": \"CERT_BIGCOUCH\"
   },
   \"kazoo_apps@printf.server.com\": {
       \"kapps\": [
           \"crossbar\"
       ],
       \"zone\": \"zone_QA4\",
       \"console_log_level\": \"debug\",
       \"error_log_level\": \"debug\",
       \"syslog_log_level\": \"debug\",
       \"cookie\": \"CERT_BIGCOUCH\"
   },
   \"default\": {
       \"kapps\": [
           \"sysconf\",
           \"reorder\",
           \"pivot\",
           \"registrar\",
           \"stepswitch\",
           \"media_mgr\",
           \"callflow\",
           \"cdr\",
           \"jonny5\"
       ],
       \"console_log_level\": \"info\",
       \"error_log_level\": \"error\",
       \"syslog_log_level\": \"info\",
       \"cookie\": \"CERT_BIGCOUCH\"
   }
}">>).

migrate_doc_test() ->
    Migrated = kapps_config:migrate_from_doc(kz_json:decode(?WHAPPS_CONTROLLER), kz_json:from_list([{<<"_id">>, <<"kapps_controller">>}])),
    ?assert(kz_json:are_equal(kz_json:decode(?KAPPS_CONTROLLER), Migrated)).
