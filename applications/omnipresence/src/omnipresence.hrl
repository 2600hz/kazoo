-ifndef(OMNIPRESENCE_HRL).

%% Typical includes needed
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").

-define(APP_NAME, <<"omnipresence">>).
-define(APP_VERSION, <<"4.0.0">>).
-define(CONFIG_CAT, ?APP_NAME).

-define(CACHE_NAME, 'omnipresence_cache').

-define(PRESENCE_HANGUP, <<"terminated">>).
-define(PRESENCE_RINGING, <<"early">>).
-define(PRESENCE_ANSWERED, <<"confirmed">>).

-define(BLF_EVENT, <<"dialog">>).
-define(DIALOG_EVENT, <<"dialog">>).
-define(MWI_EVENT, <<"message-summary">>).
-define(PRESENCE_EVENT, <<"presence">>).
-define(OMNIPRESENCE_EVENT_ALL, <<"all">>).

-define(RINGING_TIME, 24 * ?SECONDS_IN_HOUR).
-define(ANSWERED_TIME, 24 * ?SECONDS_IN_HOUR).
-define(HANGUP_TIME, 10).
-define(OTHER_TIME, 24 * ?SECONDS_IN_HOUR).

-record(omnip_subscription, {user                                 :: kz_term:api_binary() | '_' %% user@realm.com
                            ,from                                 :: kz_term:api_binary() | <<>> | '_' %% user@realm.com
                            ,stalker                              :: kz_term:api_binary() | '_' | '$2' % amqp queue to publish updates to
                            ,expires = 0                          :: non_neg_integer() | '_' | '$2'
                            ,timestamp = kz_time:now_s() :: kz_time:gregorian_seconds() | '_' | '$1'
                            ,username                             :: kz_term:api_binary() | '_'
                            ,realm                                :: kz_term:api_binary() | '_'
                            ,normalized_user                      :: kz_term:api_binary() | '_' | '$1'
                            ,normalized_from                      :: kz_term:api_binary() | '_' | '$1'
                            ,event                                :: kz_term:api_binary() | '_'
                            ,contact                              :: kz_term:api_binary() | '_'
                            ,call_id                              :: kz_term:api_binary() | '_'
                            ,subscription_id                      :: kz_term:api_binary() | '_'
                            ,proxy_route                          :: kz_term:api_binary() | '_'
                            ,version = 1                          :: non_neg_integer() | '_'
                            ,last_sequence = 0                    :: non_neg_integer() | '_'
                            ,last_reply = 0                       :: non_neg_integer() | '_'
                            ,last_body                            :: kz_term:api_binary() | '_'
                            ,user_agent                            :: kz_term:api_binary() | '_'
                            }).

-type subscription() :: #omnip_subscription{}.
-type subscriptions() :: [subscription()].

-record(channel, {call_id    :: kz_term:api_binary()
                 ,direction  :: kz_term:api_binary()
                 ,state      :: kz_term:api_binary()
                 ,to         :: kz_term:api_binary()
                 }).

-type channel() :: #channel{}.
-type channels() :: [channel()].

-define(SUBSCRIPTION_SIP_VERSION, 2).

-define(OMNIPRESENCE_HRL, 'true').
-endif.
