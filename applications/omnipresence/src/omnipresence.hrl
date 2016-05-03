-ifndef(OMNIPRESENCE_HRL).

%% Typical includes needed
-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").
-include_lib("kazoo/include/kz_databases.hrl").

-define(APP_NAME, <<"omnipresence">>).
-define(APP_VERSION, <<"4.0.0">>).

-define(CONFIG_CAT, <<"omnipresence">>).
-define(CACHE_NAME, 'omnipresence_cache').

-define(PRESENCE_HANGUP, <<"terminated">>).
-define(PRESENCE_RINGING, <<"early">>).
-define(PRESENCE_ANSWERED, <<"confirmed">>).

-define(BLF_EVENT, <<"dialog">>).
-define(DIALOG_EVENT, <<"dialog">>).
-define(MWI_EVENT, <<"message-summary">>).
-define(PRESENCE_EVENT, <<"presence">>).
-define(OMNIPRESENCE_EVENT_ALL, <<"all">>).

-define(FAKE_CALLID(C), kz_util:to_hex_binary(crypto:hash(md5, C))).

-record(omnip_subscription, {
          user                                  :: api(binary()) | '_' %% user@realm.com
          ,from                                 :: api(binary()) | <<>> | '_' %% user@realm.com
          ,stalker                              :: api(binary()) | '_' | '$2' % amqp queue to publish updates to
          ,expires = 0                          :: non_neg_integer() | '_' | '$2'
          ,timestamp = kz_util:current_tstamp() :: gregorian_seconds() | '_' | '$1'
          ,protocol = <<"sip">>                 :: ne_binary() | '_' % protocol
          ,username                             :: api(binary()) | '_'
          ,realm                                :: api(binary()) | '_'
          ,normalized_user                      :: api(binary()) | '_' | '$1'
          ,normalized_from                      :: api(binary()) | '_' | '$1'
          ,event                                :: api(binary()) | '_'
          ,contact                              :: api(binary()) | '_'
          ,call_id                              :: api(binary()) | '_'
          ,subscription_id                      :: api(binary()) | '_'
          ,proxy_route                          :: api(binary()) | '_'
          ,version = 1                          :: non_neg_integer() | '_'
          ,last_sequence = 0                    :: non_neg_integer() | '_'
          ,last_reply = 0                       :: non_neg_integer() | '_'
          ,last_body                            :: api(binary()) | '_'
          ,user_agent                            :: api(binary()) | '_'
         }).

-type subscription() :: #omnip_subscription{}.
-type subscriptions() :: [subscription()].

-record(channel, {call_id     :: api(binary())
                  ,direction  :: api(binary())
                  ,state      :: api(binary())
                  ,to         :: api(binary())
                 }).

-type channel() :: #channel{}.
-type channels() :: [channel()].

-define(SUBSCRIPTION_SIP_VERSION, 2).

-define(OMNIPRESENCE_HRL, 'true').
-endif.
