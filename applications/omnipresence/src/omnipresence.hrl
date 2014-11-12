-ifndef(OMNIPRESENCE_HRL).

%% Typical includes needed
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-define(APP_NAME, <<"omnipresence">>).
-define(APP_VERSION, <<"1.0.0">>).

-define(CONFIG_CAT, <<"omnipresence">>).
-define(CACHE_NAME, 'omnipresence_cache').

-define(PRESENCE_HANGUP, <<"terminated">>).
-define(PRESENCE_RINGING, <<"early">>).
-define(PRESENCE_ANSWERED, <<"confirmed">>).

-define(BLF_EVENT, <<"dialog">>).
-define(DIALOG_EVENT, <<"dialog">>).
-define(MWI_EVENT, <<"message-summary">>).
-define(PRESENCE_EVENT, <<"presence">>).

-define(FAKE_CALLID(C), wh_util:to_hex_binary(crypto:md5(C))).

-record(omnip_subscription, {
          user                                  :: api_binary() | '_' %% user@realm.com
          ,from                                 :: api_binary() | <<>> | '_' %% user@realm.com
          ,stalker                              :: api_binary() | '_' % amqp queue to publish updates to
          ,expires = 0                          :: non_neg_integer() | '_' | '$2'
          ,timestamp = wh_util:current_tstamp() :: non_neg_integer() | '_' | '$1'
          ,protocol = <<"sip">>                 :: ne_binary() | '_' % protocol
          ,username                             :: api_binary() | '_'
          ,realm                                :: api_binary() | '_'
          ,normalized_user                      :: api_binary() | '_' | '$1'
          ,normalized_from                      :: api_binary() | '_' | '$1'
          ,event                                :: api_binary() | '_'
          ,contact                              :: api_binary() | '_'
          ,call_id                              :: api_binary() | '_'
          ,subscription_id                      :: api_binary() | '_'
          ,proxy_route                          :: api_binary() | '_'
          ,version = 1                          :: non_neg_integer() | '_'
         }).

-type subscription() :: #omnip_subscription{}.
-type subscriptions() :: [subscription(),...] | [].

-record(channel, {call_id     :: api_binary()
                  ,direction  :: api_binary()
                  ,state      :: api_binary()
                  ,to         :: api_binary()
                 }).

-type channel() :: #channel{}.
-type channels() :: [channel(),...] | [].

-define(OMNIPRESENCE_HRL, 'true').
-endif.
