-ifndef(WEBHOOKS_HRL).
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").
-include_lib("whistle_apps/include/wh_hooks.hrl").

-define(WEBHOOKS_HRL, 'true').

-type http_verb() :: 'get' | 'post'.
-type hook_retries() :: 1..5.

-record(webhook, {
          id :: ne_binary() | '_'
          ,uri :: ne_binary() | '_'
          ,http_verb :: http_verb() | '_'
          ,hook_event :: ne_binary() | '_' | '$2'
          ,hook_id :: ne_binary() | '_'
          ,retries = 3 :: hook_retries() | '_'
          ,account_id :: ne_binary() | '_' | '$1'
          ,custom_data :: wh_json:object() | '_'
         }).
-type webhook() :: #webhook{}.
-type webhooks() :: [webhook(),...] | [].

-record(webhook_event, {
          id :: ne_binary() | '$1' | '_'
          ,received :: pos_integer() | '$1' | '_'
         }).
-type webhook_event() :: #webhook_event{}.

-endif.
