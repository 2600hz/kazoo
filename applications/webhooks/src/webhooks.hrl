-ifndef(WEBHOOKS_HRL).
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").
-include_lib("whistle_apps/include/wh_hooks.hrl").

-define(APP_NAME, <<"webhooks">>).
-define(APP_VERSION, <<"3.20.0">>).

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

-define(CACHE_NAME, 'webhooks_cache').

-define(ATTEMPT_EXPIRY_KEY, <<"attempt_failure_expiry_ms">>).
-define(FAILURE_COUNT_KEY, <<"attempt_failure_count">>).

-define(FAILURE_CACHE_KEY(AccountId, HookId, Timestamp)
        ,{'failure', AccountId, HookId, Timestamp}
       ).

-define(WEBHOOKS_HRL, 'true').
-endif.
