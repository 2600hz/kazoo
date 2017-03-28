-ifndef(WEBHOOKS_HRL).
-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").
-include_lib("kazoo/include/kz_databases.hrl").
-include_lib("kazoo_apps/include/kz_hooks.hrl").

-define(APP_NAME, <<"webhooks">>).
-define(APP_VERSION, <<"4.0.0">>).

-type http_verb() :: 'get' | 'post'.
-type hook_retries() :: 1..5.

-record(webhook, {id :: api_ne_binary() | '_'
                 ,uri :: api_ne_binary() | '_'
                 ,http_verb = 'get' :: http_verb() | '_'
                 ,hook_event :: api_ne_binary() | '_' | '$1' | '$2'
                 ,hook_id :: api_ne_binary() | '_'
                 ,retries = 3 :: hook_retries() | '_'
                 ,account_id :: api_ne_binary() | '_' | '$1'
                 ,include_subaccounts = 'false' :: boolean() | '_' | '$3'
                 ,custom_data :: api_object() | '_'
                 ,modifiers :: api_object() | '_'
                 }).
-type webhook() :: #webhook{}.
-type webhooks() :: [webhook()].

-define(CACHE_NAME, 'webhooks_cache').

-define(ATTEMPT_EXPIRY_KEY, <<"attempt_failure_expiry_ms">>).
-define(FAILURE_COUNT_KEY, <<"attempt_failure_count">>).

-define(FAILURE_CACHE_KEY(AccountId, HookId, Timestamp)
       ,{'failure', AccountId, HookId, Timestamp}
       ).

-define(WEBHOOK_META_LIST, <<"webhooks/webhook_meta_listing">>).

-define(WEBHOOKS_HRL, 'true').
-endif.
