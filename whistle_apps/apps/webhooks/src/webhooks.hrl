-ifndef(WEBHOOKS_HRL).

-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").

-define(APP_NAME, <<"webhooks">>).
-define(APP_VERSION, <<"0.1.0">>).

-type http_methods() :: 'get' | 'post' | 'put'.

-type hook_types() :: 'authn' | 'authz' | 'route'.
-define(HOOKS_SUPPORTED, [authn, authz, route]).

-type api_call_errors() :: 'non_existing' | 'try_again' | 'undefined' | term().

-define(DEFAULT_REQ_HEADERS, [{"Content-Type", "application/json"}
                              ,{"Accept", "application/json"}
                             ]).

-define(DEFAULT_OPTS, [{response_format, binary}]).

-define(WEBHOOKS_HRL, true).
-endif.
