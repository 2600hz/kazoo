-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").

-define(APP_NAME, <<"webhooks">>).
-define(APP_VERSION, <<"0.1.0">>).

-type hook_types() :: 'route' | 'authn' | 'authz'.

-define(DEFAULT_REQ_HEADERS, [{"Content-Type", "application/json"}
                              ,{"Accept", "application/json"}
                             ]).

-define(DEFAULT_OPTS, [{response_format, binary}]).
