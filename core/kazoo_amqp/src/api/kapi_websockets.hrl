-ifndef(KAPI_WEBSOCKETS_HRL).

-include_lib("kazoo_amqp/include/kz_api_literals.hrl").

-define(MODULE_REQ_ROUTING_KEY, <<"ws.module_req">>).
%% websockets whapp routing keys for responses to clients
-define(KEY_WEBSOCKETS_GET_REQ, <<"websockets.get">>).

-define(KAPI_WEBSOCKETS_HRL, 'true').
-endif.
