-ifndef(BT_HRL).

-include_lib("braintree/include/braintree.hrl").

-define(APP_NAME, <<"braintree">>).
-define(APP_VERSION, <<"4.0.0">>).
-define(CONFIG_CAT, ?APP_NAME).

-define(BT_DEBUG, kapps_config:get_is_true(?CONFIG_CAT, <<"debug">>, 'false')).

-define(BT_HRL, 'true').
-endif.
