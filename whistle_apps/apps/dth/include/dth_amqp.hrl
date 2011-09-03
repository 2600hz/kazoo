-include_lib("whistle/include/wh_types.hrl").

%% Route used to publish blacklist requests
-define(KEY_DTH_BLACKLIST_REQ, <<"dth.blacklist_req">>).

-define(DTH_BLACKLIST_REQ_HEADERS, []).
-define(OPTIONAL_DTH_BLACKLIST_REQ_HEADERS, []).
-define(DTH_BLACKLIST_REQ_VALUES, [{<<"Event-Category">>, <<"dth">>}
				   ,{<<"Event-Name">>, <<"blacklist_req">>}
				  ]).
-define(DTH_BLACKLIST_REQ_TYPES, []).

-define(DTH_BLACKLIST_RESP_HEADERS, [<<"Accounts">>]). %% [ {CustomerID, "Reason"} ]
-define(OPTIONAL_DTH_BLACKLIST_RESP_HEADERS, []).
-define(DTH_BLACKLIST_RESP_VALUES, [{<<"Event-Category">>, <<"dth">>}
				    ,{<<"Event-Name">>, <<"blacklist_resp">>}
				   ]).
-define(DTH_BLACKLIST_RESP_TYPES, [{<<"Accounts">>, ?IS_JSON_OBJECT}]).
