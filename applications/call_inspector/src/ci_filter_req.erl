%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(ci_filter_req).

-export([handle_req/2]).

-include("call_inspector.hrl").

-spec handle_req(kz_json:object(), kz_term:proplist()) -> ok.
handle_req(JObj, _Props) ->
    true = kapi_inspector:filter_req_v(JObj),
    CallIds = [CallId
               || CallId <- kz_json:get_value(<<"Call-IDs">>, JObj, []),
                  ci_datastore:callid_exists(CallId)
              ],
    Data = kz_json:from_list(
             [{<<"Call-IDs">>, CallIds}
             ,{<<"Msg-ID">>, kz_api:msg_id(JObj)}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    kapi_inspector:publish_filter_resp(kz_api:server_id(JObj), Data).
