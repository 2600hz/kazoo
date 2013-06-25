%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, 2600Hz
%%% @doc
%%% Handle registration queries
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(reg_query).

-include("reg.hrl").

-export([init/0]).
-export([req_query_req/2]).
-export([presence_probe/2]).

init() -> 'ok'.

-spec presence_probe(wh_json:object(), wh_proplist()) -> 'ok'.
presence_probe(ApiJObj, _Props) ->
    process_presence_probe(ApiJObj, wh_json:get_value(<<"Subscription">>, ApiJObj)).

process_presence_probe(_, <<"message-summary">>) -> 'ok';
process_presence_probe(ApiJObj, _) ->
    ToRealm = wh_json:get_ne_value(<<"To-Realm">>, ApiJObj),
    ToUser = wh_json:get_ne_value(<<"To-User">>, ApiJObj),
    FromRealm = wh_json:get_ne_value(<<"From-Realm">>, ApiJObj),
    FromUser = wh_json:get_ne_value(<<"From-User">>, ApiJObj),
    case reg_util:lookup_registration(ToRealm, ToUser) of
        {'error', 'not_found'} -> 'ok';
        {'ok', _} ->
            PresenceUpdate = [{<<"Presence-ID">>, list_to_binary([ToUser, "@", ToRealm])}
                              ,{<<"To">>, list_to_binary([FromUser, "@", FromRealm])}
                              ,{<<"Switch-Nodename">>, wh_json:get_ne_value(<<"Switch-Nodename">>, ApiJObj)}
                              ,{<<"Subscription-Call-ID">>, wh_json:get_ne_value(<<"Subscription-Call-ID">>, ApiJObj)}
                              ,{<<"Dialog-State">>, wh_json:get_ne_value(<<"Dialog-State">>, ApiJObj)}
                              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                             ],
            wapi_notifications:publish_presence_update(PresenceUpdate)
    end.

-spec req_query_req(wh_json:object(), wh_proplist()) -> 'ok'.
req_query_req(ApiJObj, _Props) ->
    'true' = wapi_registration:query_req_v(ApiJObj),
    _ = wh_util:put_callid(ApiJObj),

    Realm = wh_json:get_value(<<"Realm">>, ApiJObj),
    Username = wh_json:get_value(<<"Username">>, ApiJObj),
    MsgID = wh_json:get_value(<<"Msg-ID">>, ApiJObj),

    %% only send data if a registration is found
    case reg_util:lookup_registration(Realm, Username) of
        {'ok', RegJObjs} when is_list(RegJObjs) ->
            lager:debug("found multiple contacts for ~s@~s in cache", [Username, Realm]),
            Resp = [{<<"Multiple">>, <<"true">>}
                    ,{<<"Msg-ID">>, MsgID}
                    ,{<<"Fields">>, [filter(ApiJObj, RegJObj)
                                     || RegJObj <- RegJObjs
                                    ]}
                    | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            wapi_registration:publish_query_resp(wh_json:get_value(<<"Server-ID">>, ApiJObj), Resp);
        {'ok', RegJObj} ->
            lager:debug("found contact for ~s@~s in cache", [Username, Realm]),
            Resp = [{<<"Fields">>, filter(ApiJObj, RegJObj)}
                    ,{<<"Msg-ID">>, MsgID}
                    | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            wapi_registration:publish_query_resp(wh_json:get_value(<<"Server-ID">>, ApiJObj), Resp);
        {'error', 'not_found'} ->
            lager:debug("no registration for ~s@~s", [Username, Realm]),
            maybe_send_error(MsgID, ApiJObj)
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec maybe_send_error(ne_binary(), wh_json:object()) -> 'ok'.
maybe_send_error(MsgID, ApiJObj) ->
    case wh_json:is_true(<<"Suppress-Errors">>, ApiJObj) of
        'true' -> 'ok';
        'false' ->
            Resp = [{<<"Msg-ID">>, MsgID}
                    | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            wapi_registration:publish_query_err(wh_json:get_value(<<"Server-ID">>, ApiJObj), Resp)
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% extract the requested fields from the registration and send a response
%% @end
%%-----------------------------------------------------------------------------
-spec filter(wh_json:object(), wh_json:object()) -> wh_json:object().
filter(ApiJObj, RegJObj) ->
    case wh_json:get_value(<<"Fields">>, ApiJObj, []) of
        [] -> wh_json:delete_key(<<"_id">>, wh_json:delete_key(<<"_rev">>, RegJObj));
        Fields ->
            wh_json:from_list(lists:foldl(fun(F, Acc) ->
                                                  [ {F, wh_json:get_value(F, RegJObj)} | Acc]
                                          end, [], Fields))
    end.
