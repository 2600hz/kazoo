%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Handle registration queries
%%% @end
%%% Created : 19 Aug 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(reg_query).

-include("reg.hrl").

-export([init/0]).
-export([req_query_req/2]).
-export([presence_probe/2]).

init() ->
    ok.

-spec presence_probe/2 :: (wh_json:json_object(), proplist()) -> ok.
presence_probe(ApiJObj, _Props) ->
    case wh_json:get_value(<<"Subscription">>, ApiJObj) of
        <<"message-summary">> -> ok;
        _Else ->
            ToRealm = wh_json:get_ne_value(<<"To-Realm">>, ApiJObj),
            ToUser = wh_json:get_ne_value(<<"To-User">>, ApiJObj),
            FromRealm = wh_json:get_ne_value(<<"From-Realm">>, ApiJObj),
            FromUser = wh_json:get_ne_value(<<"From-User">>, ApiJObj),
            case reg_util:lookup_registration(ToRealm, ToUser) of
                {ok, RegJObjs} when is_list(RegJObjs) ->
                    PresenceUpdate = [{<<"Presence-ID">>, list_to_binary([ToUser, "@", ToRealm])}
                                      ,{<<"To">>, list_to_binary([FromUser, "@", FromRealm])}
                                      ,{<<"Switch-Nodename">>, wh_json:get_ne_value(<<"Switch-Nodename">>, ApiJObj)}
                                      ,{<<"Subscription-Call-ID">>, wh_json:get_ne_value(<<"Subscription-Call-ID">>, ApiJObj)}
                                      ,{<<"Dialog-State">>, wh_json:get_ne_value(<<"Dialog-State">>, ApiJObj)}
                                      | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                                     ],
                    wapi_notifications:publish_presence_update(PresenceUpdate);
                {ok, _} ->
                    PresenceUpdate = [{<<"Presence-ID">>, list_to_binary([ToUser, "@", ToRealm])}
                                      ,{<<"To">>, list_to_binary([FromUser, "@", FromRealm])}
                                      ,{<<"Switch-Nodename">>, wh_json:get_ne_value(<<"Switch-Nodename">>, ApiJObj)}
                                      ,{<<"Subscription-Call-ID">>, wh_json:get_ne_value(<<"Subscription-Call-ID">>, ApiJObj)}
                                      ,{<<"Dialog-State">>, wh_json:get_ne_value(<<"Dialog-State">>, ApiJObj)}
                                      | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                                     ],
                    wapi_notifications:publish_presence_update(PresenceUpdate);
                {error, not_found} -> 
                    ok
            end
    end,
    ok.

-spec req_query_req/2 :: (wh_json:json_object(), proplist()) -> ok.
req_query_req(ApiJObj, _Props) ->
    true = wapi_registration:query_req_v(ApiJObj),

    CallId = wh_json:get_value(<<"Call-ID">>, ApiJObj, <<"000000000000">>),
    put(callid, CallId),

    lager:debug("received registration query"),
    true = wapi_registration:query_req_v(ApiJObj),

    Realm = wh_json:get_value(<<"Realm">>, ApiJObj),
    Username = wh_json:get_value(<<"Username">>, ApiJObj),
    MsgID = wh_json:get_value(<<"Msg-ID">>, ApiJObj),

    %% only send data if a registration is found
    case reg_util:lookup_registration(Realm, Username) of
        {ok, RegJObjs} when is_list(RegJObjs) ->
            lager:debug("found multiple contacts for ~s@~s in cache", [Username, Realm]),
            Resp = [{<<"Multiple">>, <<"true">>}
                    ,{<<"Msg-ID">>, MsgID}
                    ,{<<"Fields">>, [filter(ApiJObj, RegJObj) 
                                     || RegJObj <- RegJObjs
                                    ]}
                    | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            wapi_registration:publish_query_resp(wh_json:get_value(<<"Server-ID">>, ApiJObj), Resp),
            lager:debug("sent multiple registration AORs");
        {ok, RegJObj} ->
            lager:debug("found contact for ~s@~s in cache", [Username, Realm]),
            Resp = [{<<"Fields">>, filter(ApiJObj, RegJObj)}
                    ,{<<"Msg-ID">>, MsgID}
                    | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            wapi_registration:publish_query_resp(wh_json:get_value(<<"Server-ID">>, ApiJObj), Resp),
            lager:debug("sent reply for AOR: ~s", [wh_json:get_value(<<"Contact">>, RegJObj)]);
        {error, not_found} ->
            lager:debug("no registration for ~s@~s", [Username, Realm])
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% extract the requested fields from the registration and send a response
%% @end
%%-----------------------------------------------------------------------------
-spec filter/2 :: (wh_json:json_object(), wh_json:json_object()) -> ok.
filter(ApiJObj, RegJObj) ->
    case wh_json:get_value(<<"Fields">>, ApiJObj, []) of
        [] ->
            wh_json:delete_key(<<"_id">>, wh_json:delete_key(<<"_rev">>, RegJObj));
        Fields ->
            wh_json:from_list(lists:foldl(fun(F, Acc) ->
                                                  [ {F, wh_json:get_value(F, RegJObj)} | Acc]
                                          end, [], Fields))
    end.
