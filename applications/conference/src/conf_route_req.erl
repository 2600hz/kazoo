%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(conf_route_req).

-include("conference.hrl").

-export([handle_req/2]).

-spec handle_req(wh_json:object(), wh_proplist()) -> any().
handle_req(JObj, Props) ->
    'true' = wapi_route:req_v(JObj),
    wh_util:put_callid(JObj),
    Call = whapps_call:from_route_req(JObj),
    case whapps_call:request_user(Call) of
        <<"conference">> ->
            Q = props:get_value('queue', Props),
            maybe_send_route_response(JObj, Q, Call);
         _Else -> 'ok'
    end.

maybe_send_route_response(JObj, Q, Call) ->
    case find_conference(Call) of
        {'ok', Conference} ->
            send_route_response(Conference, JObj, Q, Call);
        {'error', _} -> 'ok'
    end.

send_route_response(Conference, JObj, Q, Call) ->
    wh_cache:store_local(?CONFERENCE_CACHE, whapps_call:call_id(Call), {Conference, Call}),
    Resp = props:filter_undefined([{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                                   ,{<<"Routes">>, []}
                                   ,{<<"Method">>, <<"park">>}
                                   | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
                                  ]),
    ServerId = wh_json:get_value(<<"Server-ID">>, JObj),
    Publisher = fun(P) -> wapi_route:publish_resp(ServerId, P) end,
    whapps_util:amqp_pool_send(Resp, Publisher),
    lager:info("conference knows how to route the call! sent park response").

find_conference(Call) ->
    find_conference(find_account_db(Call), Call).

find_conference('undefined', _) -> {'error', 'realm_unknown'};
find_conference(AccountDb, Call) ->
    ConferenceId = whapps_call:to_user(Call),
    case couch_mgr:open_doc(AccountDb, ConferenceId) of
        {'ok', JObj} ->
            <<"conference">> = wh_json:get_value(<<"pvt_type">>, JObj),
            {'ok', whapps_conference:from_conference_doc(JObj)};
        {'error', _R}=Error ->
            lager:info("unable to find conference ~s in account db ~s: ~p"
                       ,[ConferenceId, AccountDb, _R]),
            Error
    end.

find_account_db(Call) ->
    Realm = whapps_call:to_realm(Call),
    case whapps_util:get_account_by_realm(Realm) of
        {'ok', AccountDb} -> AccountDb;
        {'multiples', [AccountDb|_]} -> AccountDb;
        {'error', _R} -> 
            lager:debug("unable to find account for realm ~s: ~p"
                        ,[Realm, _R]),
            'undefined'
    end.
