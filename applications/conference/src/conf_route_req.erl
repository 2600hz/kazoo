%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(conf_route_req).

-include("conference.hrl").

-export([handle_req/2]).

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
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

-spec maybe_send_route_response(wh_json:object(), ne_binary(), whapps_call:call()) -> 'ok'.
maybe_send_route_response(JObj, Q, Call) ->
    case find_conference(Call) of
        {'ok', Conference} ->
            send_route_response(JObj, Q, Call, Conference);
        {'error', _} -> 'ok'
    end.

-spec send_route_response(wh_json:object(), ne_binary(), whapps_call:call(), whapps_conference:conference()) ->
                                 'ok'.
send_route_response(JObj, Q, Call, Conference) ->
    conf_util:cache(Call, Conference),
    Resp = props:filter_undefined([{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                                   ,{<<"Routes">>, []}
                                   ,{<<"Method">>, <<"park">>}
                                   | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
                                  ]),
    ServerId = wh_json:get_value(<<"Server-ID">>, JObj),
    Publisher = fun(P) -> wapi_route:publish_resp(ServerId, P) end,
    whapps_util:amqp_pool_send(Resp, Publisher),
    lager:info("conference knows how to route the call! sent park response").

-spec find_conference(whapps_call:call()) -> {'error', _} |
                                             {'ok', whapps_conference:conference()}.
find_conference(Call) ->
    find_conference(Call, find_account_db(Call)).

find_conference(_Call, 'undefined') -> {'error', 'realm_unknown'};
find_conference(Call, AccountDb) ->
    ConferenceId = whapps_call:to_user(Call),
    case couch_mgr:open_cache_doc(AccountDb, ConferenceId) of
        {'ok', JObj} ->
            <<"conference">> = wh_doc:type(JObj),
            {'ok', whapps_conference:from_conference_doc(JObj)};
        {'error', _R}=Error ->
            lager:info("unable to find conference ~s in account db ~s: ~p"
                       ,[ConferenceId, AccountDb, _R]
                      ),
            Error
    end.

-spec find_account_db(whapps_call:call()) -> api_binary().
find_account_db(Call) ->
    Realm = whapps_call:to_realm(Call),
    case whapps_util:get_account_by_realm(Realm) of
        {'ok', AccountDb} -> AccountDb;
        {'multiples', [AccountDb|_]} -> AccountDb;
        {'error', _R} ->
            lager:debug("unable to find account for realm ~s: ~p"
                        ,[Realm, _R]
                       ),
            'undefined'
    end.
