%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%   Edouard Swiac
%%%-------------------------------------------------------------------
-module(lync_handlers).

-export([handle_route_req/2
         ,handle_epid_req/2
        ]).

-include("lync.hrl").

-spec handle_route_req/2 :: (wh_json:json_object(), wh_proplist()) -> any().
handle_route_req(JObj, _Props) ->
    %true = wapi_route:req_v(JObj),
    Epid = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Epid-From">>], JObj, undefined),
    IncAccountId = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj, undefined),
    lager:debug("route_req received by lync_handler for epid:~s", [Epid]),
    case IncAccountId of
        undefined -> case Epid of
                        undefined -> lager:debug("no account_id matching epid");
                        Epid -> case wh_cache:fetch(Epid) of
                                  {ok, AccountId} ->
                                            lager:debug("Replaying route request for ~s:~s", [Epid, AccountId]),
                                            relay_route_req(
                                              wh_json:set_value(<<"Custom-Channel-Vars">>, custom_channel_vars(AccountId, undefined, JObj), JObj)
                                            );
                                  {error, _} -> bye
                                  end
                      end;
        _ -> ok
    end, 
    ok.

-spec handle_epid_req/2 :: (wh_json:json_object(), wh_proplist()) -> any().
handle_epid_req(JObj, _Props) -> 
    true = wapi_lync:broadcast_epid_v(JObj),
    Epid = wh_json:get_value(<<"Epid">>, JObj),
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    lager:debug("epid_req received by lync_handler") ,
    %% Store EPID as Key, Account Id as value for 60 seconds
    wh_cache:store(Epid, AccountId, 60),
    lager:debug("stored ~s:~s", [Epid, AccountId]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% build the JSON to set the custom channel vars with the calls
%% account and authorizing  ID
%% @end
%%--------------------------------------------------------------------
-spec custom_channel_vars/3 :: ('undefined' | ne_binary(), 'undefined' | ne_binary(), wh_json:json_object()) -> wh_json:json_object().
custom_channel_vars(AccountId, AuthId, JObj) ->
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new()),
    Vars = [{<<"Account-ID">>, AccountId}
            ,{<<"Inception">>, <<"off-net">>}
            ,{<<"Authorizing-ID">>, AuthId}
            | [Var || {K, _}=Var <- wh_json:to_proplist(CCVs)
                           ,K =/= <<"Account-ID">>
                           ,K =/= <<"Inception">>
                           ,K =/= <<"Authorizing-ID">>
               ]
           ],
    wh_json:from_list([ KV || {_, V}=KV <- Vars, V =/= undefined ]).
%%--------------------------------------------------------------------
%% @private
%% @doc
%% relay a route request once populated with the new properties
%% @end
%%--------------------------------------------------------------------
-spec relay_route_req/1 :: (wh_json:json_object()) -> 'ok'.
relay_route_req(Req) ->
    wapi_route:publish_req(Req),
    lager:debug("relayed route request").
