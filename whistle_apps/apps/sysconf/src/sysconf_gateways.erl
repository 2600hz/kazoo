%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(sysconf_gateways).

-export([build/1]).

-include("sysconf.hrl").

build(_Node) ->
    get_gateways().

get_gateways() ->
    Routines = [fun get_local_gateways/1
                ,fun get_offnet_gateways/1
               ],
    lists:foldl(fun(F, J) -> F(J) end, wh_json:new(), Routines).

get_local_gateways(Gateways) ->
    ViewOptions = [],
    case couch_mgr:get_results(?WH_SIP_DB, <<"resources/listing_uac_gateways">>, ViewOptions) of
        {error, _R} ->
            lager:debug("unable to fetch resource registrations: ~p", [_R]),
            Gateways;
        {ok, JObjs} ->
            lists:foldl(fun(JObj, J) ->
                                Gateway = wh_json:get_value(<<"value">>, JObj),
                                Id = wh_json:get_value(<<"id">>, Gateway),
                                wh_json:set_value(Id, format_gateway(Gateway), J)
                        end, Gateways, JObjs)
    end.

get_offnet_gateways(Gateways) ->
    ViewOptions = [],
    case couch_mgr:get_results(?WH_SIP_DB, <<"resources/listing_uac_gateways">>, ViewOptions) of
        {error, _R} ->
            lager:debug("unable to fetch resource registrations: ~p", [_R]),
            Gateways;
        {ok, JObjs} ->
            lists:foldl(fun(JObj, J) ->
                                Gateway = wh_json:get_value(<<"value">>, JObj),
                                Id = wh_json:get_value(<<"id">>, Gateway),
                                wh_json:set_value(Id, format_gateway(Gateway), J)
                        end, Gateways, JObjs)
    end.

format_gateway(JObj) ->
    DefaultProxy = undefined,
    Variables = [{<<"Account-ID">>, wh_json:get_value(<<"account_id">>, JObj)}
                 ,{<<"Username">>, wh_json:get_value(<<"username">>, JObj)}
                 ,{<<"Realm">>, wh_json:get_value(<<"realm">>, JObj)}
                 ,{<<"Authorizing-Type">>, wh_json:get_value(<<"resource">>, JObj)}
                 ,{<<"Authorizing-ID">>, wh_json:get_value(<<"id">>, JObj)}
                 ,{<<"Inception">>, <<"off-net">>}
                 ,{<<"Gateway-Version">>, wh_json:get_value(<<"version">>, JObj)}
                ],
    Gateway = [{<<"Username">>, wh_json:get_value(<<"username">>, JObj)}
               ,{<<"Password">>, wh_json:get_value(<<"password">>, JObj)}
               ,{<<"Realm">>, wh_json:get_value(<<"server">>, JObj)}
               ,{<<"Proxy">>, wh_json:get_value(<<"proxy">>, JObj, DefaultProxy)}
               ,{<<"From-Domain">>, wh_json:get_value(<<"realm">>, JObj, <<"auto-aleg-full">>)}
               ,{<<"Expire-Seconds">>, wh_json:get_value(<<"expiration">>, JObj)}
               ,{<<"Register">>, <<"true">>}
               ,{<<"Variables">>, wh_json:from_list(props:filter_undefined(Variables))}
              ],
    wh_json:from_list(props:filter_undefined(Gateway)).
                                                                                               
