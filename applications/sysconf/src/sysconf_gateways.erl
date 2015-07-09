%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(sysconf_gateways).

-export([build/1]).

-include("sysconf.hrl").

-spec build(ne_binary()) -> wh_json:object().
build(_Node) ->
    get_gateways().

-spec get_gateways() -> wh_json:object().
get_gateways() ->
    Routines = [fun get_local_gateways/1
                ,fun get_offnet_gateways/1
               ],
    lists:foldl(fun(F, J) -> F(J) end, wh_json:new(), Routines).

-spec get_local_gateways(wh_json:object()) -> wh_json:object().
get_local_gateways(Gateways) ->
    ViewOptions = [],
    case couch_mgr:get_results(?WH_SIP_DB, <<"resources/listing_uac_gateways">>, ViewOptions) of
        {'error', _R} ->
            lager:debug("unable to fetch resource registrations: ~p", [_R]),
            Gateways;
        {'ok', JObjs} ->
            lists:foldl(fun gateway_fold/2, Gateways, JObjs)
    end.

-spec get_offnet_gateways(wh_json:object()) -> wh_json:object().
get_offnet_gateways(Gateways) ->
    ViewOptions = [],
    case couch_mgr:get_results(?WH_OFFNET_DB, <<"resources/listing_uac_gateways">>, ViewOptions) of
        {'error', _R} ->
            lager:debug("unable to fetch resource registrations: ~p", [_R]),
            Gateways;
        {'ok', JObjs} ->
            lists:foldl(fun gateway_fold/2, Gateways, JObjs)
    end.

-spec gateway_fold(wh_json:object(), wh_json:object()) -> wh_json:object().
gateway_fold(JObj, Acc) ->
    Gateway = wh_json:get_value(<<"value">>, JObj),
    Id = wh_doc:id(Gateway),
    wh_json:set_value(Id, format_gateway(Gateway), Acc).

-spec format_gateway(wh_json:object()) -> wh_json:object().
format_gateway(JObj) ->
    DefaultProxy = 'undefined',
    Variables = [{<<"Account-ID">>, wh_json:get_value(<<"account_id">>, JObj)}
                 ,{<<"Username">>, wh_json:get_value(<<"username">>, JObj)}
                 ,{<<"Realm">>, wh_json:get_value(<<"realm">>, JObj)}
                 ,{<<"Authorizing-Type">>, wh_json:get_value(<<"resource">>, JObj)}
                 ,{<<"Authorizing-ID">>, wh_doc:id(JObj)}
                 ,{<<"Inception">>, <<"${destination_number}">>}
                 ,{<<"Gateway-Version">>, wh_json:get_value(<<"version">>, JObj)}
                ],
    Gateway = [{<<"Username">>, wh_json:get_value(<<"username">>, JObj, <<"none">>)}
               ,{<<"Password">>, wh_json:get_value(<<"password">>, JObj, <<"none">>)}
               ,{<<"Realm">>, wh_json:get_value(<<"server">>, JObj)}
               ,{<<"Proxy">>, wh_json:get_value(<<"proxy">>, JObj, DefaultProxy)}
               ,{<<"From-Domain">>, wh_json:get_value(<<"realm">>, JObj)}
               ,{<<"Expire-Seconds">>, wh_json:get_binary_value(<<"expiration">>, JObj)}
               ,{<<"Register">>, <<"true">>}
               ,{<<"Extension-In-Contact">>, <<"true">>}
               ,{<<"Variables">>, wh_json:from_list(props:filter_undefined(Variables))}
              ],
    wh_json:from_list(props:filter_undefined(Gateway)).
