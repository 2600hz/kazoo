%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_gateways).

-export([get/0]).

-include("ecallmgr.hrl").

-spec get() -> kz_json:object().
get() ->
    Routines = [fun get_local_gateways/1
               ,fun get_offnet_gateways/1
               ],
    lists:foldl(fun(F, J) -> F(J) end, kz_json:new(), Routines).

-spec get_local_gateways(kz_json:object()) -> kz_json:object().
get_local_gateways(Gateways) ->
    ViewOptions = [],
    case kz_datamgr:get_results(?KZ_SIP_DB, <<"resources/listing_uac_gateways">>, ViewOptions) of
        {'error', _R} ->
            lager:debug("unable to fetch resource registrations: ~p", [_R]),
            Gateways;
        {'ok', JObjs} ->
            lists:foldl(fun gateway_fold/2, Gateways, JObjs)
    end.

-spec get_offnet_gateways(kz_json:object()) -> kz_json:object().
get_offnet_gateways(Gateways) ->
    ViewOptions = [],
    case kz_datamgr:get_results(?KZ_OFFNET_DB, <<"resources/listing_uac_gateways">>, ViewOptions) of
        {'error', _R} ->
            lager:debug("unable to fetch resource registrations: ~p", [_R]),
            Gateways;
        {'ok', JObjs} ->
            lists:foldl(fun gateway_fold/2, Gateways, JObjs)
    end.

-spec gateway_fold(kz_json:object(), kz_json:object()) -> kz_json:object().
gateway_fold(JObj, Acc) ->
    Gateway = kz_json:get_json_value(<<"value">>, JObj),
    Id = kz_doc:id(Gateway),
    kz_json:set_value(Id, format_gateway(Gateway), Acc).

-spec format_gateway(kz_json:object()) -> kz_json:object().
format_gateway(JObj) ->
    Variables =
        kz_json:from_list(
          [{<<"ecallmgr_Account-ID">>, kz_json:get_value(<<"account_id">>, JObj)}
          ,{<<"ecallmgr_Username">>, kz_json:get_value(<<"username">>, JObj)}
          ,{<<"ecallmgr_Realm">>, kz_json:get_value(<<"realm">>, JObj)}
          ,{<<"ecallmgr_Authorizing-Type">>, kz_json:get_ne_value(<<"resource">>, JObj, <<"resource">>)}
          ,{<<"ecallmgr_Authorizing-ID">>, kz_doc:id(JObj)}
          ,{<<"ecallmgr_Resource-ID">>, kz_json:get_ne_value(<<"resource_id">>, JObj, kz_doc:id(JObj))}
          ,{<<"ecallmgr_Inception">>, <<"${destination_number}">>}
          ,{<<"ecallmgr_Gateway-Version">>, kz_json:get_value(<<"version">>, JObj)}
          ]),
    kz_json:from_list(
      [{<<"Username">>, kz_json:get_value(<<"username">>, JObj, <<"none">>)}
      ,{<<"Auth-Username">>, kz_json:get_value(<<"auth_username">>, JObj)}
      ,{<<"Password">>, kz_json:get_value(<<"password">>, JObj, <<"none">>)}
      ,{<<"Realm">>, kz_json:get_value(<<"server">>, JObj)}
      ,{<<"Proxy">>, kz_json:get_value(<<"proxy">>, JObj)}
      ,{<<"From-Domain">>, kz_json:get_value(<<"realm">>, JObj)}
      ,{<<"Expire-Seconds">>, kz_json:get_binary_value(<<"expiration">>, JObj)}
      ,{<<"Register">>, <<"true">>}
      ,{<<"Extension-In-Contact">>, <<"true">>}
      ,{<<"Extension">>, kz_json:get_ne_value(<<"register_extension">>, JObj)}
      ,{<<"Variables">>, Variables}
      ]).
