%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @author James Aimonetti
%%% @author Luis Azedo
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_directory_resource).

-export([profile/2, profile/3]).

-include("kazoo_directory.hrl").

-type resource_param() :: {kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist(), knm_number_options:extra_options()}.

-spec profile(kz_term:ne_binary(), kz_term:ne_binary()) -> {'ok', kz_json:object()} | {'error', any()}.
profile(EndpointId, AccountId) ->
    profile(EndpointId, AccountId, []).

-spec profile(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> {'ok', kz_json:object()} | {'error', any()}.
profile(EndpointId, AccountId, Options) ->
    Number = number(Options),
    case knm_number:lookup_account(Number) of
        {'error', _R} = Error ->
            lager:info("unable to determine account for number ~s: ~p",[Number, _R]),
            Error;
        {'ok', _, NumberProps} ->
            generate_profile({EndpointId, AccountId, Options, NumberProps})
    end.

-spec number(kz_term:proplist()) -> kz_term:ne_binary().
number(Options) ->
    JObj = props:get_value('cauth', Options, kz_json:new()),
    Num = kz_json:get_ne_binary_value(<<"URI-User">>, JObj),
    knm_converters:normalize(Num).

-spec set_account_id(resource_param(), kz_json:object()) -> kz_json:object().
set_account_id({_, _, _, NumberProps}, JObj) ->
    AccountId = knm_number_options:account_id(NumberProps),
    {'ok', Account} = kzd_accounts:fetch(AccountId, 'accounts'),
    Realm = kzd_accounts:realm(Account),
    kz_json:set_values([{<<"Account-ID">>, AccountId}
                       ,{<<"Account-Realm">>, Realm}
                       ,{<<"Realm">>, Realm}
                       ,{<<"Reseller-ID">>, kzd_accounts:reseller_id(Account)}
                       ]
                      ,JObj
                      ).

-spec set_inception(resource_param(), kz_json:object()) -> kz_json:object().
set_inception({_, _, Options, _}, JObj) ->
    J = props:get_value('cauth', Options, kz_json:new()),
    Request = list_to_binary(
                [kz_json:get_value(<<"URI-User">>, J)
                ,"@"
                ,kz_json:get_value(<<"URI-Realm">>, J)
                ]),
    kz_json:set_value(<<"Inception">>, Request, JObj).

-spec set_resource_type(resource_param(), kz_json:object()) -> kz_json:object().
set_resource_type(_, JObj) ->
    kz_json:set_value(<<"Resource-Type">>, <<"offnet-origination">>, JObj).

-spec set_resource_id(resource_param(), kz_json:object()) -> kz_json:object().
set_resource_id({ResourceId, _, _, _}, JObj) ->
    kz_json:set_values([{<<"Resource-ID">>, ResourceId}
                       ,{<<"Global-Resource">>, 'true'}
                       ,{<<"Authorizing-Type">>, <<"resource">>}
                        %% TODO
                        %% we need to make sure that Authorizing-ID is used
                        %% with Authorizing-Type in ALL kapps
                        %% when this is done remove the comment below
                        %% ,{?CCV(<<"Authorizing-ID">>), ResourceId}
                       ]
                      ,JObj
                      ).

-spec generate_profile(resource_param()) -> {'ok', kz_json:object()}.
generate_profile({EndpointId, AccountId, _Options, _Number} = Props) ->
    Routines = [fun set_account_id/2
               ,fun set_inception/2
               ,fun set_resource_type/2
               ,fun set_resource_id/2
               ],
    CCVs = lists:foldl(fun(F, J) -> F(Props, J) end, kz_json:new(), Routines),

    Profile = [{<<"Endpoint-Type">>, <<"resource">>}
              ,{<<"Domain-Name">>, AccountId}
              ,{<<"User-ID">>, EndpointId}
              ,{<<"Custom-Channel-Vars">>, CCVs}
              ],
    {'ok', kz_json:from_list(Profile)}.
