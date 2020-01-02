%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_services_im).

-export([fetch/1
        ,flat_rate/3
        ,is_enabled/2, is_sms_enabled/1, is_mms_enabled/1
        ]).

-include("services.hrl").

-define(DEFAULT_SMS_INBOUND_FLAT_RATE, 0.007).
-define(DEFAULT_SMS_OUTBOUND_FLAT_RATE, 0.007).
-define(DEFAULT_MMS_INBOUND_FLAT_RATE, 0.015).
-define(DEFAULT_MMS_OUTBOUND_FLAT_RATE, 0.015).

-type direction() :: kapps_im:direction().
-type im_type() :: kapps_im:im_type().

-export_type([direction/0
             ,im_type/0
             ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fetch(kz_services:services() | kz_term:ne_binary()) -> kz_json:object().
fetch(?NE_BINARY=AccountId) ->
    FetchOptions = ['hydrate_plans'],
    fetch(kz_services:fetch(AccountId, FetchOptions));
fetch(Services) ->
    IMDict = kz_services_plans:foldl(fun fetch_foldl/3
                                    ,dict:new()
                                    ,kz_services:plans(Services)
                                    ),
    kz_json:from_list(dict:to_list(IMDict)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fetch_foldl(kz_term:ne_binary(), kz_services_plans:plans_list(), dict:dict()) -> dict:dict().
fetch_foldl(_BookkeeperHash, [], Providers) ->
    Providers;
fetch_foldl(_BookkeeperHash, PlansList, Providers) ->
    Plan = kz_services_plans:merge(PlansList),
    kz_json:foldl(fun(K, V, A) ->
                          dict:store(K, V, A)
                  end
                 ,Providers
                 ,kz_services_plan:im(Plan)
                 ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec flat_rate(kz_term:ne_binary(), im_type(), direction()) -> kz_currency:dollars().
flat_rate(AccountId, IM, Direction) ->
    Default = default_flat_rate(IM, Direction),
    Items = fetch(AccountId),
    Key = [kz_term:to_binary(IM)
          ,<<"rate">>
          ,kz_term:to_binary(Direction)
          ],
    kz_json:get_number_value(Key, Items, Default).

default_flat_rate('sms', 'inbound') -> ?DEFAULT_SMS_INBOUND_FLAT_RATE;
default_flat_rate('sms', 'outbound') -> ?DEFAULT_SMS_OUTBOUND_FLAT_RATE;
default_flat_rate('mms', 'inbound') -> ?DEFAULT_MMS_INBOUND_FLAT_RATE;
default_flat_rate('mms', 'outbound') -> ?DEFAULT_MMS_OUTBOUND_FLAT_RATE;
default_flat_rate(_, _) -> 0.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_sms_enabled(kz_services:services() | kz_term:ne_binary()) -> boolean().
is_sms_enabled(AccountId) ->
    kz_json:is_true([<<"sms">>, <<"enabled">>], fetch(AccountId)).

-spec is_mms_enabled(kz_services:services() | kz_term:ne_binary()) -> boolean().
is_mms_enabled(AccountId) ->
    kz_json:is_true([<<"mms">>, <<"enabled">>], fetch(AccountId)).

-spec is_enabled(kz_services:services() | kz_term:ne_binary(), im_type()) -> boolean().
is_enabled(AccountId, 'sms') ->
    is_sms_enabled(AccountId);
is_enabled(AccountId, 'mms') ->
    is_mms_enabled(AccountId);
is_enabled(_AccountId, _) -> 'false'.
