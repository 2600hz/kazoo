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
        ,is_enabled/2
        ,is_sms_enabled/1, is_mms_enabled/1
        ]).

-include("services.hrl").

-type direction() :: kapps_im:direction().
-type im_type() :: kapps_im:im_type().

-export_type([direction/0
             ,im_type/0
             ]).

-define(DEFAULT_SMS_INBOUND_FLAT_RATE, 0.007).
-define(DEFAULT_SMS_OUTBOUND_FLAT_RATE, 0.007).
-define(DEFAULT_MMS_INBOUND_FLAT_RATE, 0.015).
-define(DEFAULT_MMS_OUTBOUND_FLAT_RATE, 0.015).

-define(IM_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".im">>).
-define(DEFAULT_POLICY_FOR_NO_PLAN, false).
-define(POLICY_FOR_NO_PLAN_KEY(T), [<<"policy">>, <<"no_plan">>, kz_term:to_binary(T), <<"enabled">>]).
-define(POLICY_FOR_NO_PLAN(T), kapps_config:get_boolean(?IM_CONFIG_CAT, ?POLICY_FOR_NO_PLAN_KEY(T), ?DEFAULT_POLICY_FOR_NO_PLAN)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fetch(kz_services:services() | kz_term:ne_binary()) -> kz_json:object().
fetch(?NE_BINARY=AccountId) ->
    case cache(AccountId) of
        not_found ->
            FetchOptions = ['hydrate_plans'],
            Services = kz_services:fetch(AccountId, FetchOptions),
            fetch(Services);
        Plan ->
            Plan
    end;
fetch(Services) ->
    {IMDict, Caches} = kz_services_plans:foldl(fun fetch_foldl/3
                                              ,{dict:new(), []}
                                              ,kz_services:plans(Services)
                                              ),
    cache(kz_services:account_id(Services), Caches, kz_json:from_list(dict:to_list(IMDict))).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fetch_foldl(kz_term:ne_binary(), kz_services_plans:plans_list(), {dict:dict(), list()}) -> dict:dict().
fetch_foldl(_BookkeeperHash, [], Acc) -> Acc;
fetch_foldl(_BookkeeperHash, PlansList, {Providers, Caches}) ->
    Plan = kz_services_plans:merge(PlansList),
    IM = kz_services_plan:im(Plan),
    {kz_json:foldl(fun dict:store/3, Providers, IM), Caches ++ cache_ids(PlansList)}.

cache_ids(PlanList) ->
    [cache_id(Plan) || Plan <- PlanList].

cache_id(Plan) ->
    JObj = kz_services_plan:plan_jobj(Plan),
    {kz_doc:account_db(JObj), kz_doc:id(JObj)}.

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
    is_im_enabled(AccountId, 'sms').

-spec is_mms_enabled(kz_services:services() | kz_term:ne_binary()) -> boolean().
is_mms_enabled(AccountId) ->
    is_im_enabled(AccountId, 'mms').

-spec is_im_enabled(kz_services:services() | kz_term:ne_binary(), im_type()) -> boolean().
is_im_enabled(AccountId, Type) ->
    IM = fetch(AccountId),
    Default = ?POLICY_FOR_NO_PLAN(Type),
    case kz_json:is_empty(IM) of
        'true' -> Default;
        'false' -> kz_json:is_true([kz_term:to_binary(Type), <<"enabled">>], IM, Default)
    end.

-spec is_enabled(kz_services:services() | kz_term:ne_binary(), im_type()) -> boolean().
is_enabled(AccountId, 'sms') ->
    is_im_enabled(AccountId, 'sms');
is_enabled(AccountId, 'mms') ->
    is_im_enabled(AccountId, 'mms');
is_enabled(_AccountId, _) -> 'false'.

cache(AccountId) ->
    case kz_cache:peek_local(?CACHE_NAME, {?MODULE, AccountId}) of
        {error, not_found} -> not_found;
        {ok, Value} -> Value
    end.

cache(AccountId, Caches, Plan) ->
    CacheProps = [{'origin', cache_origin(AccountId, Caches)}
                 ,{'expires', infinity}
                 ],
    catch kz_cache:store_local(?CACHE_NAME, {?MODULE, AccountId}, Plan, CacheProps),
    Plan.

cache_origin(AccountId, Caches) ->
    [{'db', ?KZ_SERVICES_DB, AccountId}
     | [{'db', DB, Id}|| {DB, Id} <- lists:usort(Caches)]
    ].
