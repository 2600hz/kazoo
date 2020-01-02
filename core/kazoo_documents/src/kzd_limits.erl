%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_limits).

-export([new/1]).
-export([enabled/1, enabled/2
        ,set_enabled/2, set_pvt_enabled/2
        ]).
-export([calls/1, calls/2
        ,set_calls/2, set_pvt_calls/2
        ]).
-export([resource_consuming_calls/1, resource_consuming_calls/2
        ,set_resource_consuming_calls/2, set_pvt_resource_consuming_calls/2
        ]).
-export([inbound_trunks/1, inbound_trunks/2
        ,set_inbound_trunks/2, set_pvt_inbound_trunks/2
        ]).
-export([outbound_trunks/1, outbound_trunks/2
        ,set_outbound_trunks/2, set_pvt_outbound_trunks/2
        ]).
-export([twoway_trunks/1, twoway_trunks/2
        ,set_twoway_trunks/2, set_pvt_twoway_trunks/2
        ]).
-export([bundled_inbound_trunks/1, bundled_inbound_trunks/2
        ,set_pvt_bundled_inbound_trunks/2
        ]).
-export([bundled_outbound_trunks/1, bundled_outbound_trunks/2
        ,set_pvt_bundled_outbound_trunks/2
        ]).
-export([bundled_twoway_trunks/1, bundled_twoway_trunks/2
        ,set_pvt_bundled_twoway_trunks/2
        ]).
-export([burst_trunks/1, burst_trunks/2
        ,set_burst_trunks/2, set_pvt_burst_trunks/2
        ]).
-export([max_postpay_units/1, max_postpay_units/2
        ,set_pvt_max_postpay_units/2
        ]).
-export([max_postpay_dollars/1, max_postpay_dollars/2
        ,set_pvt_max_postpay_dollars/2
        ]).
-export([reserve_units/1, reserve_units/2
        ,set_pvt_reserve_units/2
        ]).
-export([reserve_dollars/1, reserve_dollars/2
        ,set_pvt_reserve_dollars/2
        ]).
-export([allow_prepay/1, allow_prepay/2
        ,set_allow_prepay/2, set_pvt_allow_prepay/2
        ]).
-export([allow_postpay/1, allow_postpay/2
        ,set_pvt_allow_postpay/2
        ]).
-export([soft_limit_inbound/1, soft_limit_inbound/2
        ,set_pvt_soft_limit_inbound/2
        ]).
-export([soft_limit_outbound/1, soft_limit_outbound/2
        ,set_pvt_soft_limit_outbound/2
        ]).
-export([authz_resource_types/1, authz_resource_types/2
        ,set_authz_resource_types/2, set_pvt_authz_resource_types/2
        ]).
-export([allotments/1, allotments/2
        ,set_allotments/2
        ]).
-export([inbound_channels_per_did_rules/1, inbound_channels_per_did_rules/2]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().

-type tristate_integer() :: -1 | non_neg_integer().

-export_type([doc/0
             ,tristate_integer/0
             ]).

-define(SCHEMA, <<"limits">>).
-define(LIMITS_CAT, <<"limits">>).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec new(kz_term:ne_binary()) -> doc().
new(Account) ->
    TStamp = kz_time:now_s(),
    kz_json:from_list(
      [{<<"_id">>, <<"limits">>}
      ,{<<"pvt_account_db">>, kzs_util:format_account_db(Account)}
      ,{<<"pvt_account_id">>, kzs_util:format_account_id(Account)}
      ,{<<"pvt_type">>, <<"limits">>}
      ,{<<"pvt_created">>, TStamp}
      ,{<<"pvt_modified">>, TStamp}
      ,{<<"pvt_vsn">>, 1}
      ]
     ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec enabled(doc()) -> boolean().
enabled(Doc) ->
    enabled(Doc, 'true').

-spec enabled(doc(), Default) -> boolean() | Default.
enabled(Doc, Default) ->
    get_limit_boolean(<<"enabled">>, Doc, Default).

-spec set_enabled(doc(), boolean()) -> doc().
set_enabled(Doc, Enabled) ->
    kz_json:set_value(<<"enabled">>, kz_term:is_true(Enabled), Doc).

-spec set_pvt_enabled(doc(), boolean()) -> doc().
set_pvt_enabled(Doc, Enabled) ->
    kz_json:set_value(<<"pvt_enabled">>, kz_term:is_true(Enabled), Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec calls(doc()) -> tristate_integer().
calls(Doc) ->
    calls(Doc, -1).

-spec calls(doc(), Default) -> tristate_integer() | Default.
calls(Doc, Default) ->
    get_limit(<<"calls">>, Doc, Default).

-spec set_calls(doc(), tristate_integer()) -> doc().
set_calls(Doc, Calls) ->
    kz_json:set_value(<<"calls">>, Calls, Doc).

-spec set_pvt_calls(doc(), tristate_integer()) -> doc().
set_pvt_calls(Doc, Calls) ->
    kz_json:set_value(<<"pvt_calls">>, Calls, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec resource_consuming_calls(doc()) -> tristate_integer().
resource_consuming_calls(Doc) ->
    resource_consuming_calls(Doc, -1).

-spec resource_consuming_calls(doc(), Default) -> tristate_integer() | Default.
resource_consuming_calls(Doc, Default) ->
    get_limit(<<"resource_consuming_calls">>, Doc, Default).

-spec set_resource_consuming_calls(doc(), tristate_integer()) -> doc().
set_resource_consuming_calls(Doc, ResourceConsumingCalls) ->
    kz_json:set_value(<<"resource_consuming_calls">>, ResourceConsumingCalls, Doc).

-spec set_pvt_resource_consuming_calls(doc(), tristate_integer()) -> doc().
set_pvt_resource_consuming_calls(Doc, ResourceConsumingCalls) ->
    kz_json:set_value(<<"pvt_resource_consuming_calls">>, ResourceConsumingCalls, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec inbound_trunks(doc()) -> tristate_integer().
inbound_trunks(Doc) ->
    inbound_trunks(Doc, 0).

-spec inbound_trunks(doc(), Default) -> tristate_integer() | Default.
inbound_trunks(Doc, Default) ->
    get_limit_integer(<<"inbound_trunks">>, Doc, Default).

-spec set_inbound_trunks(doc(), tristate_integer()) -> doc().
set_inbound_trunks(Doc, InboundTrunks) ->
    kz_json:set_value([<<"inbound_trunks">>], InboundTrunks, Doc).

-spec set_pvt_inbound_trunks(doc(), tristate_integer()) -> doc().
set_pvt_inbound_trunks(Doc, InboundTrunks) ->
    kz_json:set_value([<<"inbound_trunks">>], InboundTrunks, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec outbound_trunks(doc()) -> tristate_integer().
outbound_trunks(Doc) ->
    outbound_trunks(Doc, 0).

-spec outbound_trunks(doc(), Default) -> tristate_integer() | Default.
outbound_trunks(Doc, Default) ->
    get_limit_integer(<<"outbound_trunks">>, Doc, Default).

-spec set_outbound_trunks(doc(), tristate_integer()) -> doc().
set_outbound_trunks(Doc, OutboundTrunks) ->
    kz_json:set_value([<<"outbound_trunks">>], OutboundTrunks, Doc).

-spec set_pvt_outbound_trunks(doc(), tristate_integer()) -> doc().
set_pvt_outbound_trunks(Doc, OutboundTrunks) ->
    kz_json:set_value([<<"outbound_trunks">>], OutboundTrunks, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec twoway_trunks(doc()) -> tristate_integer().
twoway_trunks(Doc) ->
    twoway_trunks(Doc, 0).

-spec twoway_trunks(doc(), Default) -> tristate_integer() | Default.
twoway_trunks(Doc, Default) ->
    get_limit_integer(<<"twoway_trunks">>, Doc, Default).

-spec set_twoway_trunks(doc(), tristate_integer()) -> doc().
set_twoway_trunks(Doc, TwowayTrunks) ->
    kz_json:set_value([<<"twoway_trunks">>], TwowayTrunks, Doc).

-spec set_pvt_twoway_trunks(doc(), tristate_integer()) -> doc().
set_pvt_twoway_trunks(Doc, TwowayTrunks) ->
    kz_json:set_value([<<"twoway_trunks">>], TwowayTrunks, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bundled_inbound_trunks(doc()) -> non_neg_integer().
bundled_inbound_trunks(Doc) ->
    AccountDb = kz_doc:account_db(Doc),
    bundled_inbound_trunks(Doc, AccountDb).

-spec bundled_inbound_trunks(doc(), kz_term:ne_binary()) -> non_neg_integer().
bundled_inbound_trunks(Doc, AccountDb) ->
    get_bundled_inbound_limit(AccountDb, Doc).

-spec set_pvt_bundled_inbound_trunks(doc(), kz_term:ne_binary()) -> doc().
set_pvt_bundled_inbound_trunks(Doc, InboundBundle) ->
    kz_json:set_value(<<"pvt_bundled_inbound_trunks">>, InboundBundle, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bundled_outbound_trunks(doc()) -> non_neg_integer().
bundled_outbound_trunks(Doc) ->
    AccountDb = kz_doc:account_db(Doc),
    bundled_outbound_trunks(Doc, AccountDb).

-spec bundled_outbound_trunks(doc(), kz_term:ne_binary()) -> non_neg_integer().
bundled_outbound_trunks(Doc, AccountDb) ->
    get_bundled_outbound_limit(AccountDb, Doc).

-spec set_pvt_bundled_outbound_trunks(doc(), kz_term:ne_binary()) -> doc().
set_pvt_bundled_outbound_trunks(Doc, OutboundBundle) ->
    kz_json:set_value(<<"pvt_bundled_outbound_trunks">>, OutboundBundle, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bundled_twoway_trunks(doc()) -> non_neg_integer().
bundled_twoway_trunks(Doc) ->
    AccountDb = kz_doc:account_db(Doc),
    bundled_twoway_trunks(Doc, AccountDb).

-spec bundled_twoway_trunks(doc(), kz_term:ne_binary()) -> non_neg_integer().
bundled_twoway_trunks(Doc, AccountDb) ->
    get_bundled_twoway_limit(AccountDb, Doc).

-spec set_pvt_bundled_twoway_trunks(doc(), kz_term:ne_binary()) -> doc().
set_pvt_bundled_twoway_trunks(Doc, TwowayBundle) ->
    kz_json:set_value(<<"pvt_bundled_twoway_trunks">>, TwowayBundle, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec burst_trunks(doc()) -> tristate_integer().
burst_trunks(Doc) ->
    burst_trunks(Doc, 0).

-spec burst_trunks(doc(), Default) -> tristate_integer() | Default.
burst_trunks(Doc, Default) ->
    get_limit_integer(<<"burst_trunks">>, Doc, Default).

-spec set_burst_trunks(doc(), tristate_integer()) -> doc().
set_burst_trunks(Doc, BurstTrunks) ->
    kz_json:set_value([<<"burst_trunks">>], BurstTrunks, Doc).

-spec set_pvt_burst_trunks(doc(), tristate_integer()) -> doc().
set_pvt_burst_trunks(Doc, BurstTrunks) ->
    kz_json:set_value([<<"burst_trunks">>], BurstTrunks, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec max_postpay_units(doc()) -> kz_currency:units().
max_postpay_units(Doc) ->
    max_postpay_units(Doc, 0).

-spec max_postpay_units(doc(), Default) -> kz_currency:units() | Default.
max_postpay_units(Doc, Default) ->
    get_limit_units(<<"max_postpay_amount">>, Doc, Default).

-spec set_pvt_max_postpay_units(doc(), kz_currency:units()) -> doc().
set_pvt_max_postpay_units(Doc, Units) ->
    kz_json:set_value(<<"pvt_max_postpay_amount">>, Units, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec max_postpay_dollars(doc()) -> kz_currency:dollars().
max_postpay_dollars(Doc) ->
    max_postpay_dollars(Doc, 0).

-spec max_postpay_dollars(doc(), Default) -> kz_currency:dollars() | Default.
max_postpay_dollars(Doc, Default) ->
    get_limit_dollars(<<"max_postpay_amount">>, Doc, Default).

-spec set_pvt_max_postpay_dollars(doc(), kz_currency:dollars()) -> doc().
set_pvt_max_postpay_dollars(Doc, Dollars) ->
    Units = kz_currency:dollars_to_units(Dollars),
    kz_json:set_value(<<"pvt_max_postpay_amount">>, Units, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec reserve_units(doc()) -> kz_currency:units().
reserve_units(Doc) ->
    reserve_units(Doc, 0).

-spec reserve_units(doc(), Default) -> kz_currency:units() | Default.
reserve_units(Doc, Default) ->
    get_limit_units(<<"reserve_amount">>, Doc, Default).

-spec set_pvt_reserve_units(doc(), kz_currency:units()) -> doc().
set_pvt_reserve_units(Doc, Units) ->
    kz_json:set_value(<<"pvt_reserve_amount">>, Units, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec reserve_dollars(doc()) -> kz_currency:dollars().
reserve_dollars(Doc) ->
    reserve_dollars(Doc, 0).

-spec reserve_dollars(doc(), Default) -> kz_currency:dollars() | Default.
reserve_dollars(Doc, Default) ->
    get_limit_dollars(<<"reserve_amount">>, Doc, Default).

-spec set_pvt_reserve_dollars(doc(), kz_currency:dollars()) -> doc().
set_pvt_reserve_dollars(Doc, Dollars) ->
    Units = kz_currency:dollars_to_units(Dollars),
    kz_json:set_value(<<"pvt_reserve_amount">>, Units, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec allow_prepay(doc()) -> boolean().
allow_prepay(Doc) ->
    allow_prepay(Doc, 'true').

-spec allow_prepay(doc(), Default) -> boolean() | Default.
allow_prepay(Doc, Default) ->
    get_limit_boolean(<<"allow_prepay">>, Doc, Default).

-spec set_allow_prepay(doc(), boolean()) -> doc().
set_allow_prepay(Doc, AllowPrepay) ->
    kz_json:set_value(<<"allow_prepay">>, kz_term:is_true(AllowPrepay), Doc).

-spec set_pvt_allow_prepay(doc(), boolean()) -> doc().
set_pvt_allow_prepay(Doc, AllowPrepay) ->
    kz_json:set_value(<<"pvt_allow_prepay">>, kz_term:is_true(AllowPrepay), Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec allow_postpay(doc()) -> boolean().
allow_postpay(Doc) ->
    allow_postpay(Doc, 'false').

-spec allow_postpay(doc(), Default) -> boolean() | Default.
allow_postpay(Doc, Default) ->
    get_limit_boolean(<<"allow_postpay">>, Doc, Default).

-spec set_pvt_allow_postpay(doc(), boolean()) -> doc().
set_pvt_allow_postpay(Doc, AllowPostpay) ->
    kz_json:set_value(<<"pvt_allow_postpay">>, kz_term:is_true(AllowPostpay), Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec soft_limit_inbound(doc()) -> boolean().
soft_limit_inbound(Doc) ->
    soft_limit_inbound(Doc, 'false').

-spec soft_limit_inbound(doc(), Default) -> boolean() | Default.
soft_limit_inbound(Doc, Default) ->
    get_limit_boolean(<<"soft_limit_inbound">>, Doc, Default).

-spec set_pvt_soft_limit_inbound(doc(), boolean()) -> doc().
set_pvt_soft_limit_inbound(Doc, SoftLimit) ->
    kz_json:set_value(<<"pvt_soft_limit_inbound">>, kz_term:is_true(SoftLimit), Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec soft_limit_outbound(doc()) -> boolean().
soft_limit_outbound(Doc) ->
    soft_limit_outbound(Doc, 'false').

-spec soft_limit_outbound(doc(), Default) -> boolean() | Default.
soft_limit_outbound(Doc, Default) ->
    get_limit_boolean(<<"soft_limit_outbound">>, Doc, Default).

-spec set_pvt_soft_limit_outbound(doc(), boolean()) -> doc().
set_pvt_soft_limit_outbound(Doc, SoftLimit) ->
    kz_json:set_value(<<"pvt_soft_limit_outbound">>, kz_term:is_true(SoftLimit), Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec authz_resource_types(doc()) -> list().
authz_resource_types(Doc) ->
    authz_resource_types(Doc, []).

-spec authz_resource_types(doc(), Default) -> list() | Default.
authz_resource_types(Doc, Default) ->
    get_limit_list(<<"authz_resource_types">>, Doc, Default).

-spec set_authz_resource_types(doc(), list()) -> doc().
set_authz_resource_types(Doc, Types) ->
    kz_json:set_value(<<"authz_resource_types">>, Types, Doc).

-spec set_pvt_authz_resource_types(doc(), list()) -> doc().
set_pvt_authz_resource_types(Doc, Types) ->
    kz_json:set_value(<<"pvt_authz_resource_types">>, Types, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec allotments(doc()) -> kz_json:object().
allotments(Doc) ->
    allotments(Doc, kz_json:new()).

-spec allotments(doc(), Default) -> kz_json:object() | Default.
allotments(Doc, Default) ->
    kz_json:get_ne_json_value(<<"allotments">>, Doc, Default).

-spec set_allotments(doc(), kz_json:object()) -> doc().
set_allotments(Doc, Allotments) ->
    kz_json:set_value(<<"allotments">>, Allotments, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec inbound_channels_per_did_rules(doc()) -> kz_json:object().
inbound_channels_per_did_rules(Doc) ->
    inbound_channels_per_did_rules(Doc, kz_json:new()).

-spec inbound_channels_per_did_rules(doc(), Default) -> kz_json:object() | Default.
inbound_channels_per_did_rules(Doc, Default) ->
    kz_json:get_ne_json_value(<<"pvt_inbound_channels_per_did_rules">>, Doc, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_limit(kz_term:ne_binary(), kz_json:object(), tristate_integer()) ->
          tristate_integer().
get_limit(Key, Doc, Default) ->
    PrivateValue = get_private_limit(Key, Doc),
    PublicValue =  kz_json:get_integer_value(Key, Doc),
    case PrivateValue =/= 'undefined'
        andalso
        (PublicValue =:= 'undefined'
         orelse PublicValue < 0
         orelse (
           PrivateValue >= 0
           andalso PrivateValue < PublicValue
          )
        )
    of
        'true' -> PrivateValue;
        'false' -> get_public_limit(Key, Doc, Default)
    end.

-spec get_public_limit(kz_term:ne_binary(), kz_json:object(), tristate_integer()) ->
          non_neg_integer().
get_public_limit(Key, Doc, Default) ->
    case kz_json:get_integer_value(Key, Doc) of
        'undefined' -> get_default_limit(Key, Default);
        Value when Value < 0 -> get_default_limit(Key, Default);
        Value -> Value
    end.

-spec get_private_limit(kz_term:ne_binary(), kz_json:object()) -> tristate_integer().
get_private_limit(Key, Doc) ->
    kz_json:get_integer_value(<<"pvt_", Key/binary>>, Doc).

-spec get_default_limit(kz_term:ne_binary(), tristate_integer()) -> tristate_integer().
get_default_limit(Key, Default) ->
    kapps_config:get_integer(?LIMITS_CAT, Key, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_limit_integer(kz_term:ne_binary(), kz_json:object(), integer()) -> integer().
get_limit_integer(Key, Doc, Default) ->
    case kz_json:get_value(<<"pvt_", Key/binary>>, Doc) of
        'undefined' -> get_public_limit_integer(Key, Doc, Default);
        0 -> 0;
        Value when Value < 0 ->
            get_public_limit_integer(Key, Doc, Default);
        Value ->
            enforce_integer_limit(Value
                                 ,get_public_limit_integer(Key, Doc, Default)
                                 )
    end.

-spec get_public_limit_integer(kz_term:ne_binary(), kz_json:object(), integer()) -> integer().
get_public_limit_integer(Key, Doc, Default) ->
    case kz_json:get_value(Key, Doc) of
        'undefined' -> get_default_limit_integer(Key, Default);
        Value -> Value
    end.

-spec get_default_limit_integer(kz_term:ne_binary(), integer()) -> integer().
get_default_limit_integer(Key, Default) ->
    kapps_config:get_integer(?LIMITS_CAT, Key, Default).

-spec enforce_integer_limit(integer(), integer()) -> integer().
enforce_integer_limit(Private, Public) ->
    case Private > Public of
        'true' -> Public;
        'false' -> Private
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_limit_units(kz_term:ne_binary(), kz_json:object(), kz_currency:units()) -> kz_currency:units().
get_limit_units(Key, Doc, Default) ->
    case kz_json:get_value(<<"pvt_", Key/binary>>, Doc) of
        Value when is_float(Value) -> kz_currency:dollars_to_units(abs(Value));
        Value when is_integer(Value) -> abs(Value);
        _Else -> get_default_limit_units(Key, Default)
    end.

-spec get_default_limit_units(kz_term:ne_binary(), kz_currency:units()) -> kz_currency:units().
get_default_limit_units(Key, Default) ->
    case kapps_config:get(?LIMITS_CAT, Key, Default) of
        Value when is_float(Value) -> kz_currency:dollars_to_units(abs(Value));
        Value when is_integer(Value) -> abs(Value);
        Default when is_float(Default) -> kz_currency:dollars_to_units(abs(Default));
        Default when is_integer(Default) -> abs(Default);
        Default -> Default
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_limit_dollars(kz_term:ne_binary(), kz_json:object(), kz_currency:dollars()) -> kz_currency:dollars().
get_limit_dollars(Key, Doc, Default) ->
    case kz_json:get_value(<<"pvt_", Key/binary>>, Doc) of
        Value when is_float(Value) -> abs(Value);
        Value when is_integer(Value) -> kz_currency:units_to_dollars(abs(Value));
        _Else -> get_default_limit_dollars(Key, Default)
    end.

-spec get_default_limit_dollars(kz_term:ne_binary(), kz_currency:dollars()) -> kz_currency:dollars().
get_default_limit_dollars(Key, Default) ->
    case kapps_config:get(?LIMITS_CAT, Key, Default) of
        Value when is_float(Value) -> abs(Value);
        Value when is_integer(Value) -> kz_currency:units_to_dollars(abs(Value));
        Default when is_float(Default) -> abs(Default);
        Default when is_integer(Default) -> kz_currency:units_to_dollars(abs(Default));
        Default -> Default
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_limit_boolean(kz_term:ne_binary(), kz_json:object(), boolean()) -> boolean().
get_limit_boolean(Key, Doc, Default) ->
    case kz_json:get_value(<<"pvt_", Key/binary>>, Doc) of
        'undefined' -> get_public_limit_boolean(Key, Doc, Default);
        Value -> kz_term:is_true(Value)
    end.

-spec get_public_limit_boolean(kz_term:ne_binary(), kz_json:object(), boolean()) -> boolean().
%% NOTE: all other booleans (inbound_soft_limit, allow_postpay, etc) should
%%  not be made public via this helper.
get_public_limit_boolean(<<"allow_prepay">> = Key, Doc, Default) ->
    case kz_json:get_value(Key, Doc) of
        'undefined' -> get_default_limit_boolean(Key, Default);
        Value -> kz_term:is_true(Value)
    end;
get_public_limit_boolean(Key, _, Default) ->
    get_default_limit_boolean(Key, Default).

-spec get_default_limit_boolean(kz_term:ne_binary(), boolean()) -> boolean().
get_default_limit_boolean(Key, Default) ->
    kapps_config:get_is_true(?LIMITS_CAT, Key, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_limit_list(kz_term:ne_binary(), kz_json:object(), list()) -> list().
get_limit_list(Key, JObj, Default) ->
    case kz_json:get_list_value(<<"pvt_", Key/binary>>, JObj) of
        'undefined' ->
            get_public_limit_list(Key, JObj, Default);
        Value -> Value
    end.

-spec get_public_limit_list(kz_term:ne_binary(), kz_json:object(), list()) -> list().
get_public_limit_list(Key, JObj, Default) ->
    case kz_json:get_list_value(Key, JObj, Default) of
        'undefined' -> get_default_limit_list(Key, Default);
        Value -> Value
    end.

-spec get_default_limit_list(kz_term:ne_binary(), list()) -> list().
get_default_limit_list(Key, Default) ->
    kapps_config:get_ne_binaries(?LIMITS_CAT, Key, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_bundled_inbound_limit(kz_term:ne_binary(), kz_json:object()) -> non_neg_integer().
get_bundled_inbound_limit(AccountDb, JObj) ->
    case kz_json:get_ne_value(<<"pvt_bundled_inbound_trunks">>, JObj) of
        'undefined' -> 0;
        Type ->
            View = <<Type/binary, "/crossbar_listing">>,
            get_bundled_limit(AccountDb, View)
    end.

-spec get_bundled_outbound_limit(kz_term:ne_binary(), kz_json:object()) -> non_neg_integer().
get_bundled_outbound_limit(AccountDb, JObj) ->
    case kz_json:get_ne_value(<<"pvt_bundled_outbound_trunks">>, JObj) of
        'undefined' -> 0;
        Type ->
            View = <<Type/binary, "/crossbar_listing">>,
            get_bundled_limit(AccountDb, View)
    end.

-spec get_bundled_twoway_limit(kz_term:ne_binary(), kz_json:object()) -> non_neg_integer().
get_bundled_twoway_limit(AccountDb, JObj) ->
    case kz_json:get_ne_value(<<"pvt_bundled_twoway_trunks">>, JObj) of
        'undefined' -> 0;
        Type ->
            View = <<Type/binary, "/crossbar_listing">>,
            get_bundled_limit(AccountDb, View)
    end.

-spec get_bundled_limit(kz_term:ne_binary(), kz_term:ne_binary()) -> non_neg_integer().
get_bundled_limit(AccountDb, View) ->
    case kz_datamgr:get_results(AccountDb, View, []) of
        {'ok', JObjs} -> filter_bundled_limit(JObjs);
        {'error', _R} ->
            lager:debug("failed get bundled limit from ~s in ~s: ~p"
                       ,[View, AccountDb, _R]),
            0
    end.

-spec filter_bundled_limit(kz_json:objects()) -> non_neg_integer().
filter_bundled_limit(JObjs) ->
    length([JObj
            || JObj <- JObjs
                   ,kz_json:is_true([<<"value">>, <<"enabled">>]
                                   ,JObj
                                   ,'true')
           ]).
