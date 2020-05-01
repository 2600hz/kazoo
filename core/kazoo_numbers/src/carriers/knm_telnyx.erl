%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc A Number Manager module for carrier: telnyx.com
%%% @author Pierre Fenoll
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_telnyx).
-behaviour(knm_gen_carrier).

-export([info/0]).
-export([is_local/0]).
-export([find_numbers/3]).
-export([acquire_number/1]).
-export([disconnect_number/1]).
-export([is_number_billable/1]).
-export([should_lookup_cnam/0]).
-export([check_numbers/1]).

-include("knm.hrl").

-define(MOD_CONFIG_CAT, <<(?KNM_CONFIG_CAT)/binary, ".telnyx">>).

-define(IS_SANDBOX_PROVISIONING_TRUE
       ,kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"sandbox_provisioning">>, 'false')
       ).
-define(IS_PROVISIONING_ENABLED
       ,kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"enable_provisioning">>, 'true')
       ).

%%% API

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec info() -> map().
info() ->
    #{?CARRIER_INFO_MAX_PREFIX => 3
     }.

%%------------------------------------------------------------------------------
%% @doc Is this carrier handling numbers local to the system?
%%
%% <div class="notice">A non-local (foreign) carrier module makes HTTP requests.</div>
%% @end
%%------------------------------------------------------------------------------
-spec is_local() -> boolean().
is_local() -> 'false'.

-spec is_number_billable(knm_phone_number:record()) -> 'true'.
is_number_billable(_PN) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc Check with carrier if these numbers are registered with it.
%% @end
%%------------------------------------------------------------------------------
-spec check_numbers(kz_term:ne_binaries()) -> {'ok', kz_json:object()} |
          {'error', any()}.
check_numbers(_Numbers) -> {'error', 'not_implemented'}.


%%------------------------------------------------------------------------------
%% @doc Query the system for a quantity of available numbers in a rate center
%% @end
%%------------------------------------------------------------------------------
-spec find_numbers(kz_term:ne_binary(), pos_integer(), knm_search:options()) ->
          {'ok', knm_search:results()}.
find_numbers(<<"+1", Prefix:3/binary, _/binary>>, Quantity, Options)
  when ?IS_US_TOLLFREE(Prefix) ->
    Results = search_numbers('tollfree', Quantity, Prefix, 'undefined'),
    {'ok', format_search_numbers_resp(Results, Options)};

find_numbers(<<"+1", NPA:3/binary, _/binary>>=Num, Quantity, Options) ->
    NXX = case byte_size(Num) >= 2+3+3 of
              'true' -> binary:part(Num, 2+3, 3);
              'false' -> 'undefined'
          end,
    Results = search_numbers('npa', Quantity, NPA, NXX),
    {'ok', format_search_numbers_resp(Results, Options)};

find_numbers(<<"+",_/binary>>=_InternationalNum, Quantity, Options) ->
    Country = knm_search:country(Options),
    Results = search_numbers('region', Quantity, Country, 'undefined'),
    {'ok', format_international_numbers_search_resp(Results, Options)}.

%%------------------------------------------------------------------------------
%% @doc Acquire a given number from the carrier
%% @end
%%------------------------------------------------------------------------------
-spec acquire_number(knm_phone_number:record()) -> knm_phone_number:record().
acquire_number(PN) ->
    Debug = ?IS_SANDBOX_PROVISIONING_TRUE,
    case ?IS_PROVISIONING_ENABLED of
        'false' when Debug ->
            lager:debug("allowing sandbox provisioning"),
            PN;
        'false' ->
            knm_errors:unspecified('provisioning_disabled', PN);
        'true' ->
            Num = knm_phone_number:number(PN),
            Req = kz_json:from_list([{<<"requested_numbers">>, [Num]}
                                    ]),
            Rep = knm_telnyx_util:req('post', ["number_orders"], Req),
            case kz_json:get_ne_binary_value(<<"id">>, Rep) of
                'undefined' ->
                    lager:debug("order failure: ~s", [kz_json:encode(Rep)]),
                    Reason = kz_json:get_ne_binary_value(<<"message">>, Rep),
                    knm_errors:by_carrier(?MODULE, Reason, Num);
                OrderId ->
                    Data = kz_json:from_list([{<<"order_id">>, OrderId}]),
                    knm_phone_number:update_carrier_data(PN, Data)
            end
    end.

%%------------------------------------------------------------------------------
%% @doc Release a number from the routing table
%% @end
%%------------------------------------------------------------------------------
-spec disconnect_number(knm_phone_number:record()) -> knm_phone_number:record().
disconnect_number(PN) ->
    Debug = ?IS_SANDBOX_PROVISIONING_TRUE,
    case ?IS_PROVISIONING_ENABLED of
        'true' -> PN;
        'false' when Debug ->
            lager:debug("allowing sandbox provisioning"),
            PN;
        'false' ->
            knm_errors:unspecified('provisioning_disabled', PN)
    end.

-spec should_lookup_cnam() -> boolean().
should_lookup_cnam() -> 'true'.


%%% Internals

-type kind() :: 'npa' | 'tollfree' | 'region'.
-spec search_numbers(kind(), pos_integer(), kz_term:ne_binary(), kz_term:api_ne_binary()) ->
          kz_json:objects().
search_numbers(SearchKind, Quantity, Prefix, NXX) ->
    Descriptor = kz_json:from_list(search_prefix(SearchKind, Prefix, NXX)),
    Req = kz_json:from_list(
            [{<<"search_type">>, search_kind(SearchKind)}
            ,{<<"search_descriptor">>, Descriptor}
            ,{<<"limit">>, Quantity}
            ,{<<"with_result">>, 'true'}
            ]),
    Rep = knm_telnyx_util:req('post', ["number_searches"], Req),
    case SearchKind of
        'region' -> kz_json:get_value(<<"inexplicit_result">>, Rep);
        _ -> kz_json:get_value(<<"result">>, Rep)
    end.

-spec format_search_numbers_resp(kz_json:objects(), knm_search:options()) -> knm_search:results().
format_search_numbers_resp(JObjs, Options) ->
    QID = knm_search:query_id(Options),
    [{QID, {Num, ?MODULE, ?NUMBER_STATE_DISCOVERY, Data}}
     || Data <- JObjs,
        Num <- [kz_json:get_ne_binary_value(<<"number_e164">>, Data)]
    ].

-spec format_international_numbers_search_resp(kz_json:objects(), knm_search:options()) -> knm_search:results().
format_international_numbers_search_resp(JObjs, Options) ->
    Dialcode = knm_search:dialcode(Options),
    QID = knm_search:query_id(Options),
    [{QID, {Num, ?MODULE, ?NUMBER_STATE_DISCOVERY, Data}}
     || Data <- JObjs,
        Num0 <- [kz_json:get_ne_binary_value(<<"area_code">>, Data)],
        Num <- [ugly_hack(Dialcode, Num0)]
    ].

%%TODO: once Telnyx gives back real numbers, remove this.
%% Right now international search returns only prefixes.
ugly_hack(Dialcode, Num) ->
    kz_binary:pad(<<Dialcode/binary, Num/binary>>, 9, <<"0">>).

search_kind('npa') -> 1;
search_kind('region') -> 2;
search_kind('tollfree') -> 3.

search_prefix('tollfree', Prefix, _) ->
    [{<<"prefix">>, Prefix}];
search_prefix('region', Country, _) ->
    [{<<"country_iso">>, Country}];
search_prefix('npa', NPA, 'undefined') ->
    [{<<"npa">>, NPA}];
search_prefix('npa', NPA, NXX) ->
    [{<<"nxx">>, NXX}
     |search_prefix('npa', NPA, 'undefined')
    ].

%%% End of Module
