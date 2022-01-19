%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2022, 2600Hz
%%% @doc A Number Manager module for carrier: telnyx.com
%%% @author Pierre Fenoll
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
-include("knm_telnyx.hrl").

-define(MOD_CONFIG_CAT, <<(?KNM_CONFIG_CAT)/binary, ".telnyx">>).

-define(IS_SANDBOX_PROVISIONING_TRUE
       ,kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"sandbox_provisioning">>, 'false')
       ).
-define(IS_PROVISIONING_ENABLED
       ,kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"enable_provisioning">>, 'true')
       ).
-define(SHOULD_KEEP_BEST_EFFORT
       ,kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"should_keep_best_effort">>, 'false')
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

-spec is_number_billable(knm_phone_number:knm_phone_number()) -> 'true'.
is_number_billable(_Number) -> 'true'.

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
          {'ok', knm_number:knm_numbers()}.
find_numbers(<<"+1", NPA:3/binary, _/binary>>=Num, Quantity, Options)
  when ?IS_US_TOLLFREE(NPA) ->
    Results = numbers('tollfree', Quantity, NPA, get_nxx(Num)),
    {'ok', numbers(Results, Options)};

find_numbers(<<"+1", NPA:3/binary, _/binary>>=Num, Quantity, Options) ->
    Results = numbers('npa', Quantity, NPA, get_nxx(Num)),
    {'ok', numbers(Results, Options)};

find_numbers(<<"+",_/binary>>=_InternationalNum, Quantity, Options) ->
    Country = knm_search:country(Options),
    Results = numbers('region', Quantity, Country, 'undefined'),
    {'ok', international_numbers(Results, Options)}.

%%------------------------------------------------------------------------------
%% @doc For a given Number, parse out the NXX value.
%% Return `undefined' if the NXX value can not be parsed.
%% @end
%%------------------------------------------------------------------------------
-spec get_nxx(kz_term:ne_binary()) -> kz_term:api_ne_binary().
get_nxx(<<_PlusOneAreaCode:5/binary, NXX:3/binary, _/binary>>) -> NXX;
get_nxx(_) -> 'undefined'.

%%------------------------------------------------------------------------------
%% @doc Acquire a given number from the carrier
%% @end
%%------------------------------------------------------------------------------
-spec acquire_number(knm_number:knm_number()) -> knm_number:knm_number().
acquire_number(Number) ->
    Debug = ?IS_SANDBOX_PROVISIONING_TRUE,
    case ?IS_PROVISIONING_ENABLED of
        'false' when Debug ->
            lager:debug("allowing sandbox provisioning"),
            Number;
        'false' ->
            knm_errors:unspecified('provisioning_disabled', Number);
        'true' ->
            PhoneNumber = knm_number:phone_number(Number),
            Num = knm_phone_number:number(PhoneNumber),
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
                    PN = knm_phone_number:update_carrier_data(PhoneNumber, Data),
                    knm_number:set_phone_number(Number, PN)
            end
    end.

%%------------------------------------------------------------------------------
%% @doc Release a number from the routing table
%% @end
%%------------------------------------------------------------------------------
-spec disconnect_number(knm_number:knm_number()) -> knm_number:knm_number().
disconnect_number(Number) ->
    Debug = ?IS_SANDBOX_PROVISIONING_TRUE,
    case ?IS_PROVISIONING_ENABLED of
        'true' -> Number;
        'false' when Debug ->
            lager:debug("allowing sandbox provisioning"),
            Number;
        'false' ->
            knm_errors:unspecified('provisioning_disabled', Number)
    end.

-spec should_lookup_cnam() -> boolean().
should_lookup_cnam() -> 'true'.


%%% Internals

-type kind() :: 'npa' | 'tollfree' | 'region'.
-spec numbers(kind(), pos_integer(), kz_term:ne_binary(), kz_term:api_ne_binary()) ->
          kz_json:objects().
numbers(SearchKind, Quantity, Prefix, NXX) ->
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

numbers(JObjs, Options) ->
    QID = knm_search:query_id(Options),
    [{QID, {Num, ?MODULE, ?NUMBER_STATE_DISCOVERY, Data}}
     || Data <- JObjs,
        Num <- [kz_json:get_ne_binary_value(<<"number_e164">>, Data)]
    ].

international_numbers(JObjs, Options) ->
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

-spec search_kind(kind()) -> integer().
search_kind('npa') -> ?SEARCH_TYPE_NPA;
search_kind('region') -> ?SEARCH_TYPE_INTERNATIONAL;
search_kind('tollfree') -> ?SEARCH_TYPE_TOLEFREE.

-spec search_prefix(kind(), kz_term:ne_binary(), kz_term:api_ne_binary()) -> kz_json:json_proplist().
search_prefix('tollfree', NPA, 'undefined') ->
    [{<<"npa">>, NPA}
    ,should_keep_best_effort()
    ];
search_prefix('tollfree', NPA, NXX) ->
    [{<<"nxx">>, NXX}
    ,should_keep_best_effort()
     |search_prefix('tollfree', NPA, 'undefined')
    ];
search_prefix('region', Country, _) ->
    [{<<"country_iso">>, Country}
    ,should_keep_best_effort()
    ];
search_prefix('npa', NPA, 'undefined') ->
    [{<<"npa">>, NPA}
    ,should_keep_best_effort()
    ];
search_prefix('npa', NPA, NXX) ->
    [{<<"nxx">>, NXX}
    ,should_keep_best_effort()
     |search_prefix('npa', NPA, 'undefined')
    ].

-spec should_keep_best_effort() -> {kz_term:ne_binary(), boolean()}.
should_keep_best_effort() -> {<<"best_effort">>, ?SHOULD_KEEP_BEST_EFFORT}.
%%% End of Module
