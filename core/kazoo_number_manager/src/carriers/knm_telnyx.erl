%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%%
%%% A Number Manager module for carrier: telnyx.com
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_telnyx).
-behaviour(knm_gen_carrier).

-export([is_local/0]).
-export([find_numbers/3]).
-export([acquire_number/1]).
-export([disconnect_number/1]).
-export([is_number_billable/1]).
-export([should_lookup_cnam/0]).

-include("knm.hrl").

-define(MOD_CONFIG_CAT, <<(?KNM_CONFIG_CAT)/binary, ".telnyx">>).

-define(IS_SANDBOX_PROVISIONING_TRUE,
        kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"sandbox_provisioning">>, 'false')).
-define(IS_PROVISIONING_ENABLED,
        kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"enable_provisioning">>, 'true')).


%%% API

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Is this carrier handling numbers local to the system?
%% Note: a non-local (foreign) carrier module makes HTTP requests.
%% @end
%%--------------------------------------------------------------------
-spec is_local() -> boolean().
is_local() -> 'false'.

%% @public
-spec is_number_billable(knm_number:knm_number()) -> boolean().
is_number_billable(_Number) -> 'true'.


%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the system for a quantity of available numbers in a rate center
%% @end
%%--------------------------------------------------------------------
-spec find_numbers(ne_binary(), pos_integer(), knm_carriers:options()) ->
                          {'ok', knm_number:knm_numbers()}.
find_numbers(<<"+1", Prefix:3/binary, _/binary>>, Quantity, Options)
  when ?IS_US_TOLLFREE(Prefix) ->
    Results = numbers('tollfree', Quantity, Prefix, 'undefined'),
    {'ok', numbers(Results, Options)};

find_numbers(<<"+1", NPA:3/binary, _/binary>>=Num, Quantity, Options) ->
    NXX = case byte_size(Num) >= 2+3+3 of
              'true' -> binary:part(Num, 2+3, 3);
              'false' -> 'undefined'
          end,
    Results = numbers('npa', Quantity, NPA, NXX),
    {'ok', numbers(Results, Options)};

find_numbers(<<"+",_/binary>>=_InternationalNum, Quantity, Options) ->
    Country = knm_carriers:country(Options),
    Results = numbers('region', Quantity, Country, 'undefined'),
    {'ok', international_numbers(Results, Options)}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Acquire a given number from the carrier
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Release a number from the routing table
%% @end
%%--------------------------------------------------------------------
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

%% @public
-spec should_lookup_cnam() -> boolean().
should_lookup_cnam() -> 'true'.


%%% Internals

-spec numbers('npa' | 'tollfree', pos_integer(), ne_binary(), api_ne_binary()) ->
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

-spec numbers(kz_json:objects(), knm_carriers:options()) -> knm_number:knm_numbers().
numbers(JObjs, Options) ->
    AccountId = knm_carriers:account_id(Options),
    [Number
     || Data <- JObjs,
        Num <- [kz_json:get_ne_binary_value(<<"number_e164">>, Data)],
        {'ok', Number} <- [knm_carriers:create_found(Num, ?MODULE, AccountId, Data)]
    ].

-spec international_numbers(kz_json:objects(), knm_carriers:options()) -> knm_number:knm_numbers().
international_numbers(JObjs, Options) ->
    AccountId = knm_carriers:account_id(Options),
    Dialcode = knm_carriers:dialcode(Options),
    [Number
     || Data <- JObjs,
        Num0 <- [kz_json:get_ne_binary_value(<<"area_code">>, Data)],
        Num <- [ugly_hack(Dialcode, Num0)],
        {'ok', Number} <- [knm_carriers:create_found(Num, ?MODULE, AccountId, Data)]
    ].

%%TODO: once Telnyx gives back real numbers, remove this.
%% Right now international search returns only prefixes.
ugly_hack(Dialcode, Num) ->
    kz_util:pad_binary(<<Dialcode/binary, Num/binary>>, 9, <<"0">>).

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
