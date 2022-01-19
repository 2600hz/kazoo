%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2022, 2600Hz
%%% @doc When implementing carrier modules, these callbacks are a must!
%%% @author Karl Anderson
%%% @author Pierre Fenoll
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_gen_carrier).

-include("knm.hrl").

-callback find_numbers(kz_term:ne_binary(), pos_integer(), knm_carriers:options()) ->
    {'ok', knm_number:knm_numbers()} |
    {'bulk', knm_number:knm_numbers()} |
    {'error', any()}.

-callback acquire_number(knm_number:knm_number()) ->
    knm_number:knm_number() | no_return().

-callback disconnect_number(knm_number:knm_number()) ->
    knm_number:knm_number() | no_return().

-callback should_lookup_cnam() ->
    boolean().

-callback is_number_billable(knm_phone_number:knm_phone_number()) ->
    boolean().

-callback is_local() ->
    boolean().

-callback check_numbers(kz_term:ne_binaries()) ->
    {'ok', kz_json:object()} |
    {'error', any()}.

-callback configuration() -> kz_json:object().

-optional_callbacks([configuration/0]).

-type carrier() :: kz_term:api_ne_binary() | knm_phone_number:knm_phone_number().

-export_type([carrier/0]).

-export([configuration/1]).

-spec configuration(carrier()) -> kz_json:object().
configuration(Carrier) ->
    try carrier_callback(Carrier, 'configuration', []) of
        'no_callback' -> default_config(Carrier);
        Value ->
            case kz_json:is_json_object(Value) of
                'true' -> Value;
                'false' -> default_config(Carrier)
            end
    catch
        _ -> kz_json:new()
    end.

default_config(Carrier) ->
    Category = list_to_binary([?KNM_CONFIG_CAT, ".", carrier_config(Carrier)]),
    case kapps_config:get_category(Category) of
        {'ok', JObj} ->
            Default = kz_json:get_json_value(<<"default">>, JObj, kz_json:new()),
            kz_json:get_json_value(kz_term:to_binary(node()), JObj, Default);
        _ -> kz_json:new()
    end.

-spec carrier_config(carrier()) -> binary().
carrier_config(<<"knm_", Carrier/binary>>) ->
    Carrier;
carrier_config(Carrier)
  when is_binary(Carrier) ->
    Carrier;
carrier_config(PN)
  when is_tuple(PN) ->
    carrier_config(knm_phone_number:module_name(PN));
carrier_config(Carrier) ->
    kz_term:to_binary(Carrier).

-spec carrier_module(carrier()) -> module().
carrier_module(<<"knm_", _/binary>> = Carrier) ->
    kz_term:to_atom(Carrier, 'true');
carrier_module(Carrier)
  when is_binary(Carrier) ->
    kz_term:to_atom(<<"knm_", Carrier/binary>>, 'true');
carrier_module(PN)
  when is_tuple(PN) ->
    carrier_module(knm_phone_number:module_name(PN)).

-spec carrier_callback(carrier(), atom(), [term()]) -> 'no_callback' | term().
carrier_callback(Carrier, Function, Args) ->
    Module = carrier_module(Carrier),
    case kz_module:is_exported(Module, Function, length(Args)) of
        'true' -> erlang:apply(Module, Function, Args);
        'false' -> 'no_callback'
    end.
