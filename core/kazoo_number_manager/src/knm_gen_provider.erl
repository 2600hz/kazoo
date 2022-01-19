%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2022, 2600Hz
%%% @doc When implementing provider modules, these callbacks are a must!
%%% @author Pierre Fenoll
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_gen_provider).

-include("knm.hrl").

-callback save(knm_number:knm_number()) ->
    knm_number:knm_number().

-callback delete(knm_number:knm_number()) ->
    knm_number:knm_number().

-callback available(knm_phone_number:knm_phone_number()) -> boolean().
-callback available(kz_term:ne_binary() | module(), kz_term:ne_binary(), kz_term:api_ne_binary()) -> boolean().

-callback settings(knm_phone_number:knm_phone_number()) -> kz_json:object().

-optional_callbacks([available/1
                    ,available/3
                    ,settings/1
                    ]).

-export([available/2, available/4
        ,settings/2
        ]).

-spec available(kz_term:ne_binary() | module(), knm_phone_number:knm_phone_number()) -> boolean().
available(Feature, Number) ->
    try feature_callback(Feature, available, [Number]) of
        no_module -> false;
        no_callback -> true;
        Value -> kz_term:is_true(Value)
    catch
        _ -> true
    end.

-spec available(kz_term:ne_binary() | module(), kz_term:ne_binary() | module(), kz_term:ne_binary(), kz_term:api_ne_binary()) -> boolean().
available(Feature, Carrier, State, AccountId) ->
    try feature_callback(Feature, available, [Carrier, State, AccountId]) of
        no_module -> false;
        no_callback -> true;
        Value -> kz_term:is_true(Value)
    catch
        _ -> true
    end.

-spec settings(kz_term:ne_binary() | module(), knm_phone_number:knm_phone_number()) -> kz_json:object().
settings(Feature, PN) ->
    try feature_callback(Feature, settings, [PN]) of
        no_callback -> kz_json:new();
        no_module -> kz_json:new();
        Value ->
            case kz_json:is_json_object(Value) of
                true -> Value;
                false -> kz_json:new()
            end
    catch
        _ -> kz_json:new()
    end.

-spec feature_module(kz_term:ne_binary() | module()) -> module().
feature_module(<<"knm_", _/binary>> = Feature) ->
    kz_term:to_atom(Feature, 'true');
feature_module(Feature)
  when is_binary(Feature) ->
    kz_term:to_atom(<<"knm_", Feature/binary>>, 'true');
feature_module(Feature) ->
    Feature.

-spec feature_callback(kz_term:ne_binary(), atom(), [term()]) -> 'no_callback' | term().
feature_callback(Feature, Function, Args) ->
    Module = feature_module(Feature),
    case kz_module:ensure_loaded(Module) of
        'false' -> 'no_module';
        Mod ->
            case kz_module:is_exported(Mod, Function, length(Args)) of
                'true' -> erlang:apply(Mod, Function, Args);
                'false' -> 'no_callback'
            end
    end.
