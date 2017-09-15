%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%% Sup commands for EDR backends
%%% @end
%%% @contributors
%%%    SIPLABS, LLC (Vorontsov Nikita) <info@siplabs.ru>
%%%    Conversant Ltd (Max Lay)
%%%-------------------------------------------------------------------
-module(edr_maintenance).

-include("edr.hrl").

%% TODO: Register, delete, enable, disable should start and stop backends
-export([registered_backends/0
        ,register_backend/4
        ,delete_backend/1
        ,enable_backend/1
        ,disable_backend/1
        ]).

-spec register_backend(ne_binary(), ne_binary(), kz_json:object() | ne_binary(), boolean() | ne_binary())-> 'ok' | {'error', 'already_registered'}.
register_backend(Name, Type, Opts, IsEnable) when is_binary(Opts) ->
   register_backend(Name, Type, kz_json:decode(Opts), IsEnable);
register_backend(Name, Type, Opts, IsEnable) ->
    %% TODO: Add provision for bindings
    Backends = registered_backends(),
    case [J || J <- Backends, match_backend(Name, J)] of
        [] ->
            Backend = kz_json:from_list([{<<"name">>, Name}
                                        ,{<<"options">>, Opts}
                                        ,{<<"type">>, Type}
                                        ,{<<"enabled">>, kz_term:is_true(IsEnable)}
                                        ,{<<"bindings">>, edr_bindings:bindings_to_json([#edr_binding{}])}
                                        ]),
            set_registered_backends([Backend | Backends]),
            case kz_term:is_true(IsEnable) of
                'true' ->
                    edr_backend_sup:start_backend(Name),
                    'ok';
                'false' ->
                    'ok'
            end;
        _V ->
            {'error', 'already_registered'}
    end.

-spec delete_backend(ne_binary())-> 'ok'.
delete_backend(Name) ->
    Backends = registered_backends(),
    case [J || J <- Backends, match_backend(Name, J)] of
        [] ->
            {'error', 'not_registered'};
        _ ->
            edr_backend_sup:stop_backend(Name),
            set_registered_backends([J || J <- Backends, not match_backend(Name, J)])
    end.

-spec enable_backend(ne_binary())-> 'ok' | {'error', 'not_registered'}.
enable_backend(Name) ->
    edr_backend_sup:start_backend(Name),
    modify_backend(Name, fun(Backend) -> kz_json:set_value(<<"enabled">>, 'true', Backend) end).

-spec disable_backend(ne_binary()) -> 'ok'.
disable_backend(Name)->
    edr_backend_sup:stop_backend(Name),
    modify_backend(Name, fun(Backend) -> kz_json:set_value(<<"enabled">>, 'false', Backend) end).

-spec modify_backend(ne_binary(), fun((kz_json:object()) -> kz_json:object())) -> 'ok' | {'error', 'not_registered'}.
modify_backend(Name, Fun) ->
    Backends = registered_backends(),
    case [J || J <- Backends, match_backend(Name, J)] of
        [] ->
            {'error', 'not_registered'};
        _ ->
            set_registered_backends([maybe_modify_backend(Name, Fun, J) || J <- Backends])
    end.

-spec maybe_modify_backend(ne_binary(), fun((kz_json:object()) -> kz_json:object()), kz_json:object()) -> kz_json:object().
maybe_modify_backend(Name, Fun, JObj) ->
    case match_backend(Name, JObj) of
        'true' -> Fun(JObj);
        'false' -> JObj
    end.

-spec registered_backends() -> kz_json:object().
registered_backends() -> 
    kapps_config:get_jsons(<<"edr">>, <<"backends">>, []).

-spec match_backend(ne_binary(), kz_json:object()) -> boolean().
match_backend(Name, JObj) ->
    kz_json:get_binary_value(<<"name">>, JObj) =:= Name.

-spec set_registered_backends(kz_json:objects()) -> 'ok'.
set_registered_backends(NewBackends) ->
    {'ok', _} = kapps_config:set(<<"edr">>, <<"backends">>, NewBackends),
    'ok'.
