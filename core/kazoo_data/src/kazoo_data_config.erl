-module(kazoo_data_config).

-export([get_ne_binary/1, get_ne_binary/2
        ,get_ne_binaries/1, get_ne_binaries/2
        ,get_pos_integer/1, get_pos_integer/2
        ,get_json/1, get_json/2
        ,get_is_true/1, get_is_true/2
        ]).

-include("kz_data.hrl").

-compile({no_auto_import,[get/1]}).

-spec get_pos_integer(ne_binary()) -> api_pos_integer().
-spec get_pos_integer(ne_binary(), Default) -> pos_integer() | Default.
get_pos_integer(Key) ->
    get_pos_integer(Key, 'undefined').
get_pos_integer(Key, Default) ->
    case get(Key) of
        'undefined' -> Default;
        {'ok', Value} -> as_integer(Value, Default, fun(V) -> V > 0 end)
    end.

-spec get_ne_binary(ne_binary()) -> api_ne_binary().
-spec get_ne_binary(ne_binary(), Default) -> ne_binary() | Default.
get_ne_binary(Key) ->
    get_ne_binary(Key, 'undefined').
get_ne_binary(Key, Default) ->
    case get(Key) of
        'undefined' -> Default;
        {'ok', Value} ->
            nonempty(kz_term:to_binary(Value), Default)
    end.

-spec get_ne_binaries(ne_binary()) -> api_ne_binaries().
-spec get_ne_binaries(ne_binary(), Default) -> ne_binaries() | Default.
get_ne_binaries(Key) ->
    get_ne_binaries(Key, 'undefined').
get_ne_binaries(Key, Default) ->
    case get(Key) of
        'undefined' -> Default;
        {'ok', Value} ->
            nonempty(Value, Default)
    end.

-spec get_json(ne_binary()) -> api_object().
-spec get_json(ne_binary(), Default) -> kz_json:object() | Default.
get_json(Key) ->
    get_json(Key, 'undefined').
get_json(Key, Default) ->
    case get(Key) of
        'undefined' -> Default;
        {'ok', Value} ->
            as_json(Value, Default)
    end.

-spec get_is_true(ne_binary()) -> boolean().
-spec get_is_true(ne_binary(), Default) -> boolean() | Default.
get_is_true(Key) ->
    get_is_true(Key, 'false').
get_is_true(Key, Default) ->
    case get(Key) of
        'undefined' -> Default;
        {'ok', Value} ->
            as_boolean(Value, Default)
    end.

-spec get(ne_binary()) -> 'undefined' |
                          {'ok', any()}.
get(<<_/binary>>=Key) ->
    application:get_env(?APP, kz_term:to_atom(Key, 'true')).

-spec nonempty(any(), Default) -> any() | Default.
nonempty(Value, Default) ->
    case kz_term:is_empty(Value) of
        'true' -> Default;
        'false' -> Value
    end.

-spec as_integer(any(), Default, fun((integer()) -> boolean())) -> integer() | Default.
as_integer(Value, Default, VFun) ->
    VInt = kz_term:to_integer(Value),
    case VFun(VInt) of
        'true' -> VInt;
        'false' -> Default
    end.

-spec as_json(any(), Default) -> kz_json:object() | Default.
as_json(Value, Default) ->
    case kz_json:is_json_object(Value) of
        'true' -> Value;
        'false' -> Default
    end.

-spec as_boolean(any(), Default) -> boolean() | Default.
as_boolean(Value, Default) ->
    try kz_term:to_boolean(Value) of
        Boolean -> Boolean
    catch
        _:_ -> Default
    end.
