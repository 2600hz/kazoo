-module(whistle_util).

-export([reload_all_apps/0, reload_app/1]).
-export([to_e164/1, to_npanxxxxxx/1, to_1npanxxxxxx/1]).
-export([to_integer/1, to_float/1, to_hex/1, to_list/1, to_binary/1]).
-export([a1hash/3]).

reload_all_apps() ->
    Apps = application:which_applications(),
    lists:foreach(fun({App, _Desc, _Vsn}) -> reload_app(App) end, Apps).

reload_app(stdlib) -> ok;
reload_app(kernel) -> ok;
reload_app(App) ->
    {ok, Prop} = application:get_all_key(App),
    Mods = proplists:get_value(module, Prop, []),
    lists:foreach(fun(M) ->
			  code:purge(M),
			  code:load_file(M)
		  end, Mods).

to_hex(Bin) when is_binary(Bin) ->
    to_hex(binary_to_list(Bin));
to_hex(L) when is_list(L) ->
    string:to_lower(lists:flatten([io_lib:format("~2.16.0B", [H]) || H <- L])).

%% +18001234567 -> +18001234567
to_e164(<<$+, $1, N/bitstring>>=E164) when erlang:bit_size(N) == 80 -> % 8bits/ch * 10ch
    E164;
%% 18001234567 -> +18001234567
to_e164(<<$1, N/binary>>=NPAN1) when erlang:bit_size(N) == 80 ->
    << $+, NPAN1/bitstring >>;
%% 8001234567 -> +18001234567
to_e164(NPAN) when erlang:bit_size(NPAN) == 80 ->
    <<$+, $1, NPAN/bitstring>>;
to_e164(Other) ->
    Other.

to_npanxxxxxx(<<$+, $1, N/bitstring>>) when erlang:bit_size(N) == 80 ->
    N;
to_npanxxxxxx(<<$1, N/bitstring>>) when erlang:bit_size(N) == 80 ->
    N;
to_npanxxxxxx(NPAN) when erlang:bit_size(NPAN) == 80 ->
    NPAN;
to_npanxxxxxx(Other) ->
    Other.

to_1npanxxxxxx(<<$+, $1, N/bitstring>>) when erlang:bit_size(N) == 80 ->
    <<$1, N>>;
to_1npanxxxxxx(<<$1, N/bitstring>>=NPAN1) when erlang:bit_size(N) == 80 ->
    NPAN1;
to_1npanxxxxxx(NPAN) when erlang:bit_size(NPAN) == 80 ->
    <<$1, NPAN>>;
to_1npanxxxxxx(Other) ->
    Other.

-spec(to_integer/1 :: (X :: list() | binary() | integer() | float()) -> integer()).
to_integer(X) when is_float(X) ->
    round(X);
to_integer(X) when is_binary(X) ->
    list_to_integer(binary_to_list(X));
to_integer(X) when is_list(X) ->
    list_to_integer(X);
to_integer(X) when is_integer(X) ->
    X.

-spec(to_float/1 :: (X :: list() | binary() | integer() | float()) -> float()).
to_float(X) when is_binary(X) ->
    list_to_float(binary_to_list(X));
to_float(X) when is_list(X) ->
    list_to_float(X);
to_float(X) when is_integer(X) ->
    X * 1.0;
to_float(X) when is_float(X) ->
    X.

-spec(to_list/1 :: (X :: atom() | list() | binary() | integer() | float()) -> list()).
to_list(X) when is_float(X) ->
    mochinum:digits(X);
to_list(X) when is_integer(X) ->
    integer_to_list(X);
to_list(X) when is_binary(X) ->
    binary_to_list(X);
to_list(X) when is_atom(X) ->
    atom_to_list(X);
to_list(X) when is_list(X) ->
    X.

-spec(to_binary/1 :: (X :: atom() | list() | binary() | integer() | float()) -> binary()).
to_binary(X) when is_float(X) ->
    to_binary(mochinum:digits(X));
to_binary(X) when is_integer(X) ->
    to_binary(integer_to_list(X));
to_binary(X) when is_atom(X) ->
    list_to_binary(atom_to_list(X));
to_binary(X) when is_list(X) ->
    list_to_binary(X);
to_binary(X) when is_binary(X) ->
    X.

-spec(a1hash/3 :: (User :: binary() | list(), Realm :: binary() | list(), Password :: binary() | list()) -> string()).
a1hash(User, Realm, Password) ->
    to_hex(erlang:md5(list_to_binary([User,":",Realm,":",Password]))).
