-module(openid_pm).
-export([url_encode/1, uri_encode/1, kvf_encode/1, kvf_decode/1]).

url_encode(PL) when is_list(PL) ->
    encode(url, PL).

uri_encode(PL) when is_list(PL) ->
    encode(uri, PL).

kvf_encode(PL) when is_list(PL) ->
    encode(kvf, PL).

kvf_decode(Body) when is_list(Body) ->
    [ split_kv(Pair) || Pair <- string:tokens(Body, "\n") ].

split_kv(Pair) when is_list(Pair) ->
    {Key, [$:|Value]} = lists:splitwith(fun(C) -> C =/= $: end, Pair),
    { Key, Value }.

encode(Type, PL) when is_list(PL) andalso ((Type =:= uri) orelse (Type =:= url) orelse (Type =:= kvf)) ->
    {Encoder, KVSep, PairSep} = case Type of
				    uri -> {fun openid_utils:url_encode/1, "=", "&"};
				    url -> {fun openid_utils:uri_encode/1, "=", "&"};
				    kvf -> {fun (X) -> X end, ":", ""}
				end,
    Pairs = lists:foldr(
	      fun({K, V}, Acc) ->
		      FormattedK = format_key(Type, K),
		      EncodedK = Encoder(FormattedK),
		      FormattedV = format_val(Type, V),
		      EncodedV = Encoder(FormattedV),
		      [ EncodedK ++ KVSep ++ EncodedV | Acc ]
	      end, [], PL),
    string:join(Pairs, PairSep).

format_key(Type, K) ->
    String = to_string(K),
    case String of
	"openid." ++ Suffix ->
	    case Type of
		kvf -> Suffix;
		_ -> String
	    end;
	Suffix ->
	    case Type of
		kvf -> Suffix;
		_ -> "openid." ++ String
	    end
    end.

format_val(kvf, V) -> to_string(V) ++ "\n";
format_val(_Type, V) -> to_string(V).

to_string(List) when is_list(List) -> List;
to_string(Binary) when is_binary(Binary) -> binary_to_list(Binary);
to_string(Atom) when is_atom(Atom) -> atom_to_list(Atom).
