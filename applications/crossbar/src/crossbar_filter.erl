-module(crossbar_filter).
-include("crossbar.hrl").
-export([build/1, defined/1]).

true(_) -> true.

build(Context) ->
    QS = cb_context:query_string(Context),
    build(Context, defined_in(QS)).

build(_QS, false) -> fun true/1;
build(QS, true) -> build_with(QS).

defined(Context) ->
    defined_in(cb_context:query_string(Context)).

build_with(QueryString) ->
    fun(Doc) ->
            filter_doc_by_querystring(Doc, QueryString)
    end.

defined_in(QueryString) ->
    kz_json:any(fun is_filter_key/1, QueryString).

-spec is_filter_key({binary(), any()}) -> boolean().
is_filter_key({<<"filter_", _/binary>>, _}) -> 'true';
is_filter_key({<<"has_key", _/binary>>, _}) -> 'true';
is_filter_key({<<"key_missing", _/binary>>, _}) -> 'true';
is_filter_key({<<"has_value", _/binary>>, _}) -> 'true';
is_filter_key({<<"created_from">>, _}) -> 'true';
is_filter_key({<<"created_to">>, _}) -> 'true';
is_filter_key({<<"modified_from">>, _}) -> 'true';
is_filter_key({<<"modified_to">>, _}) -> 'true';
is_filter_key(_) -> 'false'.

-spec filter_doc_by_querystring(kz_json:object(), kz_json:object()) -> boolean().
filter_doc_by_querystring(Doc, QueryString) ->
    kz_json:all(fun({K, V}) -> should_filter_doc(Doc, K, V) end, QueryString).

-spec should_filter_doc(kz_json:object(), ne_binary(), kz_json:json_term()) -> boolean().
should_filter_doc(Doc, K, V) ->
    try filter_prop(Doc, K, V) of
        'undefined' -> 'true';
        Bool -> Bool
    catch
        _E:_R ->
            lager:debug("failed to process filter ~s: ~s:~p", [K, _E, _R]),
            'false'
    end.

-spec filter_prop(kz_json:object(), ne_binary(), any()) -> api_boolean().
filter_prop(Doc, <<"filter_not_", Key/binary>>, Val) ->
    not should_filter(Doc, Key, Val);
filter_prop(Doc, <<"filter_", Key/binary>>, Val) ->
    should_filter(Doc, Key, Val);
filter_prop(Doc, <<"has_key">>, Key) ->
    has_key(Doc, Key);
filter_prop(Doc, <<"key_missing">>, Key) ->
    not has_key(Doc, Key);
filter_prop(Doc, <<"has_value">>, Key) ->
    has_value(Doc, Key);
filter_prop(Doc, <<"created_from">>, Val) ->
    lowerbound(kz_doc:created(Doc), kz_util:to_integer(Val));
filter_prop(Doc, <<"created_to">>, Val) ->
    upperbound(kz_doc:created(Doc), kz_util:to_integer(Val));
filter_prop(Doc, <<"modified_from">>, Val) ->
    lowerbound(kz_doc:modified(Doc), kz_util:to_integer(Val));
filter_prop(Doc, <<"modified_to">>, Val) ->
    upperbound(kz_doc:modified(Doc), kz_util:to_integer(Val));
filter_prop(_, _, _) ->
    'undefined'.

-spec upperbound(integer(), integer()) -> boolean().
upperbound(DocTimestamp, QSTimestamp) ->
    QSTimestamp >= DocTimestamp.

-spec lowerbound(integer(), integer()) -> boolean().
lowerbound(DocTimestamp, QSTimestamp) ->
    QSTimestamp =< DocTimestamp.

-spec should_filter(binary(), ne_binary()) -> boolean().
-spec should_filter(kz_json:object(), ne_binary(), kz_json:json_term()) -> boolean().
should_filter(Val, Val) -> 'true';
should_filter(Val, FilterVal) ->
    try kz_json:unsafe_decode(FilterVal) of
        List when is_list(List) ->
            lists:member(Val, List);
        _Data ->
            lager:debug("data is not a list: ~p", [_Data]),
            'false'
    catch
        _Error -> 'false'
    end.

should_filter(Doc, Key, Val) ->
    Keys = binary_key_to_json_key(Key),
    should_filter(
      kz_json:get_binary_value(Keys, Doc, <<>>)
                 ,kz_util:to_binary(Val)
     ).

-spec has_key(kz_json:object(), ne_binary()) -> boolean().
has_key(Doc, Key) ->
    Keys = binary_key_to_json_key(Key),
    kz_json:get_value(Keys, Doc) =/= 'undefined'.

-spec has_value(kz_json:object(), ne_binary()) -> boolean().
has_value(Doc, Key) ->
    Keys = binary_key_to_json_key(Key),
    kz_json:get_ne_value(Keys, Doc) =/= 'undefined'.

-spec binary_key_to_json_key(ne_binary()) -> ne_binaries().
binary_key_to_json_key(Key) ->
    binary:split(Key, <<".">>, ['global']).
