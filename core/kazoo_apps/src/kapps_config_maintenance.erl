%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2022, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapps_config_maintenance).

-export([get/2, get_pp/2
        ,get_category/1, get_category_pp/1
        ,get_json/2, get_json_pp/2
        ]).

-spec get(binary(), binary()) -> 'ok'.
get(Category, Key) ->
    get_pp(Category, Key, 'false').

-spec get_pp(binary(), binary()) -> 'ok'.
get_pp(Category, Key) ->
    get_pp(Category, Key, 'true').

get_pp(Category, Key, PP) ->
    pp(kapps_config:get(Category, Key), PP).

-spec get_json(binary(), binary()) -> 'ok'.
get_json(Category, Key) ->
    get_json_pp(Category, Key, 'false').

-spec get_json_pp(binary(), binary()) -> 'ok'.
get_json_pp(Category, Key) ->
    get_json_pp(Category, Key, 'true').

-spec get_json_pp(binary(), binary(), boolean()) -> 'ok'.
get_json_pp(Category, Key, PP) ->
    pp(kapps_config:get_json(Category, Key), PP).

-spec get_category(binary()) -> 'ok'.
get_category(Category) ->
    get_category_pp(Category, 'false').

-spec get_category_pp(binary()) -> 'ok'.
get_category_pp(Category) ->
    get_category_pp(Category, 'true').

-spec get_category_pp(binary(), boolean()) -> 'ok'.
get_category_pp(Category, PP) ->
    pp(kapps_config:get_category(Category), PP).

pp({'ok', Thing}, PP) ->
    pp(Thing, PP);
pp({'error', _E}, _PP) ->
    io:format("error: ~p~n", [_E]);
pp(Thing, PP) ->
    case kz_json:is_json_object(Thing) of
        'true' -> pp_json(Thing, PP);
        'false' -> pp_thing(Thing, PP)
    end.

pp_json(JObj, 'true') ->
    io:format("~s~n", [kz_json:encode(JObj, ['pretty'])]);
pp_json(JObj, 'false') ->
    io:format("~s~n", [kz_json:encode(JObj)]).

pp_thing(<<Value/binary>>, _PP) ->
    io:format("~s~n", [Value]);
pp_thing([], _PP) ->
    io:format("[]~n");
pp_thing([<<_/binary>>|_]=Values, 'true') ->
    io:format("~s~n", [kz_binary:join(Values)]);
pp_thing(Thing, _PP) ->
    io:format("~p~n", [Thing]).
