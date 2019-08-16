%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_json_generators).

-export([test_object/0
        ,non_empty_object/0
        ,deep_object/0, deep_object/1
        ,path/1

        ,max_depth/1
        ]).

-include_lib("kazoo_stdlib/include/kazoo_json.hrl").
-include_lib("proper/include/proper.hrl").

non_empty_object() ->
    ?SUCHTHAT(JObj, test_object(), not kz_json:is_empty(JObj)).

test_object() ->
    ?LET(JObj, object(), remove_duplicate_keys(JObj)).

remove_duplicate_keys(?JSON_WRAPPER(_)=JObj) ->
    kz_json:expand(kz_json:from_map(kz_json:to_map(kz_json:flatten(JObj)))).

deep_object() ->
    ?SIZED(Nesting, deep_object(Nesting)).

deep_object(Nesting) ->
    ?LET(DeepObject
        ,deep_object(Nesting, {'$call', 'kz_json', 'new', []})
        ,DeepObject
        ).

deep_object(0, JObj) ->
    JObj;
deep_object(Depth, JObj) ->
    deep_object(Depth-1
               ,{'$call', 'kz_json', 'set_value', [path(Depth), freq_value(Depth), JObj]}
               ).

path(0) ->
    ?LET(Key, json_string(), Key);
path(Depth) ->
    ?LET(Path
        ,resize(Depth, non_empty(json_string()))
        ,Path
        ).

freq_value(0) ->
    ?LET(Value, non_object_json_term(), Value);
freq_value(Depth) ->
    frequency([{90, ?LET(Value, non_object_json_term(), Value)}
              ,{10, ?LET(JObj, deep_object(Depth-1), JObj)}
              ]).

max_depth(JObj) ->
    Depth = kz_json:foldl(fun(Path, _, Max) ->
                                  case length(Path) of
                                      NewMax when NewMax > Max -> NewMax;
                                      _Len -> Max
                                  end
                          end
                         ,0
                         ,kz_json:flatten(JObj)
                         ),
    Depth.
