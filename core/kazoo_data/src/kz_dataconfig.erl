%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_dataconfig).

-include("kz_data.hrl").

%%==============================================================================
%% API functions
%%==============================================================================

-export([connection/0, connection/1]).

-spec connection() -> data_connection().
connection() ->
    [Section] = kz_config:get_binary(<<"data">>, <<"config">>, [<<"bigcouch">>]),
    lager:debug("using section ~s for connection", [Section]),
    Config = kz_config:get(Section),
    Props = props:get_value(<<"generic">>, Config, []),
    connection(connection_options(Props)).

-spec connection_options(kz_term:proplist()) -> kz_term:proplist().
connection_options(Props) ->
    case props:get_binary_value(<<"connect_options">>, Props) of
        'undefined' -> Props;
        Section ->
            lager:debug("adding connect_options from ~s", [Section]),
            Options = props:get_value(<<"generic">>, kz_config:get(Section), []),
            [{<<"connect_options">>, normalize_connect_options(Options)} | props:delete(<<"connect_options">>, Props)]
    end.

-spec connection(kz_term:proplist() | map()) -> data_connection().
connection(List) when is_list(List) ->
    connection(maps:from_list([{kz_term:to_atom(K, 'true'), V} || {K, V} <- List]));
connection(#{driver := App}=Map)
  when not is_atom(App) ->
    connection(Map#{driver => kz_term:to_atom(App, 'true')});
connection(#{app := App}=Map)
  when not is_atom(App) ->
    connection(Map#{app => kz_term:to_atom(App, 'true')});
connection(#{module := App}=Map)
  when not is_atom(App) ->
    connection(Map#{module => kz_term:to_atom(App, 'true')});
connection(#{module := App, tag := Tag}=Map) ->
    _ = ensure_driver_app(Map),

    _IsDriverApp = is_driver_app(App),
    lager:debug("is ~s a driver app: ~p", [App, _IsDriverApp]),

    #data_connection{props=Map, app=App, tag=Tag};
connection(#{driver := App, tag := Tag}=Map) ->
    _ = ensure_driver_app(Map),

    _IsDriverApp = is_driver_app(App),
    lager:debug("is ~s a driver app: ~p", [App, _IsDriverApp]),

    #data_connection{props=Map, app=App, tag=Tag};
connection(#{}=Map) ->
    connection(maps:merge(?MERGE_MAP, Map)).

%%==============================================================================
%% Internal functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec ensure_driver_app(map()) -> any().
ensure_driver_app(#{app := App}) ->
    application:ensure_all_started(App);
ensure_driver_app(#{driver := App}) ->
    application:ensure_all_started(App).

-spec is_driver_app(atom()) -> boolean().
is_driver_app(App) ->
    Funs = kz_data:behaviour_info('callbacks'),
    lists:all(fun({Fun, Arity}) -> erlang:function_exported(App, Fun, Arity) end, Funs).

%% hackney connect_options which follow gen_tcp connect options
normalize_connect_options(Options) ->
    [normalize_connect_option(Option) || Option <- Options].

normalize_connect_option({<<"keepalive">>, Keep}) ->
    {'keepalive', kz_term:is_true(Keep)};
normalize_connect_option({Key, Value}) ->
    {kz_term:to_atom(Key, 'true'), Value};
normalize_connect_option(Key) ->
    kz_term:to_atom(Key).
