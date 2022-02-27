%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2022, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_dataconfig).

-include("kz_data.hrl").

%%==============================================================================
%% API functions
%%==============================================================================

-export([connection/0, connection/1]).

-spec ensure_driver_app(map()) -> any().
ensure_driver_app(#{app := App}) ->
    application:ensure_all_started(App);
ensure_driver_app(#{driver := App}) ->
    application:ensure_all_started(App).

-spec is_driver_app(atom()) -> boolean().
is_driver_app(App) ->
    Funs = kz_data:behaviour_info('callbacks'),
    lists:all(fun({Fun, Arity}) -> erlang:function_exported(App, Fun, Arity) end, Funs).

-spec connection() -> data_connection().
connection() ->
    [Section] = kz_config:get('data', 'config', ['bigcouch']),
    Props = props:get_value('generic', kz_config:get(Section), []),
    connection(connection_options(Props)).

-spec connection_options(kz_term:proplist()) -> kz_term:proplist().
connection_options(Props) ->
    case props:get_value('connect_options', Props) of
        'undefined' -> Props;
        Section ->
            Options = props:get_value('generic', kz_config:get(Section), []),
            [{'connect_options', Options} | props:delete('connect_options', Props)]
    end.

-spec connection(kz_term:proplist() | map()) -> data_connection().
connection(List) when is_list(List) ->
    connection(maps:from_list(List));
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
    is_driver_app(App),
    #data_connection{props=Map, app=App, tag=Tag};
connection(#{driver := App, tag := Tag}=Map) ->
    _ = ensure_driver_app(Map),
    is_driver_app(App),
    #data_connection{props=Map, app=App, tag=Tag};
connection(#{}=Map) ->
    connection(maps:merge(?MERGE_MAP, Map)).

%%==============================================================================
%% Internal functions
%%==============================================================================
