%% @author root
%% @doc @todo Add description to kz_datafactory.


-module(kz_dataconfig).

-include("kz_data.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([connection/0, connection/1]).


ensure_driver_app(#{app := App}) ->
    application:ensure_all_started(App);
ensure_driver_app(#{driver := App}) ->
    application:ensure_all_started(App).

is_driver_app(App) ->
    Funs = kz_data:behaviour_info(callbacks),
    lists:all(fun({Fun, Arity}) -> erlang:function_exported(App, Fun, Arity) end, Funs).

connection() ->
    [Section] = wh_config:get('data', 'config', ['bigcouch']),
    Props = props:get_value('generic', wh_config:get(Section), []),
    connection(connection_options(Props)).

connection_options(Props) ->
    case props:get_value('connect_options', Props) of
        'undefined' -> Props;
        Section ->
            Options = props:get_value('generic', wh_config:get(Section), []),
            [{'connect_options', Options} | props:delete('connect_options', Props)]
    end.

connection(List) when is_list(List) ->
    connection(maps:from_list(props:filter_undefined(List)));
connection(#{driver := App}=Map)
  when not is_atom(App) ->
    connection(Map#{driver => wh_util:to_atom(App, 'true')});
connection(#{app := App}=Map)
  when not is_atom(App) ->
    connection(Map#{app => wh_util:to_atom(App, 'true')});
connection(#{module := App}=Map)
  when not is_atom(App) ->
    connection(Map#{module => wh_util:to_atom(App, 'true')});
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

%% ====================================================================
%% Internal functions
%% ====================================================================


