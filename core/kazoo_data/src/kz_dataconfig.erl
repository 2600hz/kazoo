%% @author root
%% @doc @todo Add description to kz_datafactory.


-module(kz_dataconfig).

-include("kz_data.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([connection/0]).


ensure_driver_app(App) ->
    application:ensure_all_started(App).

is_driver_app(App) ->
    Funs = kz_data:behaviour_info(callbacks),
    lists:all(fun({Fun, Arity}) -> erlang:function_exported(App, Fun, Arity) end, Funs).

connection() ->
    [Section] = wh_config:get('data', 'config', ['bigcouch']),
    Props = props:get_value('generic', wh_config:get(Section), []),
    connection(Props).
    
connection(List) when is_list(List) ->
    connection(maps:from_list(List));
connection(#{driver := App, tag := Tag}=Map) ->
    _ = ensure_driver_app(App),
    is_driver_app(App),
    #data_connection{props=Map, app=App, tag=Tag};
connection(#{}=Map) ->
    connection(maps:merge(?MERGE_MAP, Map)).

%% ====================================================================
%% Internal functions
%% ====================================================================


