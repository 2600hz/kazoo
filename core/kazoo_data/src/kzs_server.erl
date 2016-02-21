%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz
%%% @doc
%%% data adapter behaviour
%%% @end
%%% @contributors
%%%-----------------------------------------------------------------------------
-module(kzs_server).


-export([get_db/2
         ,server_url/1
         ,db_url/2
         ,server_info/1
         ,format_error/1
        ]).

-include("kz_data.hrl").

-spec server_info(server()) -> {'ok', wh_json:object()} |
                               {'error', any()}.
server_info({App, Conn}) -> App:server_info(Conn).

-spec server_url(server()) -> ne_binary().
server_url({App, Conn}) -> App:server_url(Conn).

-spec db_url(server(), ne_binary()) -> ne_binary().
db_url({App, Conn}, DbName) -> App:db_url(Conn, DbName).

-spec get_db(server(), ne_binary()) -> db().
get_db({App, Conn}, DbName) ->
    {'ok', Db} = App:get_db(Conn, DbName),
    #db{app=App, server=Conn, db=Db, name=DbName}.


format_error({'failure', 404}) -> 'not_found';
format_error({'failure', 400}) -> 'client_error';
format_error({'http_error', {'status', 504}}) -> 'gateway_timeout';
format_error({'conn_failed', {'error', 'timeout'}}) -> 'connection_timeout';
format_error({'conn_failed', {'error', 'enetunreach'}}) -> 'network_unreachable';
format_error({'conn_failed', {'error', 'system_limit'}}) ->
    lager:critical("system limit reached for database operations!!"),
    'system_limit';
format_error({'conn_failed',{'error','econnrefused'}}) ->
    lager:warning("connection is being refused"),
    'econnrefused';
format_error({'ok', "500", _Headers, Body}) ->
    case wh_json:get_value(<<"error">>, wh_json:decode(Body)) of
        <<"timeout">> -> 'server_timeout';
        _Error ->
            lager:warning("server error: ~s", [Body]),
            'server_error'
    end;
format_error('timeout') -> 'timeout';
format_error(E) ->
    lager:warning("unformatted error: ~p", [E]),
    E.
