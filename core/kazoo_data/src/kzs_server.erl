%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc data adapter behaviour
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzs_server).


-export([get_db/2
        ,server_url/1
        ,db_url/2
        ,server_info/1
        ,format_error/1
        ]).

-include("kz_data.hrl").

-spec server_info(map() | server()) -> {'ok', kz_json:object()} |
          {'error', any()}.
server_info(#{server := {App, Conn}}) -> App:server_info(Conn);
server_info({App, Conn}) -> App:server_info(Conn).

-spec server_url(map() | server()) -> kz_term:ne_binary().
server_url(#{server := {App, Conn}}) -> App:server_url(Conn);
server_url({App, Conn}) -> App:server_url(Conn).

-spec db_url(map() | server(), kz_term:ne_binary()) -> kz_term:ne_binary().
db_url(#{server := {App, Conn}}, DbName) -> App:db_url(Conn, DbName);
db_url({App, Conn}, DbName) -> App:db_url(Conn, DbName).

-spec get_db(map() | server(), kz_term:ne_binary()) -> db().
get_db(#{server := {App, Conn}}, DbName) ->
    Db = App:get_db(Conn, DbName),
    #db{app=App, server=Conn, db=Db, name=DbName};
get_db({App, Conn}, DbName) ->
    Db = App:get_db(Conn, DbName),
    #db{app=App, server=Conn, db=Db, name=DbName}.

-spec format_error(any()) -> any().
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
    case kz_json:get_value(<<"error">>, kz_json:decode(Body)) of
        <<"timeout">> -> 'server_timeout';
        _Error ->
            lager:warning("server error: ~s", [Body]),
            'server_error'
    end;
format_error('timeout') -> 'timeout';
format_error(E) ->
    lager:warning("unformatted error: ~p", [E]),
    E.
