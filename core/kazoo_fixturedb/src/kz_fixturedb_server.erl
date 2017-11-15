%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_fixturedb_server).

%% Driver callbacks
-export([new_connection/1
        ]).

%% Server callbacks
-export([server_info/1
        ,server_url/1
        ,get_db/2
        ,db_url/2
        ]).

%% API
-export([get_app_connection/1
        ,maybe_use_app_connection/2
        ]).

-include("kz_fixturedb.hrl").

%%%===================================================================
%%% Driver callbacks
%%%===================================================================

-spec new_connection(map()) -> {'ok', server_map()}.
new_connection(Map) ->
    Url = code:priv_dir(kazoo_fixturedb),
    {'ok', #{url => Url ++ "/dbs"
            ,options => Map
            }
    }.

%%%===================================================================
%%% Connection operations
%%%===================================================================

-spec get_db(server_map(), ne_binary()) -> map().
get_db(Server, DbName) ->
    ConnToUse = maybe_use_app_connection(Server, DbName),
    #{server => ConnToUse, name => DbName}.

-spec server_url(server_map()) -> ne_binary().
server_url(#{url := Url}) ->
    Url.

-spec db_url(server_map(), ne_binary()) -> ne_binary().
db_url(Server, DbName) ->
    #{url := Url} = maybe_use_app_connection(Server, DbName),
    <<(kz_term:to_binary(Url))/binary, "/", DbName/binary>>.


-spec server_info(server_map()) -> doc_resp().
server_info(_Server) ->
    {'ok', kz_json:from_list(
             [{<<"kazoo">>, <<"Willkommen">>}
             ,{<<"version">>, <<"0.0.0.0.0.0.0.1">>}
             ,{<<"features">>, [<<"cool">>]}
             ,{<<"vendor">>, kz_json:from_list([{<<"name">>, <<"the Great FixtureDB Committee">>}])}
             ])
    }.

%%%===================================================================
%%% API
%%%===================================================================

-spec get_app_connection(server_map()) -> server_map().
get_app_connection(#{options := Options}=Server) ->
    case maps:get(test_app, Options, 'unedfined') of
        'unedfined' -> Server;
        AppName ->
            set_app_connection(Server, AppName)
    end.

-spec maybe_use_app_connection(server_map(), ne_binary()) -> server_map().
maybe_use_app_connection(#{options := Options}=Server, DbName) ->
    case {maps:get(test_app, Options, 'unedfined')
         ,maps:get(test_db, Options, 'unedfined')
         }
    of
        {'unedfined', _} -> Server;
        {_AppName, 'unedfined'} ->
            ?LOG_DEBUG("test_db is not set, using kazoo_fixturedb database..."),
            Server;
        {AppName, DbName} ->
            set_app_connection(Server, AppName);
        {_AppName, _OtherDb} ->
            ?LOG_DEBUG("requested db ~s is not test_db ~s, using kazoo_fixturedb database path...", [DbName, _OtherDb]),
            Server
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec set_app_connection(server_map(), atom()) -> server_map().
set_app_connection(#{options := Options}=Server, AppName) ->
    Path = case maps:get(test_db_subdir, Options, 'unedfined') of
               'unedfined' -> code:priv_dir(AppName);
               P -> code:lib_dir(AppName, kz_term:to_atom(P, 'true'))
           end,
    case Path of
        {'error', 'bad_name'} ->
            ?LOG_DEBUG("bad_name for path ~p, using default kazoo_fixturedb database path...", [AppName]),
            Server;
        _ ->
            Server#{url => Path}
    end.
