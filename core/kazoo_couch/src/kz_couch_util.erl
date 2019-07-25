%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Util functions used by kazoo_couch.
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_couch_util).

-export([retry504s/1]).

-export([new_connection/1
        ,admin_connection/1
        ,get_db/2
        ,server_url/1
        ,db_url/2
        ,server_info/1
        ,connection_info/1
        ,format_error/1
        ,is_couchdb/1
        ]).

-export([maybe_add_rev/3]).

-include("kz_couch.hrl").
-include_lib("kazoo_amqp/include/kapi_conf.hrl").

%%------------------------------------------------------------------------------
%% @doc Send the query function in an anon fun with arity 0; if it returns 504, retry
%% until 3 failed retries occur.
%% @end
%%------------------------------------------------------------------------------
-type retry504_ret() :: any().
%% 'ok' | kz_term:ne_binary() |
%% {'ok', kz_json:object() | kz_json:objects() |
%%  binary() | kz_term:ne_binaries() | boolean() | integer()
%% } |
%% couchbeam_error() |
%% {'error', 'timeout'}.

-spec retry504s(fun(() -> retry504_ret())) -> retry504_ret().
retry504s(Fun) when is_function(Fun, 0) ->
    retry504s(Fun, 0).

-spec retry504s(fun(() -> retry504_ret()), 0..3) -> retry504_ret().
retry504s(_Fun, 3) ->
    lager:debug("504 retry failed"),
    {'error', 'timeout'};
retry504s(Fun, Cnt) ->
    kazoo_stats:increment_counter(<<"bigcouch-request">>),
    try Fun() of
        {'error', {'ok', 504, _, _}} ->
            kazoo_stats:increment_counter(<<"bigcouch-504-error">>),
            timer:sleep(100 * (Cnt+1)),
            retry504s(Fun, Cnt+1);
        {'error', {'ok', 500, _, _}} ->
            kazoo_stats:increment_counter(<<"bigcouch-500-error">>),
            timer:sleep(100 * (Cnt+1)),
            retry504s(Fun, Cnt+1);
        {'error', {'ok', ErrCode, _Hdrs, _Body}} ->
            kazoo_stats:increment_counter(<<"bigcouch-other-error">>),
            {'error', kz_term:to_integer(ErrCode)};
        %% couchbeam doesn't pass 202 as acceptable
        {'error', {'bad_response',{202, _Headers, Body}}} ->
            {'ok', kz_json:decode(Body)};
        {'error', {'bad_response',{204, _Headers, _Body}}} ->
            {'ok', kz_json:new()};
        {'error', {'bad_response',{_Code, _Headers, _Body}}=Response} ->
            kazoo_stats:increment_counter(<<"bigcouch-other-error">>),
            lager:critical("response code ~b not expected : ~p", [_Code, _Body]),
            {'error', format_error(Response)};
        {'error', Other}=Error when is_binary(Other) ->
            kazoo_stats:increment_counter(<<"bigcouch-other-error">>),
            Error;
        {'error', Other} ->
            kazoo_stats:increment_counter(<<"bigcouch-other-error">>),
            {'error', format_error(Other)};
        OK -> OK
    catch
        ?STACKTRACE(_E, _R, ST)
        lager:debug("exception running fun: ~p:~p", [_E, _R]),
        kz_util:log_stacktrace(ST),
        kazoo_stats:increment_counter(<<"bigcouch-other-error">>),
        retry504s(Fun, Cnt+1)
        end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec new_connection(couch_connection() | map()) ->
                            server() |
                            {'error', 'timeout' | 'ehostunreach' | _}.
new_connection(#{}=Map) ->
    connect(maps:fold(fun connection_parse/3, #kz_couch_connection{}, Map)).

-spec maybe_add_auth(string(), string(), kz_term:proplist()) -> kz_term:proplist().
maybe_add_auth("", _Pass, Options) -> Options;
maybe_add_auth(User, Pass, Options) ->
    [{'basic_auth', {User, Pass}} | Options].

check_options(Options) ->
    Routines = [fun convert_options/1
               ,fun filter_options/1
               ,fun maybe_default_recv_timeout/1
               ],
    lists:foldl(fun(Fun, Opts) -> Fun(Opts) end, Options, Routines).

-spec maybe_default_recv_timeout(kz_term:proplist()) -> kz_term:proplist().
maybe_default_recv_timeout(Options) ->
    case props:get_value('recv_timeout', Options) of
        'undefined' -> [{'recv_timeout', 20000} | Options];
        _Else -> Options
    end.

filter_options(Options) ->
    [KV || {K, _} = KV <- Options, not lists:member(K, ?NO_OPTIONS)].

convert_options(Options) ->
    [convert_option(O) || O <- Options].

convert_option({K, V} = KV) ->
    case lists:member(K, ?ATOM_OPTIONS) of
        'true' -> {K, kz_term:to_atom(V, 'true')};
        'false' when is_map(V) -> {K, maps:to_list(V)};
        'false' -> KV
    end.

-spec connection_parse(any(), any(), couch_connection()) -> couch_connection().
connection_parse(settings, Map, Conn)
  when is_map(Map) ->
    maps:fold(fun connection_parse/3, Conn, Map);
connection_parse(credentials, #{username := Username, password := Password}, Conn) ->
    Conn#kz_couch_connection{username=Username, password=Password};
connection_parse(pool, #{name := PoolName, size := PoolSize}, #kz_couch_connection{options=Options}=Conn) ->
    KVs = [{pool, PoolName}
          ,{pool_name, PoolName}
          ,{pool_size, PoolSize}
          ],
    Conn#kz_couch_connection{options = Options ++ KVs};
connection_parse(ip, V, Conn) ->
    Conn#kz_couch_connection{host=V};
connection_parse(host, V, Conn) ->
    Conn#kz_couch_connection{host=V};
connection_parse(port, V, Conn) ->
    Conn#kz_couch_connection{port=V};
connection_parse(username, V, Conn) ->
    Conn#kz_couch_connection{username=V};
connection_parse(password, V, Conn) ->
    Conn#kz_couch_connection{password=V};
connection_parse(K, V, #kz_couch_connection{options=Options}=Conn) ->
    Conn#kz_couch_connection{options = [{K, V} | Options]}.

-spec connect(couch_connection()) ->
                     {'ok', server()} |
                     {'error', 'timeout'} |
                     {'error', 'ehostunreach'}.
connect(#kz_couch_connection{host=Host
                            ,port=Port
                            ,username=User
                            ,password=Pass
                            ,options=Options
                            }) ->
    ConnMap = #{host => Host
               ,port => Port
               ,username => User
               ,password => Pass
               ,options => Options
               },
    Opts = [{'connection_map', ConnMap} | maybe_add_auth(User, Pass, check_options(Options))],
    Conn = couchbeam:server_connection(kz_term:to_list(Host), Port, <<>>, Opts),
    lager:info("new connection to host ~s:~b, testing: ~p", [Host, Port, Conn]),
    connection_info(Conn).

-spec add_couch_version(kz_term:ne_binary(), kz_term:api_ne_binary(), server()) -> server().
add_couch_version(<<"1.6", _/binary>>, 'undefined', #server{options=Options}=Conn) ->
    Conn#server{options = props:set_value('driver_version', 'couchdb_1_6', Options)};
add_couch_version(<<"1.1", _/binary>>, _Bigcouch, #server{options=Options}=Conn) ->
    Conn#server{options = props:set_value('driver_version', 'bigcouch', Options)};
add_couch_version(_, 'undefined', #server{options=Options}=Conn) ->
    Conn#server{options = props:set_value('driver_version', 'couchdb_2', Options)};
add_couch_version(_, _, #server{options=Options}=Conn) ->
    Conn#server{options = props:set_value('driver_version', 'bigcouch', Options)}.

-spec server_info(server()) -> {'ok', kz_json:object()} |
                               {'error', any()}.
server_info(#server{}=Conn) -> couchbeam:server_info(Conn).

-spec server_url(server()) -> kz_term:ne_binary().
server_url(#server{url=Url}) -> Url.

-spec db_url(server(), kz_term:ne_binary()) -> kz_term:ne_binary().
db_url(#server{}=Conn, DbName) ->
    Server = server_url(Conn),
    list_to_binary([Server, "/", DbName]).

%%------------------------------------------------------------------------------
%% @doc returns the #db{} record
%% @end
%%------------------------------------------------------------------------------
-spec get_db(server(), kz_term:ne_binary()) -> db().
get_db(Conn, <<DbName/binary>>) ->
    get_db(Conn, DbName, kazoo_couch:server_version(Conn)).

-spec get_db(server(), kz_term:ne_binary(), couch_version()) -> db().
get_db(Conn, DbName, Driver) ->
    ConnToUse = select_conn(Conn, DbName, Driver),
    {'ok', Db} = couchbeam:open_db(ConnToUse, DbName),
    Db.

-spec select_conn(server(), kz_term:ne_binary(), couch_version()) -> server().
select_conn(Conn, DbName, Driver) ->
    case is_admin_db(DbName, Driver) of
        'true' -> maybe_use_admin_conn(Conn);
        'false' -> Conn
    end.

-spec is_admin_db(kz_term:ne_binary(), couch_version()) -> boolean().
is_admin_db(<<"_dbs">>, 'couchdb_2') -> 'true';
is_admin_db(<<"_users">>, 'couchdb_2') -> 'true';
is_admin_db(<<"_nodes">>, 'couchdb_2') -> 'true';
is_admin_db(<<"_dbs">>, 'couchdb_1_6') -> 'true';
is_admin_db(<<"_users">>, 'couchdb_1_6') -> 'true';
is_admin_db(<<"_nodes">>, 'couchdb_1_6') -> 'true';
is_admin_db(<<"dbs">>, 'bigcouch') -> 'true';
is_admin_db(<<"users">>, 'bigcouch') -> 'true';
is_admin_db(<<"nodes">>, 'bigcouch') -> 'true';
is_admin_db(_Db, _Driver) -> 'false'.

%% loads the admin connection if possible
-spec admin_connection(server()) -> server().
admin_connection(Conn) ->
    maybe_use_admin_conn(Conn).

-spec maybe_use_admin_conn(server()) -> server().
maybe_use_admin_conn(#server{options=Options}=Conn) ->
    case props:get_value('admin_connection', Options) of
        'undefined' -> maybe_use_admin_port(Conn);
        AdminConn -> AdminConn
    end.

-spec maybe_use_admin_port(server()) -> server().
maybe_use_admin_port(#server{options=Options}=Conn) ->
    ConnectionMap = props:get_value('connection_map', Options, #{}),
    ConnMapOptions = maps:get('options', ConnectionMap),
    case props:get_value('admin_port', ConnMapOptions) of
        'undefined' ->
            APIPort = maps:get('port', ConnectionMap),
            AdminPort = APIPort + 2,
            change_connection_to_admin(Conn, APIPort, AdminPort);
        AdminPort ->
            APIPort = maps:get('port', ConnectionMap),
            change_connection_to_admin(Conn, APIPort, AdminPort)
    end.

-spec change_connection_to_admin(server(), inet:port_number(), inet:port_number()) -> server().
change_connection_to_admin(#server{url=Host
                                  ,options=Options
                                  }=Conn
                          ,APIPort
                          ,AdminPort
                          ) ->
    ConnectionMap = props:get_value('connection_map', Options, #{}),
    ConnMapOptions = maps:get('options', ConnectionMap),

    ConnMapOptions1 = props:set_value('port', AdminPort, ConnMapOptions),
    Options1 = props:set_value('connection_map', ConnectionMap#{'options'=>ConnMapOptions1}, Options),
    Conn#server{url=binary:replace(Host, kz_term:to_binary(APIPort), kz_term:to_binary(AdminPort))
               ,options=Options1
               }.

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
format_error({'ok', 500, _Headers, Body}) ->
    case kz_json:get_value(<<"error">>, kz_json:decode(Body)) of
        <<"timeout">> -> 'server_timeout';
        _Error ->
            lager:warning("server error: ~s", [Body]),
            'server_error'
    end;
format_error({'bad_response',{500, _Headers, Body}}) ->
    lager:warning_unsafe("server error - headers: ~p", [_Headers]),
    lager:warning_unsafe("server error - body : ~s", [Body]),
    kz_json:get_first_defined([<<"reason">>, <<"error">>], kz_json:decode(Body), 'unknown_error');
format_error({'bad_response',{Code, _Headers, Body}}) ->
    lager:warning_unsafe("server error - headers: ~p : ~p", [Code, _Headers]),
    lager:warning_unsafe("server error - body : ~p ~s", [Code, Body]),
    BodyJObj = kz_json:decode(Body),
    iolist_to_binary([integer_to_list(Code), ": ", kz_json:get_value(<<"error">>, BodyJObj, <<"unknown error">>)]);
format_error('timeout') -> 'timeout';
format_error('conflict') -> 'conflict';
format_error('not_found') -> 'not_found';
format_error('db_not_found') -> 'db_not_found';
format_error({'error', 'connect_timeout'}) -> 'connect_timeout';
format_error({'http_error', _, Msg}) -> Msg;
format_error({'error', {'closed', _Buffer}}) ->
    lager:warning("socket closed unexpectedly"),
    'tcp_closed';
format_error({'error', 'closed'}) ->
    lager:warning("socket closed unexpectedly"),
    'tcp_closed';
format_error({'error', Error}) -> Error;
format_error(<<"400: illegal_database_name">>) -> 'illegal_database_name';
format_error('forbidden') -> 'forbidden';
format_error(E) ->
    lager:warning("unformatted error: ~p", [E]),
    E.

-spec maybe_add_rev(couchbeam_db(), kz_term:ne_binary(), kz_term:proplist()) -> kz_term:proplist().
maybe_add_rev(#db{name=_Name}=Db, DocId, Options) ->
    case props:get_value('rev', Options) =:= 'undefined'
        andalso do_fetch_rev(Db, DocId)
    of
        <<_/binary>> = Rev ->
            lager:debug("adding rev ~s to options", [Rev]),
            [{'rev', Rev} | Options];
        'false' ->
            lager:debug("rev is in options list: ~p", [Options]),
            Options;
        {'error', 'not_found'} ->
            lager:debug("failed to find rev of ~s in ~p, not_found in db", [DocId, _Name]),
            Options;
        {'error', 'empty_doc_id'} ->
            lager:debug("failed to find doc id ~p", [DocId]),
            Options;
        _Else ->
            lager:debug("unknown rev format for ~p: ~p", [DocId, _Else]),
            Options
    end.

-spec do_fetch_rev(couchbeam_db(), kz_term:ne_binary()) ->
                          kz_term:ne_binary() |
                          couchbeam_error().
do_fetch_rev(#db{}=Db, DocId) ->
    case kz_term:is_empty(DocId) of
        'true' -> {'error', 'empty_doc_id'};
        'false' -> ?RETRY_504(couchbeam:lookup_doc_rev(Db, DocId))
    end.

-spec connection_info(server()) -> {'ok', server()} | {'error', term()}.
connection_info(#server{url=Url}=Conn) ->
    case server_info(Conn) of
        {'ok', ConnData} ->
            CouchVersion = kz_json:get_ne_binary_value(<<"version">>, ConnData),
            BigCouchVersion = kz_json:get_ne_binary_value(<<"bigcouch">>, ConnData),
            lager:debug("connected successfully to ~s", [Url]),
            lager:debug("responding CouchDB version: ~p", [CouchVersion]),
            lager:debug("responding BigCouch version: ~p", [BigCouchVersion]),
            {'ok', add_couch_version(CouchVersion, BigCouchVersion, Conn)};
        {'error', {'conn_failed', {'error', 'timeout'}}} ->
            lager:warning("connection timed out for ~s", [Url]),
            {'error', 'timeout'};
        {'error', {'conn_failed', {'error', 'ehostunreach'}}} ->
            lager:warning("connection to ~s failed: Host is unreachable", [Url]),
            {'error', 'ehostunreach'};
        {'error', _E}=E ->
            lager:warning("connection to ~s failed: ~p", [Url, _E]),
            E
    end.

-spec is_couchdb(term()) -> boolean().
is_couchdb(#server{}) -> 'true';
is_couchdb(_) -> 'false'.
