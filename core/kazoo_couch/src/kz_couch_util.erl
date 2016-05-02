%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz
%%% @doc
%%% Util functions used by kazoo_couch
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-----------------------------------------------------------------------------
-module(kz_couch_util).


-export([retry504s/1]).

-export([new_connection/1
         ,get_db/2
         ,server_url/1
         ,db_url/2
         ,server_info/1
         ,format_error/1
        ]).

-export([maybe_add_rev/3]).

-include("kz_couch.hrl").
-include_lib("kazoo/include/kapi_conf.hrl").



%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Send the query function in an anon fun with arity 0; if it returns 504, retry
%% until 3 failed retries occur.
%% @end
%%------------------------------------------------------------------------------
-type retry504_ret() :: _.
%% 'ok' | ne_binary() |
%% {'ok', kz_json:object() | kz_json:objects() |
%%  binary() | ne_binaries() | boolean() | integer()
%% } |
%% couchbeam_error() |
%% {'error', 'timeout'}.

-spec retry504s(fun(() -> retry504_ret())) -> retry504_ret().
-spec retry504s(fun(() -> retry504_ret()), 0..3) -> retry504_ret().
retry504s(Fun) when is_function(Fun, 0) ->
    retry504s(Fun, 0).
retry504s(_Fun, 3) ->
    lager:debug("504 retry failed"),
    kazoo_stats:increment_counter(<<"bigcouch-504-error">>),
    {'error', 'timeout'};
retry504s(Fun, Cnt) ->
    kazoo_stats:increment_counter(<<"bigcouch-request">>),
    case catch Fun() of
        {'error', {'ok', 504, _, _}} ->
            kazoo_stats:increment_counter(<<"bigcouch-504-error">>),
            timer:sleep(100 * (Cnt+1)),
            retry504s(Fun, Cnt+1);
        {'error', {'ok', ErrCode, _Hdrs, _Body}} ->
            kazoo_stats:increment_counter(<<"bigcouch-other-error">>),
            {'error', kz_util:to_integer(ErrCode)};
        %%% couchbeam doesn't pass 202 as acceptable
        {'error', {'bad_response',{202, _Headers, Body}}} ->
            {'ok', kz_json:decode(Body)};
        {'error', {'bad_response',{204, _Headers, _Body}}} ->
            {'ok', kz_json:new()};
        {'error', {'bad_response',{_Code, _Headers, _Body}}=Response} ->
            kazoo_stats:increment_counter(<<"bigcouch-other-error">>),
            lager:critical("response code ~b not expected : ~p", [_Code, _Body]),
            {'error', format_error(Response)};
        {'error', Other} ->
            kazoo_stats:increment_counter(<<"bigcouch-other-error">>),
            {'error', format_error(Other)};
        {'ok', _Other}=OK -> OK;
        {'EXIT', _E} ->
            ST = erlang:get_stacktrace(),
            lager:debug("exception running fun: ~p", [_E]),
            kz_util:log_stacktrace(ST),
            kazoo_stats:increment_counter(<<"bigcouch-other-error">>),
            retry504s(Fun, Cnt+1);
        OK -> OK
    end.

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec new_connection(couch_connection() | #{}) ->
                                server() |
                                {'error', 'timeout' | 'ehostunreach' | _}.
new_connection(#kz_couch_connection{host=Host
                                   ,port=Port
                                   ,username=User
                                   ,password=Pass
                                   ,options=Options
                                    }) ->
    get_new_conn(Host, Port, maybe_add_auth(User, Pass, check_options(Options)));
new_connection(#{}=Map) ->
    new_connection(maps:fold(fun connection_parse/3, #kz_couch_connection{}, Map)).

-spec maybe_add_auth(string(), string(), kz_proplist()) -> kz_proplist().
maybe_add_auth("", _Pass, Options) -> Options;
maybe_add_auth(User, Pass, Options) ->
    [{'basic_auth', {User, Pass}} | Options].

check_options(Options) ->
    Routines = [fun convert_options/1
                ,fun filter_options/1
               ],
    lists:foldl(fun(Fun, Opts) -> Fun(Opts) end, Options, Routines).

filter_options(Options) ->
    [ KV || {K, _} = KV <- Options, not lists:member(K, ?NO_OPTIONS)].

convert_options(Options) ->
    lists:map(fun convert_option/1, Options).

convert_option({K, V}) ->
    case lists:member(K, ?ATOM_OPTIONS) of
        'true' -> {K, kz_util:to_atom(V, 'true')};
        'false' when is_map(V) -> {K, maps:to_list(V)};
        'false' -> {K, V}
    end.

-spec connection_parse(any(), any(), couch_connection()) -> couch_connection().
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


-spec get_new_conn(nonempty_string() | ne_binary(), pos_integer(), kz_proplist()) ->
                          {'ok', server()} |
                          {'error', 'timeout'} |
                          {'error', 'ehostunreach'}.
get_new_conn(Host, Port, Opts) ->
    Conn = couchbeam:server_connection(kz_util:to_list(Host), Port, "", Opts),
    lager:debug("new connection to host ~s:~b, testing: ~p", [Host, Port, Conn]),
    case server_info(Conn) of
        {'ok', ConnData} ->
            CouchVersion = kz_json:get_ne_binary_value(<<"version">>, ConnData),
            BigCouchVersion = kz_json:get_ne_binary_value(<<"bigcouch">>, ConnData),
            lager:info("connected successfully to ~s:~b", [Host, Port]),
            lager:debug("responding CouchDB version: ~p", [CouchVersion]),
            lager:debug("responding BigCouch version: ~p", [BigCouchVersion]),
            {'ok', add_couch_version(CouchVersion, BigCouchVersion, Conn)};
        {'error', {'conn_failed', {'error', 'timeout'}}} ->
            lager:warning("connection timed out for ~s:~p", [Host, Port]),
            {'error', 'timeout'};
        {'error', {'conn_failed', {'error', 'ehostunreach'}}} ->
            lager:warning("connection to ~s:~p failed: Host is unreachable", [Host, Port]),
            {'error', 'ehostunreach'};
        {'error', _E}=E ->
            lager:warning("connection to ~s:~p failed: ~p", [Host, Port, _E]),
            E
    end.

add_couch_version(<<"1.6", _/binary>>, 'undefined', #server{options=Options}=Conn) ->
    Conn#server{options = [{driver_version, 'couchdb_1_6'} | Options]};
add_couch_version(_, 'undefined', #server{options=Options}=Conn) ->
    Conn#server{options = [{driver_version, 'couchdb_2'} | Options]};
add_couch_version(_, _, #server{options=Options}=Conn) ->
    Conn#server{options = [{driver_version, 'bigcouch'} | Options]}.

-spec server_info(server()) -> {'ok', kz_json:object()} |
                               {'error', any()}.
server_info(#server{}=Conn) -> couchbeam:server_info(Conn).

-spec server_url(server()) -> ne_binary().
server_url(#server{url=Url}) -> Url.

-spec db_url(server(), ne_binary()) -> ne_binary().
db_url(#server{}=Conn, DbName) ->
    Server = server_url(Conn),
    list_to_binary([Server, "/", DbName]).

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% returns the #db{} record
%% @end
%%------------------------------------------------------------------------------
-spec get_db(server(), ne_binary()) -> db().
get_db(#server{}=Conn, DbName) ->
    {'ok', Db} = couchbeam:open_db(Conn, DbName),
    Db.

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
    kz_json:get_first_defined([<<"reason">>, <<"error">>], kz_json:decode(Body), 'unknown_error');
format_error({'bad_response',{Code, _Headers, _Body}}) ->
    io_lib:format("response code ~b not expected", [Code]);
format_error('timeout') -> 'timeout';
format_error('not_found') -> 'not_found';
format_error({'error', 'connect_timeout'}) -> 'connect_timeout';
format_error({'http_error', 500, Msg}) -> Msg;
format_error({'error', Error}) -> Error;
format_error(E) ->
    lager:warning("unformatted error: ~p", [E]),
    E.

-spec maybe_add_rev(couchbeam_db(), ne_binary(), kz_proplist()) -> kz_proplist().
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

-spec do_fetch_rev(couchbeam_db(), ne_binary()) ->
                          ne_binary() |
                          couchbeam_error().
do_fetch_rev(#db{}=Db, DocId) ->
    case kz_util:is_empty(DocId) of
        'true' -> {'error', 'empty_doc_id'};
        'false' -> ?RETRY_504(couchbeam:lookup_doc_rev(Db, DocId))
    end.
