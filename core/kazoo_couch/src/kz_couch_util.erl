%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz
%%% @doc
%%% Util functions used by whistle_couch
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
-include_lib("whistle/include/wapi_conf.hrl").



%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Send the query function in an anon fun with arity 0; if it returns 504, retry
%% until 3 failed retries occur.
%% @end
%%------------------------------------------------------------------------------
-type retry504_ret() :: _.
%% 'ok' | ne_binary() |
%% {'ok', wh_json:object() | wh_json:objects() |
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
    whistle_stats:increment_counter(<<"bigcouch-504-error">>),
    {'error', 'timeout'};
retry504s(Fun, Cnt) ->
    whistle_stats:increment_counter(<<"bigcouch-request">>),
    case catch Fun() of
        {'error', {'ok', "504", _, _}} ->
            whistle_stats:increment_counter(<<"bigcouch-504-error">>),
            timer:sleep(100 * (Cnt+1)),
            retry504s(Fun, Cnt+1);
        {'error', {'ok', ErrCode, _Hdrs, _Body}} ->
            whistle_stats:increment_counter(<<"bigcouch-other-error">>),
            {'error', wh_util:to_integer(ErrCode)};
        {'error', _Other}=E ->
            whistle_stats:increment_counter(<<"bigcouch-other-error">>),
            E;
        {'ok', _Other}=OK -> OK;
        {'EXIT', _E} ->
            ST = erlang:get_stacktrace(),
            lager:debug("exception running fun: ~p", [_E]),
            wh_util:log_stacktrace(ST),
            whistle_stats:increment_counter(<<"bigcouch-other-error">>),
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
                                        ,username=_User
                                        ,password=_Pass
                                    }) ->
%%    Options = [{'basic_auth', {User, Pass}}
%%     Options = [{'pool', Tag}
%%                ,{'pool_name', Tag}
    Options = [{'pool_size', 100}
               ,{'max_sessions', 512}
               ,{'max_pipeline_size', 10}
               ,{'connect_timeout', 500}
               ,{'connect_options', [{'keepalive', true}]}
%               ,{'timeout', 'infinity'}
              ],
    get_new_conn(Host, Port, Options);
new_connection(#{}=Map) ->
    new_connection(maps:fold(fun connection_parse/3, #kz_couch_connection{}, Map)).

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


-spec get_new_conn(nonempty_string() | ne_binary(), pos_integer(), wh_proplist()) ->
                          {'ok', server()} |
                          {'error', 'timeout'} |
                          {'error', 'ehostunreach'}.
get_new_conn(Host, Port, Opts) ->
    Conn = couchbeam:server_connection(wh_util:to_list(Host), Port, "", Opts),
    lager:debug("new connection to host ~s:~b, testing: ~p", [Host, Port, Conn]),
    case server_info(Conn) of
        {'ok', ConnData} ->
            CouchVersion = wh_json:get_value(<<"version">>, ConnData),
            BigCouchVersion = wh_json:get_value(<<"bigcouch">>, ConnData),
            lager:info("connected successfully to ~s:~b", [Host, Port]),
            lager:debug("responding CouchDB version: ~p", [CouchVersion]),
            lager:debug("responding BigCouch version: ~p", [BigCouchVersion]),
            {'ok', Conn};
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

-spec server_info(server()) -> {'ok', wh_json:object()} |
                               {'error', any()}.
server_info(#server{}=Conn) -> couchbeam:server_info(Conn).

-spec server_url(server()) -> ne_binary().
server_url(#server{url=Url}) -> Url.
%%     UserPass = case props:get_value('basic_auth', Options) of
%%                    'undefined' -> <<>>;
%%                    {U, P} -> list_to_binary([U, <<":">>, P])
%%                end,
%%     Protocol = case wh_util:is_true(props:get_value('is_ssl', Options)) of
%%                    'false' -> <<"http">>;
%%                    'true' -> <<"https">>
%%                end,
%%
%%     list_to_binary([Protocol, <<"://">>, UserPass
%%                     ,<<"@">>, wh_util:to_binary(Host)
%%                     ,<<":">>, wh_util:to_binary(Port)
%%                     ,<<"/">>
%%                    ]).

-spec db_url(server(), ne_binary()) -> ne_binary().
db_url(#server{}=Conn, DbName) ->
    Server = server_url(Conn),
    list_to_binary([Server, DbName]).

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

-spec maybe_add_rev(couchbeam_db(), ne_binary(), wh_proplist()) -> wh_proplist().
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
    case wh_util:is_empty(DocId) of
        'true' -> {'error', 'empty_doc_id'};
        'false' -> ?RETRY_504(couchbeam:lookup_doc_rev(Db, DocId))
    end.
