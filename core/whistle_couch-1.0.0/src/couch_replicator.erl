%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, James Aimonetti
%%% @doc
%%% Replicate all DBs in from one connection to another
%%% @end
%%% Created :  7 Aug 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(couch_replicator).

-export([open_conn/1, open_conn/2, open_conn/3, open_conn/4]).
-export([start/3, repl_db/4]).

-include("wh_couch.hrl").

-type repl_result() :: 'ok' | {'error', 'failed'}.
-type repl_results() :: [repl_result(), ...] | [].

-spec open_conn(nonempty_string() | ne_binary()) ->
    server() | {'error', 'timeout' | 'ehostunreach' | term()}.
open_conn(Host) ->
    open_conn(Host, 5984).

-spec open_conn(nonempty_string() | ne_binary(), pos_integer()) ->
    server()
    | {'error', 'timeout' | 'ehostunreach' | term()}.
open_conn(Host, Port) ->
    open_conn(Host, Port, "", "").

-spec open_conn(nonempty_string() | ne_binary(), string(), string()) ->
    server()
    | {'error', 'timeout' | 'ehostunreach' | term()}.
open_conn(Host, User, Pass) ->
    open_conn(Host, 5984, User, Pass).

-spec open_conn(nonempty_string() | ne_binary(), pos_integer(), string(), string()) ->
    server()
    | {'error', 'timeout' | 'ehostunreach' | term()}.
open_conn(Host, Port, User, Pass) ->
    couch_util:get_new_connection(Host, Port, User, Pass).

-spec start(server(), server(), server()) -> repl_results().
start(AdminSource, Source, Target) ->
    {'ok', DBs} = couch_util:db_info(Source),
    [repl_db(AdminSource, Source, Target, wh_util:to_binary(mochiweb_util:quote_plus(DB))) || DB <- DBs].

%% url-encoded DB required here
-spec repl_db(server(), server(), server(), ne_binary()) -> repl_result().
repl_db(AdminSource, Source, Target, DB) ->
    put('callid', DB),
    lager:debug("replicating ~s", [DB]),
    case couch_util:db_exists(Target, DB) of
        'true' -> 'ok';
        'false' ->
            lager:debug("need to create ~s on target", [DB]),
            couch_util:db_create(Target, DB)
    end,

    ReplData = [{<<"source">>, couch_util:db_url(Source, DB)}
                ,{<<"target">>, couch_util:db_url(Target, DB)}
               ],

    try
        %% using couch_mgr here because Source and Target are port-forwarded localhost ports
        %% so they can't resolve each other; however, since couch_mgr is talking to my local
        %% bigcouch, it can get Source and Target talking...

        lager:debug("replication beginning"),
        case couch_util:db_replicate(AdminSource, ReplData) of % if Source and Target can talk directly
            %%case couch_mgr:db_replicate(ReplData) of
            {'ok', _} -> 'ok';
            {'error', {500, _}} ->
                {'error', 'failed'}
        end
    catch
        _E:_R ->
            lager:error("exception occurred: ~p:~p", [_E, _R]),
            wh_util:log_stacktrace(),
            {'error', 'failed'}
    end.
