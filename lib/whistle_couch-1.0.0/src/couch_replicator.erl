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
-export([start/2, repl_db/3]).

-include("wh_couch.hrl").

open_conn(Host) ->
    open_conn(Host, 5984).

open_conn(Host, Port) ->
    open_conn(Host, Port, "", "").

open_conn(Host, User, Pass) ->
    open_conn(Host, 5984, User, Pass).

open_conn(Host, Port, User, Pass) ->
    couch_util:get_new_connection(Host, Port, User, Pass).

start(Source, Target) ->
    {ok, DBs} = couch_util:db_info(Source),
    [ repl_db(Source, Target, wh_util:to_binary(mochiweb_util:quote_plus(DB))) || DB <- DBs].

%% url-encoded DB required here
repl_db(Source, Target, DB) ->
    ?LOG_SYS("Replicating ~s", [DB]),
    put(callid, DB),
    case couch_util:db_exists(Target, DB) of
	true -> ok;
	false ->
	    ?LOG_SYS("Need to create ~s on target", [DB]),
	    couch_util:db_create(Target, DB)
    end,

    ReplData = [{<<"source">>, couch_util:db_url(Source, DB)}
		,{<<"target">>, couch_util:db_url(Target, DB)}
	       ],

    try
	%% using couch_mgr here because Source and Target are port-forwarded localhost ports
	%% so they can't resolve each other; however, since couch_mgr is talking to my local
	%% bigcouch, it can get Source and Target talking...

	?LOG_SYS("Replication beginning"),
	case couch_util:db_replicate(Source, ReplData) of % if Source and Target can talk directly
	    %%case couch_mgr:db_replicate(ReplData) of
	    {ok, _} -> ok;
	    {error, {500, _}} ->
		{error, failed}
	end
    catch
	_E:_R ->
	    ?LOG_SYS("Exception occurred, probably a gateway timeout"),
	    ?LOG_SYS("~p", [_E]),
	    ?LOG_SYS("~p", [_R]),
	    {error, failed}
    end.
