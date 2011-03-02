-module(trunkstore_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, update_views/0, setup_views/0]).
-export([start_replication/0, start_replication/1, stop_replication/0, stop_replication/1]).

-include("ts.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec(start/2 :: (StartType :: term(), StartArgs :: term()) -> tuple(ok, pid()) | tuple(error, term())).
start(_StartType, _StartArgs) ->
    setup_views(),
    setup_base_docs(),
    case trunkstore:start_link() of
	{ok, P} -> {ok, P};
	{error,{already_started, P}} -> {ok, P};
	{error, _}=E -> E
    end.

stop(_State) ->
    ok.

setup_views() ->
    lists:foreach(fun(File) ->
			  couch_mgr:load_doc_from_file(?TS_DB, trunkstore, File)
		  end, ?TS_COUCH_DESIGN_DOCS).

update_views() ->
    lists:foreach(fun(File) ->
			  case couch_mgr:update_doc_from_file(?TS_DB, trunkstore, File) of
			      {ok, _} -> logger:format_log(info, "Updating ~s: success~n", [File]);
			      {error, Reason} -> logger:format_log(info, "Updating ~s: error ~w~n", [File, Reason])
			  end
		  end, ?TS_COUCH_DESIGN_DOCS),
    ts_acctmgr:update_views().


setup_base_docs() ->
    lists:foreach(fun(File) -> couch_mgr:load_doc_from_file(?TS_DB, trunkstore, File) end, ?TS_COUCH_BASE_DOCS).

start_replication() ->
    start_replication(nodes()).

-spec(start_replication/1 :: (Ns :: list(atom())) -> no_return()).
start_replication([]) -> ok;
start_replication([N | Ns]) ->
    [_, H] = binary:split(whistle_util:to_binary(N), <<"@">>),
    UserPass = case couch_mgr:get_creds() of
		   {"", ""} -> "";
		   {U, P} -> list_to_binary([U, ":", P, "@"])
	       end,

    Sources = [ {list_to_binary(["http://", UserPass, H, ":5984/", ?TS_DB]), whistle_util:to_binary(?TS_DB)}
		,{list_to_binary(["http://", UserPass, H, ":5984/", ?TS_CDR_DB]), whistle_util:to_binary(?TS_CDR_DB)}],

    lists:foreach(fun({Source, Target}) ->
			  Res = couch_mgr:replicate([{<<"source">>, Source}
						     ,{<<"target">>, Target}
						     ,{<<"continuous">>, true}
						    ]),
			  logger:format_log(info, "TS_ACCTMGR.setup_replication: From ~s to ~s: ~w~n", [Source, Target, Res])
		  end, Sources),
    start_replication(Ns).

stop_replication() -> stop_replication(nodes()).

-spec(stop_replication/1 :: (Ns :: list(atom())) -> no_return()).
stop_replication([]) -> ok;
stop_replication([N | Ns]) ->
    [_, H] = binary:split(whistle_util:to_binary(N), <<"@">>),
    UserPass = case couch_mgr:get_creds() of
		   {"", ""} -> "";
		   {U, P} -> list_to_binary([U, ":", P, "@"])
	       end,

    Sources = [ {list_to_binary(["http://", UserPass, H, ":5984/", ?TS_DB]), whistle_util:to_binary(?TS_DB)}
		,{list_to_binary(["http://", UserPass, H, ":5984/", ?TS_CDR_DB]), whistle_util:to_binary(?TS_CDR_DB)}],

    lists:foreach(fun({Source, Target}) ->
			  Res = couch_mgr:replicate([{<<"source">>, Source}
						     ,{<<"target">>, Target}
						     ,{<<"continuous">>, true}
						     ,{<<"cancel">>, true}
						    ]),
			  logger:format_log(info, "TS_ACCTMGR.stop_replication: From ~s to ~s: ~w~n", [Source, Target, Res])
		  end, Sources),
    stop_replication(Ns).
