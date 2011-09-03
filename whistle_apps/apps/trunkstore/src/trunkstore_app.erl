-module(trunkstore_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, revise_views/0, setup_base_docs/0]).

-include("ts.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec start/2 :: (StartType, StartArgs) -> {'ok', pid()} | {'error', term()} when
      StartType :: term(),
      StartArgs :: term().
start(_StartType, _StartArgs) ->
    trunkstore_app:revise_views(),
    trunkstore_app:setup_base_docs(),
    case trunkstore:start_link() of
	{ok, P} -> {ok, P};
	{error,{already_started, P}} -> {ok, P};
	{error, _}=E -> E;
	ignore -> {error, failed_to_start}
    end.

stop(_State) ->
    ok.

revise_views() ->
    lists:foreach(fun(File) ->
			  couch_mgr:revise_doc_from_file(?TS_DB, trunkstore, File)
		  end, ?TS_COUCH_DESIGN_DOCS).

setup_base_docs() ->
    lists:foreach(fun(File) -> couch_mgr:load_doc_from_file(?TS_DB, trunkstore, File) end, ?TS_COUCH_BASE_DOCS).
