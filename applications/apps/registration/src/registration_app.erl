%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011 James Aimonetti
%%% @doc
%%% 
%%% @end
%%% Created :  Thu, 13 Jan 2011 22:12:40 GMT: James Aimonetti <james@2600hz.org>
-module(registration_app).

-behaviour(application).

-include("reg.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec(start/2 :: (StartType :: term(), StartArgs :: term()) -> tuple(ok, pid()) | tuple(error, term())).
start(_StartType, _StartArgs) ->
    setup_views(),
    case registration:start_link() of
	{ok, P} -> {ok, P};
	{error, {already_started, P} } -> {ok, P};
	{error, _}=E -> E
    end.

stop(_State) ->
    ok.

setup_views() ->
    lists:foreach(fun(File) ->
			  load_doc_from_file(?REG_DB, File)
		  end, ["registrations.json"]).

load_doc_from_file(DB, File) ->
    Path = lists:flatten([code:priv_dir(registration), "/couchdb/", whistle_util:to_list(File)]),
    logger:format_log(info, "Read into ~p from CouchDB dir: ~p~n", [DB, Path]),
    try
	{ok, ViewStr} = file:read_file(Path),
	{CDoc} = couchbeam_util:json_decode(ViewStr),
	{ok, _} = couch_mgr:save_doc(DB, CDoc) %% if it crashes on the match, the catch will let us know
    catch
	_A:_B ->
	    logger:format_log(error, "Error reading ~p into couch: ~p:~p~n", [Path, _A, _B])
    end.
