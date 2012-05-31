%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license. 
%%% See the NOTICE for more information.

-module(test_gen_changes).

-behaviour(gen_changes).
-export([start_link/2,
         init/1,
         handle_change/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-export([get_changes/1]).


-record(state, {changes=[]}).

start_link(Db, Opts) ->
    gen_changes:start_link(?MODULE, Db, Opts, []).


init([]) ->
    {ok, #state{}}.

get_changes(Pid) ->
    gen_changes:call(Pid, get_changes).

handle_change({done, _LastSeq}, State) ->
    {noreply, State};


handle_change(Change, State=#state{changes=Changes}) ->
    NewChanges = [Change|Changes],
    {noreply, State#state{changes=NewChanges}}.

handle_call(get_changes, _From, State=#state{changes=Changes}) ->
    {reply, lists:reverse(Changes), State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(Info, State) ->
    io:format("Unknown message: ~p~n", [Info]),
    {noreply, State}.

terminate(Reason, _State) ->
    io:format("~p terminating with reason ~p~n", [?MODULE, Reason]),
    ok.

