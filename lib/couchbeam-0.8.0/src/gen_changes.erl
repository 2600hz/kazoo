%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license.
%%% See the NOTICE for more information.

%% @doc gen_changes CouchDB continuous changes consumer behavior
%% This behaviour allws you to create easily a server that consume
%% Couchdb continuous changes

-module(gen_changes).

-include("couchbeam.hrl").

-behavior(gen_server).

-export([start_link/4]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([behaviour_info/1]).

-export([call/2,
         call/3,
         cast/2]).

-export([stop/1, get_seq/1]).


behaviour_info(callbacks) ->
    [{init, 1},
     {handle_change, 2},
     {handle_call, 3},
     {handle_cast, 2},
     {handle_info, 2},
     {terminate, 2}];
behaviour_info(_) ->
    undefined.

call(Name, Request) ->
    gen_server:call(Name, Request).

call(Name, Request, Timeout) ->
    gen_server:call(Name, Request, Timeout).

cast(Dest, Request) ->
    gen_server:cast(Dest, Request).

%% @doc create a gen_changes process as part of a supervision tree.
%% The function should be called, directly or indirectly, by the supervisor.
%% @spec start_link(Module, Db::db(), Options::changesoptions(),
%%                  InitArgs::list()) -> term()
%%       changesoptions() = [changeoption()]
%%       changeoption() = {include_docs, string()} |
%%                  {filter, string()} |
%%                  {since, integer()|string()} |
%%                  {heartbeat, string()|boolean()}
start_link(Module, Db, Options, InitArgs) ->
    gen_server:start_link(?MODULE, [Module, Db, Options, InitArgs], []).

init([Module, Db, Options, InitArgs]) ->
    case Module:init(InitArgs) of
        {ok, ModState} ->
            Self = self(),
            case couchbeam_changes:stream(Db, Self, Options) of
            {ok, StartRef, ChangesPid} ->
                erlang:monitor(process, ChangesPid),
                unlink(ChangesPid),
                LastSeq = proplists:get_value(since, Options, 0),
                {ok, #gen_changes_state{start_ref=StartRef,
                                        changes_pid=ChangesPid,
                                        mod=Module,
                                        modstate=ModState,
                                        db=Db,
                                        options=Options,
                                        last_seq=LastSeq}};
            {error, Error} ->
                Module:terminate(Error, ModState),
                {stop, Error}
            end;
        Error ->
            Error
    end.

stop(Pid) when is_pid(Pid) ->
    gen_server:cast(Pid, stop).

get_seq(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, get_seq).

handle_call(get_seq, _From, State=#gen_changes_state{last_seq=Seq}) ->
    {reply, Seq, State};
handle_call(Request, From,
            State=#gen_changes_state{mod=Module, modstate=ModState}) ->
    case Module:handle_call(Request, From, ModState) of
        {reply, Reply, NewModState} ->
            {reply, Reply, State#gen_changes_state{modstate=NewModState}};
        {reply, Reply, NewModState, A}
          when A =:= hibernate orelse is_number(A) ->
            {reply, Reply, State#gen_changes_state{modstate=NewModState}, A};
        {noreply, NewModState} ->
            {noreply, State#gen_changes_state{modstate=NewModState}};
        {noreply, NewModState, A} when A =:= hibernate orelse is_number(A) ->
            {noreply, State#gen_changes_state{modstate=NewModState}, A};
        {stop, Reason, NewModState} ->
            {stop, Reason, State#gen_changes_state{modstate=NewModState}};
        {stop, Reason, Reply, NewModState} ->
            {stop, Reason, Reply, State#gen_changes_state{modstate=NewModState}}
  end.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Msg, State=#gen_changes_state{mod=Module, modstate=ModState}) ->
    case Module:handle_cast(Msg, ModState) of
        {noreply, NewModState} ->
            {noreply, State#gen_changes_state{modstate=NewModState}};
        {noreply, NewModState, A} when A =:= hibernate orelse is_number(A) ->
            {noreply, State#gen_changes_state{modstate=NewModState}, A};
        {stop, Reason, NewModState} ->
            {stop, Reason, State#gen_changes_state{modstate=NewModState}}
    end.


handle_info({change, Ref, Msg},
        State=#gen_changes_state{mod=Module, modstate=ModState,
            start_ref=Ref}) ->

    State2 = case Msg of
        {done, LastSeq} ->
            State#gen_changes_state{last_seq=LastSeq};
        Row ->
            Seq = couchbeam_doc:get_value(<<"seq">>, Row),
            State#gen_changes_state{last_seq=Seq}
    end,

    case catch Module:handle_change(Msg, ModState) of
        {noreply, NewModState} ->
            {noreply, State2#gen_changes_state{modstate=NewModState}};
        {noreply, NewModState, A} when A =:= hibernate orelse is_number(A) ->
            {noreply, State2#gen_changes_state{modstate=NewModState}, A};
        {stop, Reason, NewModState} ->
            {stop, Reason, State2#gen_changes_state{modstate=NewModState}}
    end;


handle_info({error, Ref, LastSeq, Error},
        State=#gen_changes_state{start_ref=Ref}) ->
    handle_info({error, {LastSeq, Error}}, State);

handle_info({'DOWN', MRef, process, Pid, _},
        State=#gen_changes_state{changes_pid=Pid, options=Options,
            last_seq=LastSeq, db=Db}) ->

    %% stop monitoring db
    erlang:demonitor(MRef, [flush]),

    %% we restart the connection if we are using longpolling or
    %% continuous feeds
    Longpoll = proplists:get_value(longpoll, Options) ,
    Continuous = proplists:get_value(continuous, Options),
    if
        Longpoll == true orelse Continuous == true ->
            Self = self(),
            Options1 = case proplists:get_value(since, Options) of
                undefined ->
                    [{since, LastSeq}|Options];
                _ ->
                    [{since, LastSeq}|proplists:delete(since, Options)]
            end,
            case couchbeam_changes:stream(Db, Self, Options1) of
            {ok, StartRef, ChangesPid} ->
                erlang:monitor(process, ChangesPid),
                unlink(ChangesPid),
                {noreply, State#gen_changes_state{start_ref=StartRef,
                                        changes_pid=ChangesPid,
                                        options=Options1}};
            Error ->
                {stop, Error, State}
        end;
        true ->
            {stop, done, State}
    end;

handle_info(Info, State=#gen_changes_state{mod=Module, modstate=ModState}) ->
    case Module:handle_info(Info, ModState) of
        {noreply, NewModState} ->
            {noreply, State#gen_changes_state{modstate=NewModState}};
        {noreply, NewModState, A} when A =:= hibernate orelse is_number(A) ->
            {noreply, State#gen_changes_state{modstate=NewModState}, A};
        {stop, Reason, NewModState} ->
            {stop, Reason, State#gen_changes_state{modstate=NewModState}}
    end.

code_change(_OldVersion, State, _Extra) ->
    %% TODO:  support code changes?
    {ok, State}.

terminate(Reason, #gen_changes_state{changes_pid=Pid,
        mod=Module, modstate=ModState}) ->
    Module:terminate(Reason, ModState),
    catch exit(Pid, normal),
    ok.
