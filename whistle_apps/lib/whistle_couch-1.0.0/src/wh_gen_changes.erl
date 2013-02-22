%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license.
%%% See the NOTICE for more information.

%% @doc gen_changes CouchDB continuous changes consumer behavior
%% This behaviour allws you to create easily a server that consume
%% Couchdb continuous changes

-module(wh_gen_changes).

-include_lib("wh_couch.hrl").

-behavior(gen_server).

-export([start_link/4]).
-export([server_name/1]).
-export([stop/1]).
-export([get_seq/1]).
-export([call/2,
         call/3,
         cast/2
        ]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{init, 1},
     {handle_change, 2},
     {handle_call, 3},
     {handle_cast, 2},
     {handle_info, 2},
     {terminate, 2}];
behaviour_info(_) ->
    undefined.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% create a gen_changes process as part of a supervision tree.
%% The function should be called, directly or indirectly, by the supervisor.
%% @end
%%--------------------------------------------------------------------
-type changeoption() :: {'include_docs', string()}
                      | {'filter', string()}
                      | {'since', integer()|string()}
                      | {'heartbeat', string()|boolean()|pos_integer()}
                      | 'longpoll'.
-type changesoptions() :: [changeoption()].
-spec start_link(atom(), couchbeam_db(), changesoptions(), list()) -> term().
start_link(Module, Db, Options, InitArgs) ->
    gen_server:start_link({'local', server_name(Db)}, ?MODULE
                          ,[Module, Db, Options, InitArgs], []
                         ).

-spec server_name(text() | couchbeam_db()) -> atom().
server_name(#db{name=DbName}) -> server_name(DbName);
server_name(DbName) ->
    wh_util:to_atom(<<"wh_gen_changes_", (wh_util:to_binary(DbName))/binary>>, 'true').

-spec stop(pid()) -> 'ok'.
stop(Pid) when is_pid(Pid) -> gen_server:cast(Pid, 'stop').

-spec get_seq(pid()) -> list().
get_seq(Pid) when is_pid(Pid) -> gen_server:call(Pid, '$get_seq').

-spec call(pid() | atom(), _) -> _.
call(Name, Request) -> gen_server:call(Name, Request).

-spec call(pid() | atom(), _, wh_timeout()) -> _.
call(Name, Request, Timeout) -> gen_server:call(Name, Request, Timeout).

-spec cast(pid() | atom(), _) -> 'ok'.
cast(Dest, Request) -> gen_server:cast(Dest, Request).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Module, Db, Options, InitArgs]) ->
    case Module:init(InitArgs) of
        {ok, ModState} ->
            wh_gen_changes:cast(self(), '$start_change_feed'),
            {ok, #gen_changes_state{mod=Module
                                    ,modstate=ModState
                                    ,db=Db
                                    ,options=proplists:delete(since, Options)
                                    ,last_seq=proplists:get_value(since, Options)}};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call('$get_seq', _From, State=#gen_changes_state{last_seq=Seq}) ->
    {reply, Seq, State};
handle_call(Request, From, State=#gen_changes_state{mod=Module, modstate=ModState}) ->
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast('$start_change_feed', State=#gen_changes_state{last_seq=undefined, db=Db}) ->
    try get_update_seq(Db) of
        UpdateSeq ->
            gen_server:cast(self(), '$start_change_feed'),
            {noreply, State#gen_changes_state{last_seq=UpdateSeq}}
    catch
        _:_ ->
            erlang:send_after(random_wait(), self(), '$try_change_feed'),
            {noreply, State}
    end;
handle_cast('$start_change_feed', State=#gen_changes_state{db=Db, options=Options}) ->
    case couchbeam_changes:stream(Db, self(), update_since(Options, State)) of
        {ok, StartRef, ChangesPid} ->
            erlang:monitor(process, ChangesPid),
            unlink(ChangesPid),
            {noreply, State#gen_changes_state{start_ref=StartRef
                                              ,changes_pid=ChangesPid}};
        {error, Error} ->
            {stop, Error, State}
    end;
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info('$try_change_feed', State) ->
    gen_server:cast(self(), '$start_change_feed'),
    {noreply, State};
handle_info('$poll_for_changes', State=#gen_changes_state{last_seq='undefined', db=Db}) ->
    io:format("started polling ~s for changes~n", [Db#db.name]),
    try get_update_seq(Db) of
        UpdateSeq ->
            self() ! '$poll_for_changes',
            {noreply, State#gen_changes_state{last_seq=UpdateSeq}}
    catch
        _:_ ->
            erlang:send_after(random_wait(), self(), '$poll_for_changes'),
            {noreply, State}
    end;
handle_info('$poll_for_changes', State=#gen_changes_state{db=Db, options=Options
                                                          ,mod=Module, modstate=ModState}) ->
    case couchbeam_changes:fetch(Db, update_since(Options, State)) of
        {ok, _, []} ->
            erlang:send_after(random_wait(), self(), '$poll_for_changes'),
            {noreply, State};
        {ok, [_, _]=LastSeq, Rows} ->
            erlang:send_after(random_wait(), self(), '$poll_for_changes'),
            case catch Module:handle_change(Rows, ModState) of
                {noreply, NewModState} ->
                    {noreply, State#gen_changes_state{modstate=NewModState, last_seq=LastSeq}};
                {noreply, NewModState, A} when A =:= hibernate orelse is_number(A) ->
                    {noreply, State#gen_changes_state{modstate=NewModState, last_seq=LastSeq}, A};
                {stop, Reason, NewModState} ->
                    {stop, Reason, State#gen_changes_state{modstate=NewModState, last_seq=LastSeq}}
            end;
        _Else ->
            erlang:send_after(random_wait(), self(), '$poll_for_changes'),
            {noreply, State}
    end;
handle_info({error, Ref, LastSeq, Error}, State=#gen_changes_state{start_ref=Ref}) ->
    handle_info({error, {LastSeq, Error}}, State);
handle_info({change, Ref, Msg}, State=#gen_changes_state{mod=Module, modstate=ModState
                                                         ,start_ref=Ref}) ->
    State2 = case Msg of
                 {done, LastSeq} ->
                     maybe_update_seq(LastSeq, State);
                 Row ->
                     Seq = couchbeam_doc:get_value(<<"seq">>, Row),
                     maybe_update_seq(Seq, State)
             end,
    case catch Module:handle_change(Msg, ModState) of
        {noreply, NewModState} ->
            {noreply, State2#gen_changes_state{modstate=NewModState}};
        {noreply, NewModState, A} when A =:= hibernate orelse is_number(A) ->
            {noreply, State2#gen_changes_state{modstate=NewModState}, A};
        {stop, Reason, NewModState} ->
            {stop, Reason, State2#gen_changes_state{modstate=NewModState}}
    end;
handle_info({'DOWN', MRef, process, Pid, _}, State=#gen_changes_state{changes_pid=Pid, db=Db
                                                                      ,options=Options}) ->
    %% stop monitoring db
    erlang:demonitor(MRef, [flush]),
    %% we restart the connection if we are using longpolling or
    %% continuous feeds
    Longpoll = proplists:get_value(longpoll, Options) ,
    Continuous = proplists:get_value(continuous, Options),
    case Longpoll == true orelse Continuous == true of
        false -> {stop, done, State};
        true ->
            Server = wh_couch_connections:get_server(),
            gen_server:cast(self(), '$start_change_feed'),
            {noreply, State#gen_changes_state{db=Db#db{server=Server}
                                              ,last_seq=undefined}}
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

maybe_update_seq([_, _]=Seq, State) ->
    State#gen_changes_state{last_seq=Seq};
maybe_update_seq(_, State) -> State.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(Reason, #gen_changes_state{mod=Module, modstate=ModState
                                     ,changes_pid=Pid}) ->
    Module:terminate(Reason, ModState),
    catch exit(Pid, normal),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVersion, State, _Extra) ->
    %% TODO:  support code changes?
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec get_update_seq(couchbeam_db()) -> list() | 'undefined'.
get_update_seq(#db{server='undefined'}) -> 'undefined';
get_update_seq(#db{server=Server, name=DbName}) ->
    {'ok', JObj} = couch_util:db_info(Server, DbName),
    couchbeam_doc:get_value(<<"update_seq">>, JObj).

-spec update_since(wh_proplist(), #gen_changes_state{}) -> wh_proplist().
update_since(Options, #gen_changes_state{last_seq=[SeqNumber, SeqHash]}) ->
    Seq = <<"[", (wh_util:to_binary(SeqNumber))/binary, ",\"", SeqHash/binary, "\"]">>,
    [{since, wh_util:to_list(Seq)} | Options];
update_since(Options, #gen_changes_state{last_seq=Seq}) ->
    [{since, Seq} | Options].

-spec random_wait() -> 10000..120000.
random_wait() -> crypto:rand_uniform(10000, 120000).
