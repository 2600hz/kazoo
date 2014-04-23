%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Carlos Gonzalez Florido.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc Call Distribution Router
-module(nksip_call_router).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behaviour(gen_server).

-export([incoming_sync/1]).
-export([apply_dialog/2, get_all_dialogs/0, get_all_dialogs/2]).
-export([apply_sipmsg/2]).
-export([apply_transaction/2, get_all_transactions/0, get_all_transactions/2]).
-export([get_all_calls/0, get_all_data/0, get_all_info/0, clear_all_calls/0]).
-export([pending_msgs/0, pending_work/0]).
-export([send_work_sync/3, send_work_async/3]).
-export([pos2name/1, start_link/2]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2,
            handle_info/2]).

-export_type([sync_error/0]).

-include("nksip.hrl").
-include("nksip_call.hrl").

% -define(SYNC_TIMEOUT, 45000).
-define(SYNC_TIMEOUT, 10000).


%% TODO: If there are several pending messages and the process stops,
%% it will resend the messages in reverse order. The last monitor
%% comes first


%% ===================================================================
%% Types
%% ===================================================================


-type sync_error() :: sipapp_not_found | too_many_calls | timeout | loop_detected.


%% ===================================================================
%% Public
%% ===================================================================

% %% @doc Called when a new request or response has been received.
% incoming_async(#sipmsg{call_id=CallId}=SipMsg) ->
%     gen_server:cast(name(CallId), {incoming, SipMsg}).


%% @doc Called when a new request or response has been received.
incoming_sync(#sipmsg{app_id=AppId, call_id=CallId}=SipMsg) ->
    case catch gen_server:call(name(CallId), {incoming, SipMsg}, ?SYNC_TIMEOUT) of
        {'EXIT', Error} -> 
            ?warning(AppId, CallId, "error calling incoming_sync: ~p", [Error]),
            {error, timeout};
        Result -> 
            Result
    end.


%% @doc Applies a fun to a dialog and returns the result.
-spec apply_dialog(nksip:id(), function()) ->
    term() | {error, Error}
    when Error :: unknown_dialog | sync_error().

apply_dialog(Id, Fun) ->
    {AppId, DialogId, CallId} = nksip_dialog:parse_id(Id),
    send_work_sync(AppId, CallId, {apply_dialog, DialogId, Fun}).

%% @doc Get all dialog ids for all calls.
-spec get_all_dialogs() ->
    [nksip:id()].

get_all_dialogs() ->
    lists:flatten([
        get_all_dialogs(AppId, CallId)
        || 
        {AppId, CallId, _} <- get_all_calls()
    ]).


%% @doc Get all dialog ids for this SipApp, having CallId.
-spec get_all_dialogs(nksip:app_id(), nksip:call_id()) ->
    [nksip:id()].

get_all_dialogs(AppId, CallId) ->
    case send_work_sync(AppId, CallId, get_all_dialogs) of
        {ok, Ids} -> Ids;
        _ -> []
    end.


%% @doc Applies a fun to a SipMsg and returns the result.
-spec apply_sipmsg(nksip:id(), function()) ->
    term() | {error, Error}
    when Error :: unknown_sipmsg | invalid_id | sync_error().

apply_sipmsg(Id, Fun) ->
    case nksip_sipmsg:parse_id(Id) of
        {Class, AppId, MsgId, CallId} when Class==req; Class==resp->
            send_work_sync(AppId, CallId, {apply_sipmsg, MsgId, Fun})
    end.


%% @doc Applies a fun to a transaction and returns the result.
-spec apply_transaction(nksip:id(), function()) ->
    term() | {error, Error}
    when Error :: unknown_transaction | sync_error().

apply_transaction(Id, Fun) ->
    case nksip_sipmsg:parse_id(Id) of
        {Class, AppId, MsgId, CallId} when Class==req; Class==resp->
            send_work_sync(AppId, CallId, {apply_transaction, MsgId, Fun})
    end.
            


%% @doc Get all active transactions for all calls.
-spec get_all_transactions() ->
    [{nksip:app_id(), nksip:call_id(), uac, nksip_call_uac:id()} |
     {nksip:app_id(), nksip:call_id(), uas, nksip_call_uas:id()}].

get_all_transactions() ->
    lists:flatten(
        [
            [{AppId, CallId, Class, Id} 
                || {Class, Id} <- get_all_transactions(AppId, CallId)]
            || {AppId, CallId, _} <- get_all_calls()
        ]).


%% @doc Get all active transactions for this SipApp, having CallId.
-spec get_all_transactions(nksip:app_id(), nksip:call_id()) ->
    [{uac, nksip_call_uac:id()} | {uas, nksip_call_uas:id()}].

get_all_transactions(AppId, CallId) ->
    case send_work_sync(AppId, CallId, get_all_transactions) of
        {ok, Ids} -> Ids;
        _ -> []
    end.


%% @doc Get all started calls.
-spec get_all_calls() ->
    [{nksip:app_id(), nksip:call_id(), pid()}].

get_all_calls() ->
    Fun = fun(Name, Acc) -> [call_fold(Name)|Acc] end,
    lists:flatten(router_fold(Fun)).


%% @doc Get all started calls.
-spec get_all_info() ->
    [term()].

get_all_info() ->
    lists:sort(lists:flatten([send_work_sync(AppId, CallId, info)
        || {AppId, CallId, _} <- get_all_calls()])).


%% @doc Removes all calls, dialogs, transactions and forks.
clear_all_calls() ->
    lists:foreach(fun({_, _, Pid}) -> nksip_call_srv:stop(Pid) end, get_all_calls()).    


%% @private
get_all_data() ->
    [
        {AppId, CallId, nksip_call_srv:get_data(Pid)}
        || {AppId, CallId, Pid} <- get_all_calls()
    ].


%% @private
pending_work() ->
    router_fold(fun(Name, Acc) -> Acc+gen_server:call(Name, pending) end, 0).


%% @private
pending_msgs() ->
    router_fold(
        fun(Name, Acc) ->
            Pid = whereis(Name),
            {_, Len} = erlang:process_info(Pid, message_queue_len),
            Acc + Len
        end,
        0).


%% ===================================================================
%% gen_server
%% ===================================================================


-record(state, {
    pos :: integer(),
    name :: atom(),
    pending :: dict(),
    max_calls :: integer()
}).


%% @private
start_link(Pos, Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [Pos, Name], []).
        
%% @private 
-spec init(term()) ->
    gen_server_init(#state{}).

init([Pos, Name]) ->
    Name = ets:new(Name, [named_table, protected]),
    SD = #state{
        pos = Pos, 
        name = Name, 
        pending = dict:new(),
        max_calls = nksip_config:get(max_calls)
    },
    {ok, SD}.


%% @private
-spec handle_call(term(), from(), #state{}) ->
    gen_server_call(#state{}).

handle_call({send_work_sync, AppId, CallId, Work, Caller}, From, SD) ->
    case send_work_sync(AppId, CallId, Work, Caller, From, SD) of
        {ok, SD1} -> 
            {noreply, SD1};
        {error, Error} ->
            ?error(AppId, CallId, "error sending work ~p: ~p", [Work, Error]),
            {reply, {error, Error}, SD}
    end;

handle_call({incoming, SipMsg}, _From, SD) ->
    #sipmsg{app_id=AppId, call_id=CallId} = SipMsg,
    case send_work_sync(AppId, CallId, {incoming, SipMsg}, none, none, SD) of
        {ok, SD1} -> 
            {reply, ok, SD1};
        {error, Error} ->
            ?error(AppId, CallId, "error processing incoming message: ~p", [Error]),
            {reply, {error, Error}, SD}
    end;

handle_call(pending, _From, #state{pending=Pending}=SD) ->
    {reply, dict:size(Pending), SD};

handle_call(Msg, _From, SD) -> 
    lager:error("Module ~p received unexpected call ~p", [?MODULE, Msg]),
    {noreply, SD}.


%% @private
-spec handle_cast(term(), #state{}) ->
    gen_server_cast(#state{}).

handle_cast({incoming, SipMsg}, SD) ->
    #sipmsg{app_id=AppId, call_id=CallId} = SipMsg,
    case send_work_sync(AppId, CallId, {incoming, SipMsg}, none, none, SD) of
        {ok, SD1} -> 
            {noreply, SD1};
        {error, Error} ->
            ?error(AppId, CallId, "error processing incoming message: ~p", [Error]),
            {noreply, SD}
    end;

handle_cast(Msg, SD) ->
    lager:error("Module ~p received unexpected event: ~p", [?MODULE, Msg]),
    {noreply, SD}.


%% @private
-spec handle_info(term(), #state{}) ->
    gen_server_info(#state{}).

handle_info({sync_work_ok, Ref}, #state{pending=Pending}=SD) ->
    erlang:demonitor(Ref),
    Pending1 = dict:erase(Ref, Pending),
    {noreply, SD#state{pending=Pending1}};

handle_info({'DOWN', MRef, process, Pid, _Reason}, SD) ->
    #state{pos=Pos, name=Name, pending=Pending} = SD,
    case ets:lookup(Name, Pid) of
        [{Pid, Id}] ->
            {_, AppId0, CallId0} = Id,
            ?debug(AppId0, CallId0, "router ~p unregistering call", [Pos]),
            ets:delete(Name, Pid), 
            ets:delete(Name, Id);
        [] ->
            ok
    end,
    case dict:find(MRef, Pending) of
        {ok, {AppId, CallId, From, Work}} -> 
            % We had pending work for this process.
            % Actually, we know the process has stopped normally (it hasn't failed).
            % If the process had failed due to an error processing the work,
            % the "received work" message would have been received an the work will
            % not be present in Pending.
            Pending1 = dict:erase(MRef, Pending),
            SD1 = SD#state{pending=Pending1},
            case send_work_sync(AppId, CallId, Work, none, From, SD1) of
                {ok, SD2} -> 
                    ?debug(AppId, CallId, "resending work ~p from ~p", 
                           [work_id(Work), From]),
                    {noreply, SD2};
                {error, Error} ->
                    case From of
                        {Pid, Ref} when is_pid(Pid), is_reference(Ref) ->
                            gen_server:reply(From, {error, Error});
                        _ ->
                            ok
                    end,
                    {noreply, SD}
            end;
        error ->
            {noreply, SD}
    end;

handle_info(Info, SD) -> 
    lager:warning("Module ~p received unexpected info: ~p", [?MODULE, Info]),
    {noreply, SD}.


%% @private
-spec code_change(term(), #state{}, term()) ->
    gen_server_code_change(#state{}).

code_change(_OldVsn, SD, _Extra) ->
    {ok, SD}.


%% @private
-spec terminate(term(), #state{}) ->
    gen_server_terminate().

terminate(_Reason, _SD) ->  
    ok.


%% ===================================================================
%% Internal
%% ===================================================================

%% @doc Sends a synchronous piece of {@link nksip_call:work()} to a call.
-spec send_work_sync(nksip:app_id(), nksip:call_id(), nksip_call:work()) ->
    any() | {error, sync_error()}.

send_work_sync(AppId, CallId, Work) ->
    WorkSpec = {send_work_sync, AppId, CallId, Work, self()},
    case catch gen_server:call(name(CallId), WorkSpec, ?SYNC_TIMEOUT) of
        {'EXIT', Error} ->
            ?warning(AppId, CallId, "error calling send_work_sync (~p): ~p",
                     [work_id(Work), Error]),
            {error, timeout};
        Other ->
            Other
    end.


%% @private 
-spec name(nksip:call_id()) ->
    atom().

name(CallId) ->
    Pos = erlang:phash2(CallId) rem ?MSG_ROUTERS,
    pos2name(Pos).


%% @private
-spec pos2name(integer()) -> 
    atom().

pos2name(Pos) ->
    list_to_atom("nksip_call_router_"++integer_to_list(Pos)).


%% @private
-spec send_work_sync(nksip:app_id(), nksip:call_id(), nksip_call:work(), 
                     pid() | none, from(), #state{}) ->
    {ok, #state{}} | {error, sync_error()}.

send_work_sync(AppId, CallId, Work, Caller, From, SD) ->
    #state{name=Name, pending=Pending} = SD,
    case find(Name, AppId, CallId) of
        {ok, Caller} ->
            {error, loop_detected};
        {ok, Pid} -> 
            Ref = erlang:monitor(process, Pid),
            Self = self(),
            nksip_call_srv:sync_work(Pid, Ref, Self, Work, From),
            Pending1 = dict:store(Ref, {AppId, CallId, From, Work}, Pending),
            {ok, SD#state{pending=Pending1}};
        not_found ->
            case do_call_start(AppId, CallId, SD) of
                {ok, SD1} -> send_work_sync(AppId, CallId, Work, Caller, From, SD1);
                {error, Error} -> {error, Error}
            end
   end.


%% @private
-spec do_call_start(nksip:app_id(), nksip:call_id(), #state{}) ->
    {ok, #state{}} | {error, sipapp_not_found | too_many_calls}.

do_call_start(AppId, CallId, SD) ->
    #state{name=Name, max_calls=MaxCalls} = SD,
    case nksip_counters:value(nksip_calls) < MaxCalls of
        true ->
            {ok, Pid} = nksip_call_srv:start(AppId, CallId),
            erlang:monitor(process, Pid),
            Id = {call, AppId, CallId},
            true = ets:insert(Name, [{Id, Pid}, {Pid, Id}]),
            {ok, SD};
        false ->
            {error, too_many_calls}
    end.


%% @doc Sends an asynchronous piece of {@link nksip_call:work()} to a call.
-spec send_work_async(nksip:app_id(), nksip:call_id(), nksip_call:work()) ->
    ok.

send_work_async(AppId, CallId, Work) ->
    send_work_async(name(CallId), AppId, CallId, Work).


%% @private Sends an asynchronous piece of {@link nksip_call:work()} to a call.
-spec send_work_async(atom(), nksip:app_id(), nksip:call_id(), nksip_call:work()) ->
    ok.

send_work_async(Name, AppId, CallId, Work) ->
    case find(Name, AppId, CallId) of
        {ok, Pid} -> 
            nksip_call_srv:async_work(Pid, Work);
        not_found -> 
            ?info(AppId, CallId, "trying to send work ~p to deleted call", 
                  [work_id(Work)])
   end.


%% @private
-spec find(atom(), nksip:app_id(), nksip:call_id()) ->
    {ok, pid()} | not_found.

find(Name, AppId, CallId) ->
    case ets:lookup(Name, {call, AppId, CallId}) of
        [{_, Pid}] -> {ok, Pid};
        [] -> not_found
    end.

%% @private
work_id(Tuple) when is_tuple(Tuple) -> element(1, Tuple);
work_id(Other) -> Other.


%% @private
router_fold(Fun) ->
    router_fold(Fun, []).

router_fold(Fun, Init) ->
    lists:foldl(
        fun(Pos, Acc) -> Fun(pos2name(Pos), Acc) end,
        Init,
        lists:seq(0, ?MSG_ROUTERS-1)).

%% @private
call_fold(Name) ->
    ets:foldl(
        fun(Record, Acc) ->
            case Record of
                {{call, AppId, CallId}, Pid} when is_pid(Pid) ->
                    [{AppId, CallId, Pid}|Acc];
                _ ->
                    Acc
            end
        end,
        [],
        Name).