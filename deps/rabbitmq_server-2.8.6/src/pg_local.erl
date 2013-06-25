%% This file is a copy of pg2.erl from the R13B-3 Erlang/OTP
%% distribution, with the following modifications:
%%
%% 1) Process groups are node-local only.
%%
%% 2) Groups are created/deleted implicitly.
%%
%% 3) 'join' and 'leave' are asynchronous.
%%
%% 4) the type specs of the exported non-callback functions have been
%%    extracted into a separate, guarded section, and rewritten in
%%    old-style spec syntax, for better compatibility with older
%%    versions of Erlang/OTP. The remaining type specs have been
%%    removed.

%% All modifications are (C) 2010-2013 VMware, Inc.

%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
-module(pg_local).

-export([join/2, leave/2, get_members/1]).
-export([sync/0]). %% intended for testing only; not part of official API
-export([start/0, start_link/0, init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2]).

%%----------------------------------------------------------------------------

-ifdef(use_specs).

-type(name() :: term()).

-spec(start_link/0 :: () -> {'ok', pid()} | {'error', any()}).
-spec(start/0 :: () -> {'ok', pid()} | {'error', any()}).
-spec(join/2 :: (name(), pid()) -> 'ok').
-spec(leave/2 :: (name(), pid()) -> 'ok').
-spec(get_members/1 :: (name()) -> [pid()]).

-spec(sync/0 :: () -> 'ok').

-endif.

%%----------------------------------------------------------------------------

%%% As of R13B03 monitors are used instead of links.

%%%
%%% Exported functions
%%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start() ->
    ensure_started().

join(Name, Pid) when is_pid(Pid) ->
    ensure_started(),
    gen_server:cast(?MODULE, {join, Name, Pid}).

leave(Name, Pid) when is_pid(Pid) ->
    ensure_started(),
    gen_server:cast(?MODULE, {leave, Name, Pid}).

get_members(Name) ->
    ensure_started(),
    group_members(Name).

sync() ->
    ensure_started(),
    gen_server:call(?MODULE, sync, infinity).

%%%
%%% Callback functions from gen_server
%%%

-record(state, {}).

init([]) ->
    pg_local_table = ets:new(pg_local_table, [ordered_set, protected, named_table]),
    {ok, #state{}}.

handle_call(sync, _From, S) ->
    {reply, ok, S};

handle_call(Request, From, S) ->
    error_logger:warning_msg("The pg_local server received an unexpected message:\n"
                             "handle_call(~p, ~p, _)\n", 
                             [Request, From]),
    {noreply, S}.

handle_cast({join, Name, Pid}, S) ->
    join_group(Name, Pid),
    {noreply, S};
handle_cast({leave, Name, Pid}, S) ->
    leave_group(Name, Pid),
    {noreply, S};
handle_cast(_, S) ->
    {noreply, S}.

handle_info({'DOWN', MonitorRef, process, _Pid, _Info}, S) ->
    member_died(MonitorRef),
    {noreply, S};
handle_info(_, S) ->
    {noreply, S}.

terminate(_Reason, _S) ->
    true = ets:delete(pg_local_table),
    ok.

%%%
%%% Local functions
%%%

%%% One ETS table, pg_local_table, is used for bookkeeping. The type of the
%%% table is ordered_set, and the fast matching of partially
%%% instantiated keys is used extensively.
%%%
%%% {{ref, Pid}, MonitorRef, Counter}
%%% {{ref, MonitorRef}, Pid}
%%%    Each process has one monitor. Counter is incremented when the
%%%    Pid joins some group.
%%% {{member, Name, Pid}, _}
%%%    Pid is a member of group Name, GroupCounter is incremented when the
%%%    Pid joins the group Name.
%%% {{pid, Pid, Name}}
%%%    Pid is a member of group Name.

member_died(Ref) ->
    [{{ref, Ref}, Pid}] = ets:lookup(pg_local_table, {ref, Ref}),
    Names = member_groups(Pid),
    _ = [leave_group(Name, P) || 
            Name <- Names,
            P <- member_in_group(Pid, Name)],
    ok.

join_group(Name, Pid) ->
    Ref_Pid = {ref, Pid}, 
    try _ = ets:update_counter(pg_local_table, Ref_Pid, {3, +1})
    catch _:_ ->
            Ref = erlang:monitor(process, Pid),
            true = ets:insert(pg_local_table, {Ref_Pid, Ref, 1}),
            true = ets:insert(pg_local_table, {{ref, Ref}, Pid})
    end,
    Member_Name_Pid = {member, Name, Pid},
    try _ = ets:update_counter(pg_local_table, Member_Name_Pid, {2, +1})
    catch _:_ ->
            true = ets:insert(pg_local_table, {Member_Name_Pid, 1}),
            true = ets:insert(pg_local_table, {{pid, Pid, Name}})
    end.

leave_group(Name, Pid) ->
    Member_Name_Pid = {member, Name, Pid},
    try ets:update_counter(pg_local_table, Member_Name_Pid, {2, -1}) of
        N ->
            if 
                N =:= 0 ->
                    true = ets:delete(pg_local_table, {pid, Pid, Name}),
                    true = ets:delete(pg_local_table, Member_Name_Pid);
                true ->
                    ok
            end,
            Ref_Pid = {ref, Pid}, 
            case ets:update_counter(pg_local_table, Ref_Pid, {3, -1}) of
                0 ->
                    [{Ref_Pid,Ref,0}] = ets:lookup(pg_local_table, Ref_Pid),
                    true = ets:delete(pg_local_table, {ref, Ref}),
                    true = ets:delete(pg_local_table, Ref_Pid),
                    true = erlang:demonitor(Ref, [flush]),
                    ok;
                _ ->
                    ok
            end
    catch _:_ ->
            ok
    end.

group_members(Name) ->
    [P || 
        [P, N] <- ets:match(pg_local_table, {{member, Name, '$1'},'$2'}),
        _ <- lists:seq(1, N)].

member_in_group(Pid, Name) ->
    [{{member, Name, Pid}, N}] = ets:lookup(pg_local_table, {member, Name, Pid}),
    lists:duplicate(N, Pid).

member_groups(Pid) ->
    [Name || [Name] <- ets:match(pg_local_table, {{pid, Pid, '$1'}})].

ensure_started() ->
    case whereis(?MODULE) of
        undefined ->
            C = {pg_local, {?MODULE, start_link, []}, permanent,
                 16#ffffffff, worker, [?MODULE]},
            supervisor:start_child(kernel_safe_sup, C);
        PgLocalPid ->
            {ok, PgLocalPid}
    end.
