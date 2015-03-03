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

%% @private Call Management Module
%%
%% A new {@link nksip_call_srv} process is started for every different
%% Call-ID request or response, incoming or outcoming.
%% This module offers an interface to this process.

-module(nksip_call).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([app_id/1, call_id/1]).
-export([send/2, send/5, send_dialog/5, send_cancel/4]).
-export([send_reply/4]).
-export([get_all/0, get_info/0, clear_all/0]).
-export([get_all_dialogs/0, get_all_dialogs/2, apply_dialog/4, stop_dialog/3]).
-export([get_authorized_list/3, clear_authorized_list/3]).
-export([get_all_transactions/0, get_all_transactions/2, apply_transaction/4]).
-export([apply_sipmsg/4]).
-export([check_call/1]).
-export_type([call/0, trans/0, trans_id/0, fork/0]).

-include("nksip.hrl").
-include("nksip_call.hrl").


%% ===================================================================
%% Types
%% ===================================================================


-type call() :: #call{}.

-type trans() :: #trans{}.

-type trans_id() :: integer().

-type fork() :: #fork{}.



%% ===================================================================
%% Public
%% ===================================================================

%% @doc Gets the AppId
-spec app_id(call()) ->
    nksip:app_id().

app_id(#call{app_id=AppId}) ->
    AppId.


%% @doc Gets the CallId
-spec call_id(call()) ->
    nksip:call_id().

call_id(#call{call_id=CallId}) ->
    CallId.


%% ===================================================================
%% Internal
%% ===================================================================

%% @private Sends a new request.
-spec send(nksip:request(), nksip:optslist()) ->
    nksip_uac:uac_result() | nksip_uac:uac_ack_result().

send(#sipmsg{app_id=AppId, call_id=CallId}=Req, Opts) ->
    nksip_router:send_work_sync(AppId, CallId, {send, Req, Opts}).


%% @private Generates and sends a new request.
-spec send(nksip:app_id(), nksip:call_id(), nksip:method(), 
           nksip:user_uri(), nksip:optslist()) ->
    nksip_uac:uac_result() | nksip_uac:uac_ack_result().

send(AppId, CallId, Method, Uri, Opts) ->
    nksip_router:send_work_sync(AppId, CallId, {send, Method, Uri, Opts}).


%% @private Generates and sends a new in-dialog request.
-spec send_dialog(nksip:app_id(), nksip:call_id(), nksip:method(), 
                  nksip_dialog_lib:id(), nksip:optslist()) ->
    nksip_uac:uac_result() | nksip_uac:uac_ack_result().

send_dialog(AppId, CallId, Method, DialogId, Opts) ->
    nksip_router:send_work_sync(AppId, CallId, {send_dialog, DialogId, Method, Opts}).


%% @private Cancels an ongoing INVITE request.
-spec send_cancel(nksip:app_id(), nksip:call_id(), nksip_sipmsg:id(),
                  nksip:optslist()) ->
    nksip_uac:uac_cancel_result().

send_cancel(AppId, CallId, RequestId, Opts) ->
    nksip_router:send_work_sync(AppId, CallId, {send_cancel, RequestId, Opts}).



%% @private Sends a synchronous request reply.
-spec send_reply(nksip:app_id(), nksip:call_id(), nksip_sipmsg:id(), 
                 nksip:sipreply()) ->
    {ok, nksip:response()} | {error, term()}.

send_reply(AppId, CallId, ReqId, SipReply) ->
    nksip_router:send_work_sync(AppId, CallId, {send_reply, ReqId, SipReply}).
    

%% @private Get all started calls (dangerous in production with many calls)
-spec get_all() ->
    [{nksip:app_id(), nksip:call_id(), pid()}].

get_all() ->
    nksip_router:get_all_calls().


%% @private Get information about all started calls (dangerous in production with many calls)
-spec get_info() ->
    [term()].

get_info() ->
    lists:sort(lists:flatten([nksip_router:send_work_sync(AppId, CallId, info)
        || {AppId, CallId, _} <- nksip_router:get_all_calls()])).


%% @private Deletes all started calls
-spec clear_all() ->
    pos_integer().

clear_all() ->
    lists:foldl(
        fun({_, _, Pid}, Acc) -> nksip_call_srv:stop(Pid), Acc+1 end, 
        0,
        nksip_router:get_all_calls()).    


%% @private Gets all started dialog handles (dangerous in production with many calls)
-spec get_all_dialogs() ->
    [nksip:handle()].

get_all_dialogs() ->
    lists:flatten([
        case get_all_dialogs(AppId, CallId) of
            {ok, Handles} -> Handles;
            {error, _} -> []
        end
        || 
        {AppId, CallId, _} <- nksip_router:get_all_calls()
    ]).


%% @private Finds all started dialogs handles having a `Call-ID'.
-spec get_all_dialogs(nksip:app_id(), nksip:call_id()) ->
    {ok, [nksip:handle()]} | {error, term()}.

get_all_dialogs(AppId, CallId) ->
    nksip_router:send_work_sync(AppId, CallId, get_all_dialogs).


%% @private Deletes a dialog
-spec stop_dialog(nksip:app_id(), nksip:call_id(), nksip_dialog_lib:id()) ->
    ok | {error, term()}.
 
stop_dialog(AppId, CallId, DialogId) ->
    nksip_router:send_work_sync(AppId, CallId, {stop_dialog, DialogId}).


%% @private
-spec apply_dialog(nksip:app_id(), nksip:call_id(), nksip_dialog_lib:id(), function()) ->
    {apply, term()} | {error, term()}.

apply_dialog(AppId, CallId, DialogId, Fun) ->
    nksip_router:send_work_sync(AppId, CallId, {apply_dialog, DialogId, Fun}).


%% @private Gets authorized list of transport, ip and ports for a dialog.
-spec get_authorized_list(nksip:app_id(), nksip:call_id(), nksip_dialog_lib:id()) ->
    {ok, [{nksip:protocol(), inet:ip_address(), inet:port_number()}]} | {error, term()}.

get_authorized_list(AppId, CallId, DialogId) ->
    nksip_router:send_work_sync(AppId, CallId, {get_authorized_list, DialogId}).


%% @private Gets authorized list of transport, ip and ports for a dialog.
-spec clear_authorized_list(nksip:app_id(), nksip:call_id(), nksip_dialog_lib:id()) ->
    ok | {error, term()}.

clear_authorized_list(AppId, CallId, DialogId) ->
    nksip_router:send_work_sync(AppId, CallId, {clear_authorized_list, DialogId}).


%% @private Get all active transactions for all calls.
-spec get_all_transactions() ->
    [{nksip:app_id(), nksip:call_id(), uac, nksip_call:trans_id()} |
     {nksip:app_id(), nksip:call_id(), uas, nksip_call:trans_id()}].
    
get_all_transactions() ->
    lists:flatten(
        [
            case get_all_transactions(AppId, CallId) of
                {ok, List} ->
                    [{AppId, CallId, Class, Id} || {Class, Id} <- List];
                {error, _} ->
                    []
            end
            || {AppId, CallId, _} <- nksip_router:get_all_calls()
        ]).


%% @private Get all active transactions for this SipApp, having CallId.
-spec get_all_transactions(nksip:app_id(), nksip:call_id()) ->
    {ok, [{uac|uas, nksip_call:trans_id()}]} | {error, term()}.

get_all_transactions(AppId, CallId) ->
    nksip_router:send_work_sync(AppId, CallId, get_all_transactions).


%% @private Applies a fun to a transaction and returns the result.
-spec apply_transaction(nksip:app_id(), nksip:call_id(), nksip_sipmsg:id(), function()) ->
    {apply, term()} | {error, term()}.

apply_transaction(AppId, CallId, MsgId, Fun) ->
    nksip_router:send_work_sync(AppId, CallId, {apply_transaction, MsgId, Fun}).


%% @private
-spec apply_sipmsg(nksip:app_id(), nksip:call_id(), nksip_sipmsg:id(), function()) ->
    {apply, term()} | {error, term()}.

apply_sipmsg(AppId, CallId, MsgId, Fun) ->
    nksip_router:send_work_sync(AppId, CallId, {apply_sipmsg, MsgId, Fun}).


%% @private Checks if the call has expired elements
-spec check_call(call()) ->
    call().

check_call(#call{timers=#call_timers{trans=TransTime, dialog=DialogTime}} = Call) ->
    Now = nksip_lib:timestamp(),
    Trans1 = check_call_trans(Now, 2*TransTime, Call),
    Forks1 = check_call_forks(Now, 2*TransTime, Call),
    Dialogs1 = check_call_dialogs(Now, 2*DialogTime, Call),
    Call#call{trans=Trans1, forks=Forks1, dialogs=Dialogs1}.


%% @private
-spec check_call_trans(nksip_lib:timestamp(), integer(), call()) ->
    [trans()].

check_call_trans(Now, MaxTime, #call{trans=Trans}) ->
    lists:filter(
        fun(#trans{id=Id, start=Start}) ->
            case Now - Start < MaxTime of
                true ->
                    true;
                false ->
                    ?call_error("Call removing expired transaction ~p", [Id]),
                    false
            end
        end,
        Trans).


%% @private
-spec check_call_forks(nksip_lib:timestamp(), integer(), call()) ->
    [fork()].

check_call_forks(Now, MaxTime, #call{forks=Forks}) ->
    lists:filter(
        fun(#fork{id=Id, start=Start}) ->
            case Now - Start < MaxTime of
                true ->
                    true;
                false ->
                    ?call_error("Call removing expired fork ~p", [Id]),
                    false
            end
        end,
        Forks).


%% @private
-spec check_call_dialogs(nksip_lib:timestamp(), integer(), call()) ->
    [nksip:dialog()].

check_call_dialogs(Now, MaxTime, #call{dialogs=Dialogs}) ->
    lists:filter(
        fun(#dialog{id=Id, updated=Updated}) ->
            case Now - Updated < MaxTime of
                true ->
                    true;
                false ->
                    ?call_warning("Call removing expired dialog ~p", [Id]),
                    false
            end
        end,
        Dialogs).


