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

%% @doc Call Management Module
%%
%% A new {@link nksip_call_srv} process is started for every different
%% Call-ID request or response, incoming or outcoming.
%% This module offers an interface to this process.

-module(nksip_call).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([send/2, send/4, send_dialog/3, cancel/1, send_reply/2]).
-export([find_dialog/1]).
-export([get_authorized_list/1, clear_authorized_list/1, stop_dialog/1]).
-export([get_all/0, get_info/0, clear_all/0]).
-export([app_reply/4, work/3, timeout/3]).
-export([sync_send_dialog/4, make_dialog/4]).
-import(nksip_call_router, [send_work_sync/3, send_work_async/3]).

-export_type([call/0, trans/0, fork/0, work/0]).

-include("nksip.hrl").
-include("nksip_call.hrl").



%% ===================================================================
%% Types
%% ===================================================================


-type call() :: #call{}.

-type trans() :: #trans{}.

-type fork() :: #fork{}.

-type work() :: {incoming, #sipmsg{}} | 
                {app_reply, atom(), nksip_call_uas:id(), term()} |
                {send_reply, nksip_sipmsg:id(), nksip:sipreply()} |
                {make, nksip:method(), nksip:user_uri(), nksip_lib:optslist()} |
                {send, nksip:request(), nksip_lib:optslist()} |
                {send, nksip:method(), nksip:user_uri(), nksip_lib:optslist()} |
                {send_dialog, nksip_dialog:id(), nksip:method(), nksip_lib:optslist()} |
                {cancel, nksip_sipmsg:id()} |
                {make_dialog, nksip_dialog:id(), nksip:method(), nksip_lib:optslist()} |
                {apply_dialog, nksip_dialog:id(), function()} |
                {find_dialog, nksip_sipmsg:id()} |
                get_all_dialogs | 
                {stop_dialog, nksip_dialog:id()} |
                {apply_sipmsg, nksip_sipmsg:id(), function()} |
                {apply_transaction, nksip_sipmsg:id(), function()} |
                get_all_transactions | info |
                {get_authorized_list, nksip_dialog:id()} | 
                {clear_authorized_list, nksip_dialog:id()}.

%% ===================================================================
%% Public
%% ===================================================================

%% @doc Sends a new request.
-spec send(nksip:request(), nksip_lib:optslist()) ->
    nksip_uac:result() | nksip_uac:ack_result() | {error, nksip_uac:error()}.

send(#sipmsg{app_id=AppId, call_id=CallId}=Req, Opts) ->
    send_work_sync(AppId, CallId, {send, Req, Opts}).


%% @doc Generates and sends a new request.
-spec send(nksip:app_id(), nksip:method(), nksip:user_uri(), nksip_lib:optslist()) ->
    nksip_uac:result() | {error, nksip_uac:error()}.

send(AppId, Method, Uri, Opts) ->
    case nksip_lib:get_binary(call_id, Opts) of
        <<>> -> CallId = nksip_lib:luid();
        CallId -> ok
    end,
    send_work_sync(AppId, CallId, {send, Method, Uri, Opts}).


%% @doc Generates and sends a new in-dialog request.
-spec send_dialog(nksip:id(), nksip:method(), nksip_lib:optslist()) ->
    nksip_uac:result() | nksip_uac:ack_result() | {error, nksip_uac:error()}.

send_dialog(Id, Method, Opts) ->
    case nksip_dialog:get_id(Id) of
        <<>> -> 
            {error, unknown_dialog};
        Id2 ->
            {AppId, DialogId, CallId} = nksip_dialog:parse_id(Id2),
            send_work_sync(AppId, CallId, {send_dialog, DialogId, Method, Opts})
    end.


%% @doc Cancels an ongoing INVITE request.
-spec cancel(nksip:id()) ->
    ok | {error, nksip_uac:cancel_error()}.

cancel(Id) ->
    {req, AppId, ReqId, CallId} = nksip_sipmsg:parse_id(Id),
    send_work_sync(AppId, CallId, {cancel, ReqId}).


%% @doc Gets the Dialog Id of a request or response id
-spec find_dialog(nksip:id()) ->
    {ok, nksip:id()} | {error, Error}
    when Error :: unknown_dialog | invalid_request | nksip_call_router:sync_error().

find_dialog(Id) ->
    case nksip_sipmsg:parse_id(Id) of
        {Class, AppId, MsgId, CallId} when Class==req; Class==resp ->
            send_work_sync(AppId, CallId, {find_dialog, MsgId})
    end.


%% @private Sends a callback SipApp response.
-spec app_reply(atom(), nksip_call_uas:id(), pid(), term()) ->
    ok.

app_reply(Fun, TransId, Pid, Reply) ->
    gen_server:cast(Pid, {async_work, {app_reply, Fun, TransId, Reply}}).


%% @doc Sends a synchronous request reply.
-spec send_reply(nksip:id(), nksip:sipreply()) ->
    {ok, nksip:response()} | {error, Error}
    when Error :: invalid_call | invalid_request | nksip_call_router:sync_error().

send_reply(Id, Reply) ->
    {req, AppId, ReqId, CallId} = nksip_sipmsg:parse_id(Id),
    send_work_sync(AppId, CallId, {send_reply, ReqId, Reply}).
    

%% @doc Gets authorized list of transport, ip and ports for a dialog.
-spec get_authorized_list(nksip:id()) ->
    [{nksip:protocol(), inet:ip_address(), inet:port_number()}].

get_authorized_list(Id) ->
    {AppId, DialogId, CallId} = nksip_dialog:parse_id(Id),
    case send_work_sync(AppId, CallId, {get_authorized_list, DialogId}) of
        {ok, List} -> List;
        _ -> []
    end.


%% @doc Clears authorized list of transport, ip and ports for a dialog.
-spec clear_authorized_list(nksip:id()) ->
    ok | error.

clear_authorized_list(Id) ->
    {AppId, DialogId, CallId} = nksip_dialog:parse_id(Id),
    case send_work_sync(AppId, CallId, {clear_authorized_list, DialogId}) of
        ok -> ok;
        _ -> error
    end.


%% @doc Stops (deletes) a dialog.
-spec stop_dialog(nksip:id()) ->
    ok | {error, unknown_dialog}.
 
stop_dialog(Id) ->
    {AppId, DialogId, CallId} = nksip_dialog:parse_id(Id),
    send_work_sync(AppId, CallId, {stop_dialog, DialogId}).


%% @doc Get all started calls.
-spec get_all() ->
    [{nksip:app_id(), nksip:call_id(), pid()}].

get_all() ->
    nksip_call_router:get_all_calls().


%% @doc Get information about all started calls.
-spec get_info() ->
    [term()].

get_info() ->
    nksip_call_router:get_all_info().


%% @private 
clear_all() ->
    nksip_call_router:clear_all_calls().


%% ===================================================================
%% Call works
%% ===================================================================


%% @private
-spec work(work(), from()|none, call()) ->
    call().

work({incoming, #sipmsg{class={req, _}}=Req}, none, Call) ->
    nksip_call_uas_req:request(Req, Call);

work({incoming, #sipmsg{class={resp, _, _}}=Resp}, none, Call) ->
    GlobalId = nksip_config_cache:global_id(),
    case nksip_uac_lib:is_stateless(Resp, GlobalId) of
        true -> nksip_call_proxy:response_stateless(Resp, Call);
        false -> nksip_call_uac_resp:response(Resp, Call)
    end;

work({app_reply, Fun, TransId, Reply}, none, Call) ->
    nksip_call_uas_route:app_reply(Fun, TransId, Reply, Call);

work({send_reply, ReqId, Reply}, From, Call) ->
    case get_trans(ReqId, Call) of
        {ok, #trans{class=uas}=UAS} ->
            case nksip_call_uas_reply:reply(Reply, UAS, Call) of
                {{ok, _SipMsg}, Call1} -> Result = ok;
                {{error, Error}, Call1} -> Result = {error, Error}
            end;
        _ -> 
            Result = {error, invalid_call},
            Call1 = Call
    end,
    gen_server:reply(From, Result),
    Call1;

work({make, Method, Uri, Opts}, From, Call) ->
    #call{app_id=AppId, call_id=CallId} = Call,
    Opts1 = [{call_id, CallId} | Opts],
    Reply = nksip_uac_lib:make(AppId, Method, Uri, Opts1),
    gen_server:reply(From, Reply),
    Call;

work({send, Req, Opts}, From, Call) ->
    nksip_call_uac_req:request(Req, Opts, {srv, From}, Call);

work({send, Method, Uri, Opts}, From, Call) ->
    #call{app_id=AppId, call_id=CallId} = Call,
    Opts1 = [{call_id, CallId} | Opts],
    case nksip_uac_lib:make(AppId, Method, Uri, Opts1) of
        {ok, Req, ReqOpts} -> 
            work({send, Req, ReqOpts}, From, Call);
        {error, Error} ->
            gen_server:reply(From, {error, Error}),
            Call
    end;

work({send_dialog, DialogId, Method, Opts}, From, Call) ->
    case nksip_call_uac_dialog:make(DialogId, Method, Opts, Call) of
        {ok, {RUri, Opts1}, Call1} -> 
            work({send, Method, RUri, Opts1}, From, Call1);
        {error, Error} ->
            gen_server:reply(From, {error, Error}),
            Call
    end;

work({cancel, ReqId}, From, Call) ->
    case get_trans(ReqId, Call) of
        {ok, #trans{class=uac}=UAC} -> 
            gen_server:reply(From, ok),
            nksip_call_uac:cancel(UAC, undefined, Call);
        _ ->
            gen_server:reply(From, {error, unknown_request}),
            Call
    end;

work({apply_dialog, DialogId, Fun}, From, Call) ->
    case get_dialog(DialogId, Call) of
        {ok, Dialog} ->
            case catch Fun(Dialog) of
                {Reply, {update, #dialog{}=Dialog1}} ->
                    gen_server:reply(From, Reply),
                    nksip_call_dialog:store(Dialog1, Call);
                Reply ->
                    gen_server:reply(From, Reply),
                    Call
            end;
        not_found -> 
            gen_server:reply(From, {error, unknown_dialog}),
            Call
    end;
    
work(get_all_dialogs, From, #call{dialogs=Dialogs}=Call) ->
    Ids = [nksip_dialog:get_id(Dialog) || Dialog <- Dialogs],
    gen_server:reply(From, {ok, Ids}),
    Call;

work({stop_dialog, DialogId}, From, Call) ->
    case get_dialog(DialogId, Call) of
        {ok, Dialog} ->
            gen_fsm:reply(From, ok),
            nksip_call_dialog:stop(forced, Dialog, Call);
        not_found ->
            gen_fsm:reply(From, {error, unknown_dialog}),
            Call
    end;

work({find_dialog, MsgId}, From, Call) ->
    Reply = case find_dialog(MsgId, Call) of
        {ok, DialogId} -> 
            case get_dialog(DialogId, Call) of
                {ok, Dialog} -> {ok, nksip_dialog:get_id(Dialog)};
                not_found -> {ok, <<>>}
            end;
        not_found -> 
            {ok, <<>>}
    end,
    gen_server:reply(From, Reply),
    Call;

work({apply_sipmsg, MsgId, Fun}, From, Call) ->
    case get_sipmsg(MsgId, Call) of
        {ok, Msg} -> 
            case catch Fun(Msg) of
                {Reply, {update, #sipmsg{}=SipMsg1}} ->
                    gen_server:reply(From, Reply),
                    nksip_call_lib:update_sipmsg(SipMsg1, Call);
                Reply ->
                    gen_server:reply(From, Reply),
                    Call
            end;
        not_found -> 
            gen_server:reply(From, {error, unknown_sipmsg}),
            Call
    end;

work({apply_transaction, MsgId, Fun}, From, Call) ->
    case get_trans(MsgId, Call) of
        {ok, Trans} -> gen_server:reply(From, catch Fun(Trans));
        not_found ->  gen_server:reply(From, {error, unknown_transaction})
    end,
    Call;

work(get_all_transactions, From, #call{trans=Trans}=Call) ->
    Ids = [{Class, Id} || #trans{id=Id, class=Class} <- Trans],
    gen_server:reply(From, {ok, Ids}),
    Call;

work(info, From, Call) -> 
    #call{
        app_id = AppId, 
        call_id = CallId, 
        trans = Trans, 
        dialogs = Dialogs,
        events = ProvEvents
    } = Call,
    InfoTrans = lists:map(
        fun(#trans{id=Id, class=Class, method=Method, 
                   status=Status, timeout_timer=Timeout}) ->
            T = case Timeout of
                {Tag, Timer} -> {Tag, erlang:read_timer(Timer)};
                undefined -> undefined
            end,
            {trans, AppId, CallId, Id, Class, Method, Status, T}
        end,
        Trans),
    InfoDialog = lists:map(
        fun(#dialog{id=DlgId, invite=Invite, subscriptions=Subs}) ->
            Inv = case Invite of
                #invite{status=Status, timeout_timer=Timer} ->
                    T = case Timer of
                        Timer when is_reference(Timer) ->  erlang:read_timer(Timer);
                        undefined -> undefined
                    end,
                    {Status, T};
                undefined ->
                    undefined
            end,
            Ev = [
                    {EvId, Status, Class, erlang:read_timer(Exp)}
                    ||
                    #subscription{id=EvId, status=Status, class=Class, timer_expire=Exp} 
                    <- Subs
                ],
            {dlg, AppId, DlgId, {invite, Inv}, {event, Ev}}
        end,
        Dialogs),
    InfoProvEvents = case ProvEvents of
        [] -> [];
        _ -> [{prov_events, length(ProvEvents)}]
    end,
    gen_server:reply(From, InfoTrans++InfoDialog++InfoProvEvents),
    Call;

work({get_authorized_list, DlgId}, From, #call{auths=Auths}=Call) ->
    List = [{Proto, Ip, Port} || 
            {D, Proto, Ip, Port} <- Auths, D==DlgId],
    gen_server:reply(From, {ok, List}),
    Call;

work({clear_authorized_list, DlgId}, From, #call{auths=Auths}=Call) ->
    Auths1 = [{D, Proto, Ip, Port} || 
              {D, Proto, Ip, Port} <- Auths, D/=DlgId],
    gen_server:reply(From, ok),
    Call#call{auths=Auths1};

work(crash, _, _) ->
    error(forced_crash).


%% ===================================================================
%% Call timeouts
%% ===================================================================


%% @private
-spec timeout(term(), reference(), call()) ->
    call().

timeout({uac, Tag, Id}, _Ref, #call{trans=Trans}=Call) ->
    case lists:keyfind(Id, #trans.id, Trans) of
        #trans{class=uac}=UAC ->
            nksip_call_uac:timer(Tag, UAC, Call);
        false ->
            ?call_warning("Call ignoring uac timer (~p, ~p)", [Tag, Id]),
            Call
    end;


timeout({uas, Tag, Id}, _Ref, #call{trans=Trans}=Call) ->
    case lists:keyfind(Id, #trans.id, Trans) of
        #trans{class=uas}=UAS ->
            nksip_call_uas:timer(Tag, UAS, Call);
        false ->
            ?call_warning("Call ignoring uas timer (~p, ~p)", [Tag, Id]),
            Call
    end;

timeout({dlg, Tag, Id}, _Ref, #call{dialogs=Dialogs}=Call) ->
    case lists:keyfind(Id, #dialog.id, Dialogs) of
        #dialog{} = Dialog -> 
            nksip_call_dialog:timer(Tag, Dialog, Call);
        false ->
            ?call_warning("Call ignoring dialog timer (~p, ~p)", [Tag, Id]),
            Call
    end;

timeout({remove_prov_event, Id}, _Ref, Call) ->
    nksip_call_event:remove_prov_event(Id, Call);

timeout(check_call, _Ref, Call) ->
    TransTime = 1000*?MAX_TRANS_TIME,
    DialogTime = 1000*?MAX_DIALOG_TIME,
    Now = nksip_lib:timestamp(),
    Trans1 = check_call_trans(Now, TransTime, Call),
    Forks1 = check_call_forks(Now, TransTime, Call),
    Dialogs1 = check_call_dialogs(Now, DialogTime, Call),
    erlang:start_timer(round(2*TransTime), self(), check_call),
    Call#call{trans=Trans1, forks=Forks1, dialogs=Dialogs1}.



%% ===================================================================
%% Internal
%% ===================================================================

%% @private Sends a new in-dialog request from inside the call process
-spec sync_send_dialog(nksip_dialog:id(), nksip:method(), nksip_lib:optslist(), call()) ->
    {ok, call()} | {error, term()}.

sync_send_dialog(DialogId, Method, Opts, Call) ->
    case make_dialog(DialogId, Method, Opts, Call) of
        {ok, Req, ReqOpts, Call1} ->
            {ok, nksip_call_uac_req:request(Req, ReqOpts, none, Call1)};
        {error, Error} ->
            {error, Error}
    end.


%% @private Generates a new in-dialog request from inside the call process
-spec make_dialog(nksip_dialog:id(), nksip:method(), nksip_lib:optslist(), call()) ->
    {ok, nksip:request(), nksip_lib:optslist(), call()} | {error, term()}.

make_dialog(DialogId, Method, Opts, Call) ->
    #call{app_id=AppId, call_id=CallId} = Call,
    case nksip_call_uac_dialog:make(DialogId, Method, Opts, Call) of
        {ok, {RUri, Opts1}, Call1} -> 
            Opts2 = [{call_id, CallId} | Opts1],
            case nksip_uac_lib:make(AppId, Method, RUri, Opts2) of
                {ok, Req, ReqOpts} ->
                    {ok, Req, ReqOpts, Call1};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @private
-spec check_call_trans(nksip_lib:timestamp(), integer(), call()) ->
    [trans()].

check_call_trans(Now, MaxTime, #call{trans=Trans}) ->
    lists:filter(
        fun(#trans{id=Id, start=Start}) ->
            case Now - Start < MaxTime/1000 of
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
            case Now - Start < MaxTime/1000 of
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
            case Now - Updated < MaxTime/1000 of
                true ->
                    true;
                false ->
                    ?call_warning("Call removing expired dialog ~p", [Id]),
                    false
            end
        end,
        Dialogs).


%% @private
-spec find_dialog(nksip_sipmsg:id(), call()) ->
    {ok, nksip_dialog:id()} | not_found.

find_dialog(MsgId, #call{msgs=Msgs}) ->
    case lists:keyfind(MsgId, 1, Msgs) of
        false -> not_found;
        {MsgId, _TransId, DialogId} -> {ok, DialogId}
    end.


%% @private
-spec get_trans(nksip_sipmsg:id(), call()) ->
    {ok, trans()} | not_found.

get_trans(SipMsgId, #call{msgs=Msgs, trans=AllTrans}) ->
    case lists:keyfind(SipMsgId, 1, Msgs) of
        {_, TransId, _DialogId} -> 
            case lists:keyfind(TransId, #trans.id, AllTrans) of
                #trans{}=Trans -> {ok, Trans};
                false -> not_found
            end;
        false -> 
            not_found
    end.


%% @private
-spec get_sipmsg(nksip_sipmsg:id(), call()) ->
    {ok, nksip:request()|nksip:response()} | not_found.

get_sipmsg(SipMsgId, Call) ->
    case get_trans(SipMsgId, Call) of
        {ok, #trans{request=#sipmsg{id=SipMsgId}=Req}} -> {ok, Req};
        {ok, #trans{response=#sipmsg{id=SipMsgId}=Resp}} -> {ok, Resp};
        _ -> not_found
    end.


%% @private
-spec get_dialog(nksip_dialog:id(), call()) ->
    {ok, #dialog{}} | not_found.

get_dialog(DialogId, #call{dialogs=Dialogs}) ->
    case lists:keyfind(DialogId, #dialog.id, Dialogs) of
        false -> not_found;
        Dialog -> {ok, Dialog}
    end.







