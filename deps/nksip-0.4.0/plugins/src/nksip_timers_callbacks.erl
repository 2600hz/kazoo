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

%% @doc NkSIP Event State Compositor Plugin Callbacks
-module(nksip_timers_callbacks).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-include("../include/nksip.hrl").
-include("../include/nksip_call.hrl").

-export([nkcb_parse_uac_opts/2, nkcb_dialog_update/3, nkcb_make_uac_dialog/4,
         nkcb_uac_pre_request/4, nkcb_uac_pre_response/3, nkcb_uac_response/4,
         nkcb_uas_dialog_response/4, nkcb_uas_process/2, nkcb_route/4]).

%%%%%%%%%%%%%%%% Implemented core plugin callbacks %%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Called to parse specific UAC options
-spec nkcb_parse_uac_opts(nksip:request(), nksip:optslist()) ->
    {continue, list()} | {error, term()}.

nkcb_parse_uac_opts(Req, Opts) ->
    case nksip_timers_lib:parse_uac_config(Opts, Req, []) of
        {ok, Opts1} ->
            {continue, [Req, Opts1]};
        {error, Error} ->
            {error, Error}
    end.


 %% @private
-spec nkcb_dialog_update(term(), nksip:dialog(), nksip_call:call()) ->
    {ok, nksip_call:call()} | continue.

nkcb_dialog_update({update, Class, Req, Resp}, Dialog, Call) ->
    Dialog1 = nksip_call_dialog:target_update(Class, Req, Resp, Dialog, Call),
    Dialog2 = nksip_call_dialog:session_update(Dialog1, Call),
    Dialog3 = nksip_timers_lib:timer_update(Req, Resp, Class, Dialog2, Call),
    {ok, nksip_call_dialog:store(Dialog3, Call)};

nkcb_dialog_update({invite, {stop, Reason}}, Dialog, Call) ->
    #dialog{meta=Meta, invite=Invite} = Dialog,
    #invite{
        media_started = Media,
        retrans_timer = RetransTimer,
        timeout_timer = TimeoutTimer
    } = Invite,    
    RefreshTimer = nksip_lib:get_value(nksip_timers_refresh, Meta),
    nksip_lib:cancel_timer(RetransTimer),
    nksip_lib:cancel_timer(TimeoutTimer),
    nksip_lib:cancel_timer(RefreshTimer),
    StopReason = nksip_call_dialog:reason(Reason),
    nksip_call_dialog:sip_dialog_update({invite_status, {stop, StopReason}}, Dialog, Call),
    case Media of
        true -> nksip_call_dialog:sip_session_update(stop, Dialog, Call);
        _ -> ok
    end,
    {ok, nksip_call_dialog:store(Dialog#dialog{invite=undefined}, Call)};

nkcb_dialog_update({invite, Status}, Dialog, Call) ->
    #dialog{
        id = DialogId, 
        blocked_route_set = BlockedRouteSet,
        invite = #invite{
            status = OldStatus, 
            media_started = Media,
            class = Class,
            request = Req, 
            response = Resp
        } = Invite
    } = Dialog,
    Dialog1 = case Status of
        OldStatus -> 
            Dialog;
        _ -> 
            nksip_call_dialog:sip_dialog_update({invite_status, Status}, Dialog, Call),
            Dialog#dialog{invite=Invite#invite{status=Status}}
    end,
    ?call_debug("Dialog ~s ~p -> ~p", [DialogId, OldStatus, Status]),
    Dialog2 = if
        Status==proceeding_uac; Status==proceeding_uas; 
        Status==accepted_uac; Status==accepted_uas ->
            D1 = nksip_call_dialog:route_update(Class, Req, Resp, Dialog1),
            D2 = nksip_call_dialog:target_update(Class, Req, Resp, D1, Call),
            nksip_call_dialog:session_update(D2, Call);
        Status==confirmed ->
            nksip_call_dialog:session_update(Dialog1, Call);
        Status==bye ->
            case Media of
                true -> 
                    nksip_call_dialog:sip_session_update(stop, Dialog1, Call),
                    #dialog{invite=I1} = Dialog1,
                    Dialog1#dialog{invite=I1#invite{media_started=false}};
                _ ->
                    Dialog1
            end
    end,
    Dialog3 = case 
        (not BlockedRouteSet) andalso 
        (Status==accepted_uac orelse Status==accepted_uas)
    of
        true -> Dialog2#dialog{blocked_route_set=true};
        false -> Dialog2
    end,
    Dialog4 = nksip_timers_lib:timer_update(Req, Resp, Class, Dialog3, Call),
    {ok, nksip_call_dialog:store(Dialog4, Call)};

nkcb_dialog_update(_, _, _) ->
    continue.
    

%% @doc Called when a new in-dialog request is being generated
-spec nkcb_make_uac_dialog(nksip:method(), nksip:uri(), nksip:optslist(), nksip:call()) ->
    {continue, list()}.

nkcb_make_uac_dialog(Method, Uri, Opts, #call{dialogs=[Dialog|_]}=Call) ->
    Opts1 = case lists:keymember(nksip_timers_se, 1, Opts) of
        true -> 
            Opts;
        false -> 
            nksip_timers_lib:make_uac_dialog(Method, Dialog, Call)++Opts
    end,
    {continue, [Method, Uri, Opts1, Call]}.


%% @doc Called when the UAC is preparing a request to be sent
-spec nkcb_uac_pre_request(nksip:request(), nksip:optslist(), 
                           nksip_call_uac:uac_from(), nksip:call()) ->
    {continue, list()}.

nkcb_uac_pre_request(Req, Opts, From, Call) ->
    Req1 = case From of 
        {fork, _} -> nksip_timers_lib:uac_pre_request(Req, Call);
        _ -> Req
    end,
    {continue, [Req1, Opts, From, Call]}.


%% @doc Called when the UAC has just received a responses
-spec nkcb_uac_pre_response(nksip:response(), nksip_call:trans(), nksip:call()) ->
    {continue, list()}.

nkcb_uac_pre_response(Resp, UAC, Call) ->
    #trans{request=Req, from=From} = UAC,
    Resp1 = case From of 
        {fork, _} -> nksip_timers_lib:uac_pre_response(Req, Resp);
        _ -> Resp
    end,
    {continue, [Resp1, UAC, Call]}.


%% @doc Called after the UAC processes a response
-spec nkcb_uac_response(nksip:request(), nksip:response(), 
                        nksip_call:trans(), nksip:call()) ->
    {ok, nksip:call()} | continue.

nkcb_uac_response(Req, Resp, UAC, Call) ->
    #trans{from=From, code=Code} = UAC,
    IsProxy = case From of {fork, _} -> true; _ -> false end,
    case 
        (not IsProxy) andalso Code==422 andalso
        nksip_timers_lib:uac_received_422(Req, Resp, UAC, Call) 
    of
        {resend, Req1, Call1} ->
            {ok, nksip_call_uac:resend(Req1, UAC, Call1)};
        false ->
            continue
    end.


%% @doc Called when preparing a UAS dialog response
-spec nkcb_uas_dialog_response(nksip:request(), nksip:response(), 
                               nksip:optslist(), nksip:call()) ->
    {ok, nksip:response(), nksip:optslist()} | continue.

nkcb_uas_dialog_response(Req, Resp, Opts, Call) ->
    Resp1 = case Req of
        #sipmsg{} -> 
            nksip_timers_lib:uas_dialog_response(Req, Resp, Call);
        _ ->
            % In a multiple 2xx scenario, request is already deleted at UAS
            ?call_info("Skipping timer check because of no request", []),
            Resp
    end,
    {continue, [Req, Resp1, Opts, Call]}.


%% @doc Called when the UAS is proceesing a request
-spec nkcb_uas_process(nksip_call:trans(), nksip_call:call()) ->
    {ok, nksip:call()} | {continue, list()}.

nkcb_uas_process(#trans{request=Req}=UAS, Call) ->
    case nksip_timers_lib:uas_check_422(Req, Call) of
        continue -> 
            continue;
        {update, Req1, Call1} ->
            UAS1 = UAS#trans{request=Req1},
            {continue, [UAS1, nksip_call_lib:update(UAS1, Call1)]};
        {reply, Reply, Call1} ->
            {ok, nksip_call_uas:do_reply(Reply, UAS, Call1)}
    end.


%% @doc Called when a proxy is preparing a routing
-spec nkcb_route(nksip:uri_set(), nksip:optslist(), 
                 nksip_call:trans(), nksip_call:call()) -> 
    {continue, list()} | {reply, nksip:sipreply(), nksip_call:call()}.

nkcb_route(UriList, ProxyOpts, UAS, Call) ->
    #trans{request=Req} = UAS,
    case nksip_timers_lib:uas_check_422(Req, Call) of
        continue -> 
            {continue, [UriList, ProxyOpts, UAS, Call]};
        {reply, Reply, Call1} -> 
            {reply, Reply, Call1};
        {update, Req1, Call1} -> 
            UAS1 = UAS#trans{request=Req1},
            {continue, [UriList, ProxyOpts, UAS1, nksip_call_lib:update(UAS1, Call1)]}
    end.



