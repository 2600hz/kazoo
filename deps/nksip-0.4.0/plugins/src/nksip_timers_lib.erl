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

%% @doc NkSIP Reliable Provisional Responses Plugin
-module(nksip_timers_lib).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-include("../include/nksip.hrl").
-include("../include/nksip_call.hrl").

-export([parse_uac_config/3]).
-export([uac_received_422/4, make_uac_dialog/3, uas_check_422/2, uas_dialog_response/3]).
-export([timer_update/5, uac_pre_request/2, uac_pre_response/2]).


-define(MAX_422_TRIES, 5).


%% ===================================================================
%% Private
%% ===================================================================


%% @private
-spec parse_uac_config(nksip:optslist(), nksip:request(), nksip:optslist()) ->
    {ok, nksip:optslist()} | {error, term()}.

parse_uac_config([], _Req, Opts) ->
    {ok, lists:reverse(Opts)};

parse_uac_config([Term|Rest], Req, Opts) ->
    case Term of
        {nksip_timers_min_se, SE} when is_integer(SE) ->
            parse_uac_config(Rest, Req, [{replace, <<"min-se">>, SE}|Opts]);
        {nksip_timers_min_se, _} ->
            {error, {invalid_config, nksip_timers_min_se}};
        {nksip_timers_se, SE} when is_integer(SE) ->
            parse_uac_config([{nksip_timers_se, {SE, undefined}}|Rest], Req, Opts);
        {nksip_timers_se, {SE, Refresh}} when is_integer(SE) ->
            #sipmsg{app_id=AppId} = Req,
            case AppId:config_nksip_timers() of
                {_, MinSE} when SE<MinSE -> 
                    {error, {invalid_config, nksip_timers_se}};
                _ when Refresh==undefined -> 
                    Rep = {replace, <<"session-expires">>, SE},
                    parse_uac_config(Rest, Req, [Rep|Opts]);
                _ when Refresh==uac; Refresh==uas -> 
                    Rep = {replace, <<"session-expires">>, 
                            {SE, [{<<"refresher">>, Refresh}]}},
                    parse_uac_config(Rest, Req, [Rep|Opts]);
                _ ->
                    {error, {invalid_config, nksip_timers_se}}
            end;
        {nksip_timers_se, _} ->
            {error, {invalid_config, nksip_timers_se}};
        _ ->
            parse_uac_config(Rest, Req, [Term|Opts])
    end.



%% @private
-spec timer_update(nksip:request(), nksip:response(), uac|uas,
                   nksip:dialog(), nksip_call:call()) ->
    nksip:dialog().

timer_update(Req, #sipmsg{class={resp, Code, _}}=Resp, Class,
             #dialog{invite=#invite{status=confirmed}}=Dialog, Call)
             when Code>=200 andalso Code<300 ->
    #dialog{id=DialogId, invite=Invite, meta=Meta} = Dialog,
   % class from #invite{} can only be used for INVITE, not UPDATE
    #invite{retrans_timer=RetransTimer, timeout_timer=TimeoutTimer} = Invite,
    RefreshTimer = nksip_lib:get_value(nksip_timers_refresh, Meta),
    nksip_lib:cancel_timer(RetransTimer),
    nksip_lib:cancel_timer(TimeoutTimer),
    nksip_lib:cancel_timer(RefreshTimer),
    Meta1 = nksip_lib:delete(Meta, [nksip_timers_refresh, nksip_timers_se]),
    case get_timer(Req, Resp, Class, Call) of
        {refresher, SE} ->
            Invite1 = Invite#invite{
                retrans_timer = undefined,
                timeout_timer = start_timer(1000*SE, invite_timeout, DialogId)
            },
            Meta2 = [
                {nksip_timers_se, SE}, 
                {nksip_timers_refresh, start_timer(500*SE, invite_refresh, DialogId)}
                | Meta1
            ];
        {refreshed, SE} ->
            Invite1 = Invite#invite{
                retrans_timer = undefined,
                timeout_timer = start_timer(750*SE, invite_timeout, DialogId)
            },
            Meta2 = [
                {nksip_timers_se, SE}, 
                {nksip_timers_refresh, undefined}
                | Meta1
            ];
        {none, Timeout} ->
            Invite1 = Invite#invite{
                retrans_timer = undefined,
                timeout_timer = start_timer(1000*Timeout, invite_timeout, DialogId)
            },
            Meta2 = [
                {nksip_timers_se, undefined}, 
                {nksip_timers_refresh, undefined} 
                | Meta1
            ]
    end,
    Dialog#dialog{invite=Invite1, meta=Meta2};

% We are returning to confirmed after a non-2xx response
timer_update(_Req, _Resp, _Class, 
             #dialog{invite=#invite{status=confirmed}}=Dialog, _Call) ->
    #dialog{invite=Invite} = Dialog,
    #invite{retrans_timer=RetransTimer} = Invite,
    nksip_lib:cancel_timer(RetransTimer),
    Invite1 = Invite#invite{
        retrans_timer = undefined
    },
    Dialog#dialog{invite=Invite1};

timer_update(_Req, _Resp, _Class,
             #dialog{invite=#invite{status=accepted_uas}}=Dialog, Call) ->
    #dialog{id=DialogId, invite=Invite, meta=Meta} = Dialog,
    #invite{retrans_timer=RetransTimer, timeout_timer=TimeoutTimer} = Invite,
    RefreshTimer = nksip_lib:get_value(nksip_timers_refresh, Meta),
    nksip_lib:cancel_timer(RetransTimer),
    nksip_lib:cancel_timer(TimeoutTimer),
    nksip_lib:cancel_timer(RefreshTimer),
    #call{timers=#call_timers{t1=T1}} = Call,
    Meta1 = nksip_lib:delete(Meta, [nksip_timers_se, nksip_timers_refresh]),
    Meta2 = [{nksip_timers_se, undefined}, {nksip_timers_refresh, undefined}|Meta1],
    Invite1 = Invite#invite{
        retrans_timer = start_timer(T1, invite_retrans, DialogId),
        next_retrans = 2*T1,
        timeout_timer = start_timer(64*T1, invite_timeout, DialogId)
    },
    Dialog#dialog{invite=Invite1, meta=Meta2};

timer_update(_Req, _Resp, _Class, Dialog, Call) ->
    #dialog{id=DialogId, invite=Invite, meta = Meta} = Dialog,
    #invite{retrans_timer=RetransTimer, timeout_timer=TimeoutTimer} = Invite,
    RefreshTimer = nksip_lib:get_value(nksip_timers_refresh, Meta),
    nksip_lib:cancel_timer(RetransTimer),
    nksip_lib:cancel_timer(TimeoutTimer),
    nksip_lib:cancel_timer(RefreshTimer),
    #call{timers=#call_timers{t1=T1}} = Call,
    Meta1 = nksip_lib:delete(Meta, [nksip_timers_se, nksip_timers_refresh]),
    Meta2 = [{nksip_timers_se, undefined}, {nksip_timers_refresh, undefined}|Meta1],
    Invite1 = Invite#invite{
        retrans_timer = undefined,
        timeout_timer = start_timer(64*T1, invite_timeout, DialogId)        
    },
    Dialog#dialog{invite=Invite1, meta=Meta2}.


%% @private
-spec get_timer(nksip:request(), nksip:response(), uac|uas, nksip_call:call()) ->
    {refresher | refreshed | none, integer()}.

get_timer(Req, #sipmsg{class={resp, Code, _}}=Resp, Class, Call)
             when Code>=200 andalso Code<300 ->
    #call{app_id=AppId} = Call,
    {_, Default} = AppId:config_nksip_timers(),
    {SE, Refresh} = case parse(Resp) of
        {ok, SE0, Refresh0} ->
            {SE0, Refresh0};
        undefined ->            
            case parse(Req) of
                {ok, SE0, Refresh0} -> {SE0, Refresh0};
                _ -> {Default, undefined}
            end;
        invalid ->
            ?call_warning("Invalid Session-Expires in response", []),
            {Default, undefined}
    end,
    Type = case Class==Refresh of
        true -> refresher;
        false when Refresh/=undefined -> refreshed;
        false -> none
    end,
    ?call_info("Session Timer updated (~p, ~p)", [{Class, Refresh, Type}, SE]),
    {Type, SE}.


%% @private
-spec make_uac_dialog(nksip:method(), nksip:dialog(), nksip_call:call()) ->
    nksip:optslist().

make_uac_dialog(Method, Dialog, Call) ->
    #dialog{id=DialogId, meta=Meta} = Dialog,
    SE = nksip_lib:get_value(nksip_timers_se, Meta),
    case is_integer(SE) andalso (Method=='INVITE' orelse Method=='UPDATE') of
        true ->
            {SE1, MinSE} = case 
                nksip_call_dialog:get_meta(nksip_min_se, DialogId, Call)
            of
                undefined -> {SE, undefined};
                CurrMinSE -> {max(SE, CurrMinSE), CurrMinSE}
            end,
            % Do not change the roles, if a refresh is sent from the 
            % refreshed instead of the refresher
            RefreshTimer = nksip_lib:get_value(nksip_timers_refresh, Meta),
            Class = case is_reference(RefreshTimer) of
                true -> uac;
                false -> uas
            end,
            [
                {nksip_timers_se, {SE1, Class}} |
                case is_integer(MinSE) of true -> 
                    [{nksip_timers_min_se, MinSE}]; 
                    false -> [] 
                end
            ];
        _ ->
            []
    end.


%% @private
-spec uac_received_422(nksip:request(), nksip:response(), 
                       nksip_call:trans(), nksip_call:call()) ->
    {resend, nksip:request(), nksip_call:call()} | false.

uac_received_422(Req, Resp, UAC, Call) ->
    #sipmsg{app_id=AppId, dialog_id=DialogId} = Resp,
    #trans{
        method = Method, 
        code = Code, 
        iter = Iter
    } = UAC,
    case 
        Code==422 andalso 
        (Method=='INVITE' orelse Method=='UPDATE') andalso
        Iter < ?MAX_422_TRIES
    of 
        true ->
            case nksip_sipmsg:header(<<"min-se">>, Resp, integers) of
                [RespMinSE] ->
                    {_, ConfigMinSE} = AppId:config_nksip_timers(),
                    CurrentMinSE = case 
                        nksip_call_dialog:get_meta(nksip_min_se, DialogId, Call)
                    of
                        undefined -> ConfigMinSE;
                        CurrentMinSE0 -> CurrentMinSE0
                    end,
                    NewMinSE = max(CurrentMinSE, RespMinSE),
                    Call1 = case NewMinSE of 
                        CurrentMinSE -> 
                            Call;
                        _ -> 
                            nksip_call_dialog:update_meta(nksip_min_se, NewMinSE, 
                                                          DialogId, Call)
                    end,
                    case parse(Req) of
                        {ok, SE0, Class0} ->
                            SE1 = max(SE0, NewMinSE),
                            SEHd = case Class0 of
                                uac -> {SE1, [{<<"refresher">>, <<"uac">>}]};
                                uas -> {SE1, [{<<"refresher">>, <<"uas">>}]};
                                undefined -> SE1
                            end,
                            Headers1 = nksip_headers:update(Req, [
                                {single, <<"session-expires">>, SEHd},
                                {single, <<"min-se">>, NewMinSE}
                            ]),
                            Req1 = Req#sipmsg{headers=Headers1},
                            {resend, Req1, Call1};
                        _ -> 
                            false
                    end;
                _ ->
                    false
            end;
        false ->
            false
    end.


%% @private
-spec uas_check_422(nksip:request(), nksip_call:call()) ->
    continue | {update, nksip:request(), nksip_call:call()} | 
               {reply, nksip:sipreply(), nksip_call:call()}.

uas_check_422(#sipmsg{app_id=AppId, class={req, Method}}=Req, Call) ->
    case Method=='INVITE' orelse Method=='UPDATE' of
        true ->
            case parse(Req) of
                undefined ->
                    continue;
                invalid ->
                    {reply, invalid_request, Call};
                {ok, SE, _} ->
                    case 
                        erlang:function_exported(AppId, config_nksip_timers, 0)
                        andalso AppId:config_nksip_timers() 
                    of
                        {_, MinSE} when SE < MinSE ->
                            #sipmsg{dialog_id=DialogId} = Req,
                            Call1 = case 
                                nksip_call_dialog:get_meta(nksip_min_se, DialogId, Call)
                            of
                                MinSE -> Call;
                                _ -> nksip_call_dialog:update_meta(nksip_min_se, MinSE, 
                                                                   DialogId, Call)
                            end,
                            case nksip_sipmsg:supported(<<"timer">>, Req) of
                                true ->
                                    {reply, {session_too_small, MinSE}, Call1};
                                false ->
                                    % No point in returning 422
                                    % Update in case we are a proxy
                                    Headers1 = nksip_headers:update(Req, 
                                                    [{single, <<"min-se">>, MinSE}]),
                                    {update, Req#sipmsg{headers=Headers1}, Call1}
                            end;
                        _ ->
                            continue
                    end
            end;
        false ->
            continue
    end.




%% @private
-spec uas_dialog_response(nksip:request(), nksip:response(), nksip_call:call()) ->
    nksip:response().

uas_dialog_response(
        Req, #sipmsg{app_id=AppId, class={resp, Code, _}, cseq={_, Method}}=Resp, _Call)
        when Code>=200 andalso Code<300 andalso 
             (Method=='INVITE' orelse Method=='UPDATE') ->
    case nksip_sipmsg:supported(<<"timer">>, Resp) of
        true ->
            #sipmsg{require=Require} = Resp,
            ReqSupport = nksip_sipmsg:supported(<<"timer">>, Req), 
            ReqMinSE = case nksip_sipmsg:header(<<"min-se">>, Req, integers) of
                [ReqMinSE0] -> ReqMinSE0;
                _ -> 90
            end,
            {ReqSE, ReqRefresh} = case 
                ReqSupport andalso parse(Req) 
            of
                {ok, ReqSE0, ReqRefresh0} -> {ReqSE0, ReqRefresh0};
                _ -> {0, undefined}
            end,
            Config = nksip_sipapp_srv:config(AppId),
            Default = nksip_lib:get_value(nksip_timers_se, Config),
            SE = case ReqSE of
                0 -> max(ReqMinSE, Default);
                _ -> max(ReqMinSE, min(ReqSE, Default))
            end,
            Refresh = case ReqRefresh of
                uac -> <<"uac">>;
                uas -> <<"uas">>;
                undefined -> <<"uas">>
            end,
            SE_Token = {nksip_lib:to_binary(SE), [{<<"refresher">>, Refresh}]},
            Headers1 = nksip_headers:update(Resp, 
                            [{default_single, <<"session-expires">>, SE_Token}]),
            % Add 'timer' to response's Require only if supported by uac
            Require1 = case ReqSupport of
                true -> nksip_lib:store_value(<<"timer">>, Require);
                false -> Require
            end,
            Resp#sipmsg{require=Require1, headers=Headers1};
        false ->
            Resp
    end;

uas_dialog_response(_Req, Resp, _Call) ->
    Resp.


%% @private
-spec uac_pre_request(nksip:request(), nksip_call:call()) ->
    nksip:request().

uac_pre_request(#sipmsg{app_id=AppId, class={req, Method}}=Req, _Call)
                 when Method=='INVITE'; Method=='UPDATE' ->
    ReqMinSE = case nksip_sipmsg:header(<<"min-se">>, Req, integers) of
        [ReqMinSE0] -> ReqMinSE0;
        _ -> 90
    end,
    ReqSE = case parse(Req) of
        {ok, ReqSE0, _} -> ReqSE0;
        _ -> 0
    end,
            Config = nksip_sipapp_srv:config(AppId),
            Default = nksip_lib:get_value(nksip_timers_se, Config),
    SE = case ReqSE of
        0 -> max(ReqMinSE, Default);
        _ -> max(ReqMinSE, min(ReqSE, Default))
    end,
    case SE of
        ReqSE -> 
            Req;
        _ -> 
            Headers1 = nksip_headers:update(Req, [{single, <<"session-expires">>, SE}]),
            Req#sipmsg{headers=Headers1}
    end;

uac_pre_request(Req, _Call) ->
    Req.


%% @private
-spec uac_pre_response(nksip:request(), nksip:response()) ->
    nksip:response().

uac_pre_response(Req, Resp) ->
    case parse(Resp) of
        {ok, _, _} ->
            Resp;
        undefined ->
            case parse(Req) of
                {ok, SE, _} ->
                    case nksip_sipmsg:supported(<<"timer">>, Req) of
                        true ->
                            SE_Token = {nksip_lib:to_binary(SE), [{<<"refresher">>, <<"uac">>}]},
                            Headers1 = nksip_headers:update(Resp, 
                                [{single, <<"session-expires">>, SE_Token}]),
                            #sipmsg{require=Require} = Resp,
                            Require1 = nksip_lib:store_value(<<"timer">>, Require),
                            Resp#sipmsg{require=Require1, headers=Headers1};
                        false ->
                            Resp
                    end;
                _ ->
                    Resp
            end
    end.


%% @private
-spec parse(nksip:request() | nksip:response()) ->
    {ok, SE, Refresher} | undefined | invalid
    when SE :: pos_integer(), Refresher :: uac | uas | undefined.

parse(SipMsg) ->
    case nksip_sipmsg:header(<<"session-expires">>, SipMsg, tokens) of
        [] ->
            undefined;
        [{SE, Opts}] ->
            case nksip_lib:to_integer(SE) of
                SE1 when is_integer(SE1), SE1>0 -> 
                    case nksip_lib:get_binary(<<"refresher">>, Opts) of
                        <<"uac">> -> {ok, SE1, uac};
                        <<"uas">> -> {ok, SE1, uas};
                        _ -> {ok, SE1, undefined}
                    end;
                _ ->
                    invalid
            end;
        _ ->
            invalid
    end.




%% @private
-spec start_timer(integer(), atom(), nksip_dialog_lib:id()) ->
    reference().

start_timer(Time, Tag, Id) ->
    erlang:start_timer(round(Time) , self(), {dlg, Tag, Id}).



