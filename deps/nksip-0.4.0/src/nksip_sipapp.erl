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

%% @doc SipApp callback behaviour and default implementation.
-module(nksip_sipapp).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([sip_get_user_pass/4, sip_authorize/3, sip_route/5]).
-export([sip_invite/2, sip_reinvite/2, sip_cancel/3, sip_ack/2, sip_bye/2]).
-export([sip_options/2, sip_register/2, sip_info/2, sip_update/2]).
-export([sip_subscribe/2, sip_resubscribe/2, sip_notify/2, sip_message/2]).
-export([sip_refer/2, sip_publish/2]).
-export([sip_dialog_update/3, sip_session_update/3]).
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-include("nksip.hrl").


%%%%%%%%%%%%%% SIP Callbacks %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% @doc Called to check a user password for a realm.
-spec sip_get_user_pass(User::binary(), Realm::binary(), Req::nksip:request(), 
                        Call::nksip:call()) ->
    true | false | binary().

sip_get_user_pass(<<"anonymous">>, _, _Req, _Call) -> <<>>;
sip_get_user_pass(_User, _Realm, _Req, _Call) -> false.



%% @doc Called for every incoming request to be authorized or not.
-spec sip_authorize(AuthList, Req::nksip:request(), Call::nksip:call()) ->
    ok | forbidden | authenticate | {authenticate, Realm::binary()} |
    proxy_authenticate | {proxy_authenticate, Realm::binary()}
    when AuthList :: [dialog|register|{{digest, Realm::binary}, boolean()}].

sip_authorize(_AuthList, _Req, _Call) ->
    ok.


%% @doc This function is called by NkSIP for every new request, to check if it must be 
-spec sip_route(Scheme::nksip:scheme(), User::binary(), Domain::binary(), 
            Req::nksip:request(), Call::nksip:call()) ->
    proxy | {proxy, ruri | nksip:uri_set()} | 
    {proxy, ruri | nksip:uri_set(), nksip:optslist()} | 
    proxy_stateless | {proxy_stateless, ruri | nksip:uri_set()} | 
    {proxy_stateless, ruri | nksip:uri_set(), nksip:optslist()} | 
    process | process_stateless |
    {reply, nksip:sipreply()} | {reply_stateless, nksip:sipreply()}.

sip_route(_Scheme, _User, _Domain, _Req, _Call) ->
    process.


%% @doc Called when a OPTIONS request is received.
-spec sip_options(Req::nksip:request(), Call::nksip:call()) ->
    {reply, nksip:sipreply()} | noreply.

sip_options(_Req, _Call) ->
    {reply, {ok, [contact, allow, allow_event, accept, supported]}}.
    

%% @doc This function is called by NkSIP to process a new incoming REGISTER request. 
-spec sip_register(Req::nksip:request(), Call::nksip:call()) ->
    {reply, nksip:sipreply()} | noreply.

sip_register(Req, _Call) ->
    {ok, AppId} = nksip_request:app_id(Req),
    {reply, {method_not_allowed, AppId:config_allow()}}.


%% @doc This function is called by NkSIP to process a new INVITE request as an endpoint.
-spec sip_invite(Req::nksip:request(), Call::nksip:call()) ->
    {reply, nksip:sipreply()} | noreply.

sip_invite(_Req, _Call) ->
    {reply, decline}.


%% @doc This function is called when a new in-dialog INVITE request is received.
-spec sip_reinvite(Req::nksip:request(), Call::nksip:call()) ->
    {reply, nksip:sipreply()} | noreply.

sip_reinvite(Req, Call) ->
    {ok, AppId} = nksip_request:app_id(Req),
    AppId:sip_invite(Req, Call).


%% @doc Called when a pending INVITE request is cancelled.
-spec sip_cancel(InviteReq::nksip:request(), Req::nksip:request(), Call::nksip:call()) ->
    ok.

sip_cancel(_CancelledReq, _Req, _Call) ->
    ok.


%% @doc Called when a valid ACK request is received.
-spec sip_ack(Req::nksip:request(), Call::nksip:call()) ->
    ok.

sip_ack(_Req, _Call) ->
    ok.


%% @doc Called when a valid BYE request is received.
-spec sip_bye(Req::nksip:request(), Call::nksip:call()) ->
    {reply, nksip:sipreply()} | noreply.

sip_bye(_Req, _Call) ->
    {reply, ok}.


%% @doc Called when a valid UPDATE request is received.
-spec sip_update(Req::nksip:request(), Call::nksip:call()) ->
    {reply, nksip:sipreply()} | noreply.

sip_update(_Req, _Call) ->
    {reply, decline}.


%% @doc This function is called by NkSIP to process a new incoming SUBSCRIBE
-spec sip_subscribe(Req::nksip:request(), Call::nksip:call()) ->
    {reply, nksip:sipreply()} | noreply.

sip_subscribe(_Req, _Call) ->
    {reply, decline}.


%% @doc This function is called by NkSIP to process a new in-subscription SUBSCRIBE
-spec sip_resubscribe(Req::nksip:request(), Call::nksip:call()) ->
    {reply, nksip:sipreply()} | noreply.

sip_resubscribe(_Req, _Call) ->
    {reply, ok}.


%% @doc This function is called by NkSIP to process a new incoming NOTIFY
-spec sip_notify(Req::nksip:request(), Call::nksip:call()) ->
    {reply, nksip:sipreply()} | noreply.

sip_notify(_Req, _Call) ->
    {reply, ok}.


%% @doc This function is called by NkSIP to process a new incoming REFER.
-spec sip_refer(Req::nksip:request(), Call::nksip:call()) ->
    {reply, nksip:sipreply()} | noreply.

sip_refer(_Req, _Call) ->
    {reply, decline}.


%% @doc This function is called by NkSIP to process a new incoming PUBLISH request. 
-spec sip_publish(Req::nksip:request(), Call::nksip:call()) ->
    {reply, nksip:sipreply()} | noreply.

sip_publish(Req, _Call) ->
    {ok, AppId} = nksip_request:app_id(Req),
    {reply, {method_not_allowed, AppId:config_allow()}}.


%% @doc Called when a valid INFO request is received.
-spec sip_info(Req::nksip:request(), Call::nksip:call()) ->
    {reply, nksip:sipreply()} | noreply.

sip_info(_Req, _Call) ->
    {reply, ok}.


%% @doc This function is called by NkSIP to process a new incoming MESSAGE
-spec sip_message(Req::nksip:request(), Call::nksip:call()) ->
    {reply, nksip:sipreply()} | noreply.

sip_message(_Req, _Call) ->
    {reply, decline}.


%% @doc Called when a dialog has changed its state.
-spec sip_dialog_update(DialogStatus, Dialog::nksip:dialog(), Call::nksip:call()) ->
    ok when 
        DialogStatus :: start | target_update | 
                        {invite_status, nksip_dialog:invite_status()} |
                        {subscription_status, nksip_subscription:status(), nksip:subscription()} |
                        {stop, nksip_dialog:stop_reason()}.
    
sip_dialog_update(_Status, _Dialog, _Call) ->
    ok.


%% @doc Called when a dialog has updated its SDP session parameters.
-spec sip_session_update(SessionStatus, Dialog::nksip:dialog(), Call::nksip:call()) ->
    ok when 
        SessionStatus :: {start, Local, Remote} | {update, Local, Remote} | stop,
                         Local::nksip_sdp:sdp(), Remote::nksip_sdp:sdp().

sip_session_update(_Status, _Dialog, _Call) ->
    ok.



%%%%%%%%%%%%%% gen_server Callbacks %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%% @doc SipApp initialization.
%%
%% This callback function is called when the SipApp is launched using 
%% {@link nksip:start/4}.
%%
%% If `{ok, State}' or `{ok, State, Timeout}' is returned the SipApp is started with
%% this initial state. If a `Timeout' is provided (in milliseconds) a 
%% `timeout' message will be sent to the process 
%% (you will need to implement {@link handle_info/2} to receive it).
%% If `{stop, Reason}' is returned the SipApp will not start. 
%%
-spec init(Args::term()) ->
    {ok, State::term()} | {ok, State::term(), Timeout::timeout()} |
    {stop, Reason::term()}.

init(Arg) ->
    {ok, Arg}.


%% @doc Called when the SipApp is stopped.
-spec terminate(Reason::term(), State::term()) ->
    ok.

terminate(_Reason, _State) ->
    ok.


%% @doc Called when a direct call to the SipApp process is made using 
%% {@link nksip:call/2} or {@link nksip:call/3}.
-spec handle_call(Msg::term(), From::from(), State::term()) ->
    {reply, Reply::RetType, State::term()} | 
    {reply, Reply::RetType, State::term(), Timeout::timeout()} |
    {noreply, State::term()} | 
    {noreply, State::term(), Timeout::timeout()} |
    {stop, Reason::term(), Reply::RetType, State::term()} | 
    {stop, Reason::term(), State::term()}.

handle_call(Msg, _From, State) ->
    lager:warning("Unexpected handle_call in ~p: ~p", [Msg, ?MODULE]),
    {noreply, State}.


%% @doc Called when a direct cast to the SipApp process is made using 
%% {@link nksip:cast/2}.
-spec handle_cast(Msg::term(), State::term()) ->
    {noreply, State::term()} |
    {noreply, State::term(), Timeout::timeout()} |
    {stop, Reason::term(), State::term()}.

handle_cast(Msg, State) ->
    lager:warning("Unexpected handle_cast in ~p: ~p", [Msg, ?MODULE]),
    {noreply, State}.


%% @doc Called when the SipApp process receives an unknown message.
-spec handle_info(Msg::term(), State::term()) ->
    {noreply, State::term()} |
    {noreply, State::term(), Timeout::timeout()} |
    {stop, Reason::term(), State::term()}.

handle_info(_Msg, State) ->
    {noreply, State}.



%%%%%%%%%%%%%% Other Callbacks %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%







