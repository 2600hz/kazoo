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

%% @doc SipApp plugin callbacks default implementation
-module(nksip_callbacks).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-include("nksip.hrl").
-include("nksip_call.hrl").
-export([nkcb_call/3, nkcb_sip_method/2, nkcb_authorize_data/3, 
		 nkcb_transport_uac_headers/6, nkcb_transport_uas_sent/1]).
-export([nkcb_uac_pre_response/3, nkcb_uac_response/4, nkcb_parse_uac_opts/2,
		 nkcb_uac_proxy_opts/2, nkcb_make_uac_dialog/4, nkcb_uac_pre_request/4,
		 nkcb_uac_reply/3]).
-export([nkcb_uas_send_reply/3, nkcb_uas_sent_reply/1, nkcb_uas_method/4, nkcb_parse_uas_opt/3, nkcb_uas_timer/3, nkcb_uas_dialog_response/4, nkcb_uas_process/2]).
-export([nkcb_dialog_update/3, nkcb_route/4]).
-export([nkcb_connection_sent/2, nkcb_connection_recv/4]).
-export([nkcb_handle_call/3, nkcb_handle_cast/2, nkcb_handle_info/2, 
	     nkcb_sipapp_updated/1]).
-export([nkcb_debug/3]).

-type nkcb_common() :: continue | {continue, list()}.


%% @doc This plugin callback function is used to call application-level 
%% SipApp callbacks.
-spec nkcb_call(atom(), list(), nksip:app_id()) ->
	{ok, term()} | error | nkcb_common().

nkcb_call(Fun, Args, AppId) ->
	case catch apply(AppId, Fun, Args) of
	    {'EXIT', Error} -> 
	        ?call_error("Error calling callback ~p/~p: ~p", [Fun, length(Args), Error]),
	        error;
	    Reply ->
	    	% ?call_warning("Called ~p/~p (~p): ~p", [Fun, length(Args), Args, Reply]),
	    	% ?call_debug("Called ~p/~p: ~p", [Fun, length(Args), Reply]),
	        {ok, Reply}
	end.


%% @doc This plugin callback is called when a call to one of the method specific
%% application-level SipApp callbacks is needed.
-spec nkcb_sip_method(nksip_call:trans(), nksip_call:call()) ->
	{reply, nksip:sipreply()} | noreply | nkcb_common().


nkcb_sip_method(#trans{method='ACK', request=Req}, #call{app_id=AppId}=Call) ->
	case catch AppId:sip_ack(Req, Call) of
		ok -> ok;
		Error -> ?call_error("Error calling callback ack/1: ~p", [Error])
	end,
	noreply;

nkcb_sip_method(#trans{method=Method, request=Req}, #call{app_id=AppId}=Call) ->
	#sipmsg{to={_, ToTag}} = Req,
	Fun = case Method of
		'INVITE' when ToTag == <<>> -> sip_invite;
		'INVITE' -> sip_reinvite;
		'UPDATE' -> sip_update;
		'BYE' -> sip_bye;
		'OPTIONS' -> sip_options;
		'REGISTER' -> sip_register;
		'PRACK' -> sip_prack;
		'INFO' -> sip_info;
		'MESSAGE' -> sip_message;
		'SUBSCRIBE' when ToTag == <<>> -> sip_subscribe;
		'SUBSCRIBE' -> sip_resubscribe;
		'NOTIFY' -> sip_notify;
		'REFER' -> sip_refer;
		'PUBLISH' -> sip_publish
	end,
	case catch AppId:Fun(Req, Call) of
		{reply, Reply} -> 
			{reply, Reply};
		noreply -> 
			noreply;
		Error -> 
			?call_error("Error calling callback ~p/2: ~p", [Fun, Error]),
			{reply, {internal_error, "SipApp Error"}}
	end.


%% @doc This callback is called when the application use has implemented the
%% sip_authorize/3 callback, and a list with authentication tokens must be
%% generated
-spec nkcb_authorize_data(list(), nksip_call:trans(), nksip_call:call()) ->
	{ok, list()} | nkcb_common().

nkcb_authorize_data(List, #trans{request=Req}, Call) ->
	Digest = nksip_auth:authorize_data(Req, Call),
	Dialog = case nksip_call_lib:check_auth(Req, Call) of
        true -> dialog;
        false -> []
    end,
    {ok, lists:flatten([Digest, Dialog, List])}.


%% @doc Called after the UAC pre processes a response
-spec nkcb_uac_pre_response(nksip:response(),  nksip_call:trans(), nksip:call()) ->
	{ok, nksip:call()} | nkcb_common().

nkcb_uac_pre_response(Resp, UAC, Call) ->
    {continue, [Resp, UAC, Call]}.


%% @doc Called after the UAC processes a response
-spec nkcb_uac_response(nksip:request(), nksip:response(), 
					    nksip_call:trans(), nksip:call()) ->
	{ok, nksip:call()} | nkcb_common().

nkcb_uac_response(Req, Resp, UAC, Call) ->
    {continue, [Req, Resp, UAC, Call]}.


%% @doc Called to parse specific UAC options
-spec nkcb_parse_uac_opts(nksip:request(), nksip:optslist()) ->
	{error, term()} | nkcb_common().

nkcb_parse_uac_opts(Req, Opts) ->
	{continue, [Req, Opts]}.


%% @doc Called to add options for proxy UAC processing
-spec nkcb_uac_proxy_opts(nksip:request(), nksip:optslist()) ->
	{reply, nksip:sipreply()} | nkcb_common().

nkcb_uac_proxy_opts(Req, ReqOpts) ->
	{continue, [Req, ReqOpts]}.


%% @doc Called when a new in-dialog request is being generated
-spec nkcb_make_uac_dialog(nksip:method(), nksip:uri(), nksip:optslist(), nksip:call()) ->
	{continue, list()}.

nkcb_make_uac_dialog(Method, Uri, Opts, Call) ->
	{continue, [Method, Uri, Opts, Call]}.


%% @doc Called when the UAC is preparing a request to be sent
-spec nkcb_uac_pre_request(nksip:request(), nksip:optslist(), 
                           nksip_call_uac:uac_from(), nksip:call()) ->
    {continue, list()}.

nkcb_uac_pre_request(Req, Opts, From, Call) ->
	{continue, [Req, Opts, From, Call]}.


%% @doc Called when the UAC transaction must send a reply to the user
-spec nkcb_uac_reply({req, nksip:request()} | {resp, nksip:response()} | {error, term()}, 
                     nksip_call:trans(), nksip_call:call()) ->
    {ok, nksip:call()} | {continue, list()}.

nkcb_uac_reply(Class, UAC, Call) ->
    {continue, [Class, UAC, Call]}.


%% @doc Called to add headers just before sending the request
-spec nkcb_transport_uac_headers(nksip:request(), nksip:optslist(), nksip:scheme(),
							     nksip:protocol(), binary(), inet:port_number()) ->
	{ok, nksip:request()}.

nkcb_transport_uac_headers(Req, Opts, Scheme, Proto, Host, Port) ->
	Req1 = nksip_call_uac_transp:add_headers(Req, Opts, Scheme, Proto, Host, Port),
	{ok, Req1}.


%% @doc Called when a new reponse is going to be sent
-spec nkcb_uas_send_reply({nksip:response(), nksip:optslist()}, 
							 nksip_call:trans(), nksip_call:call()) ->
	{error, term()} | nkcb_common().

nkcb_uas_send_reply({Resp, RespOpts}, UAS, Call) ->
	{continue, [{Resp, RespOpts}, UAS, Call]}.


%% @doc Called when a new reponse is sent
-spec nkcb_uas_sent_reply(nksip_call:call()) ->
	{ok, nksip_call:call()} | nkcb_common().

nkcb_uas_sent_reply(Call) ->
	{continue, [Call]}.

	
%% @doc Called when a new request has to be processed
-spec nkcb_uas_method(nksip:method(), nksip:request(), 
					  nksip_call:trans(), nksip_call:call()) ->
	{ok, nksip_call:trans(), nksip_call:call()} | nkcb_common().

nkcb_uas_method(Method, Req, UAS, Call) ->
	{continue, [Method, Req, UAS, Call]}.


%% @doc Called when a UAS timer is fired
-spec nkcb_uas_timer(nksip_call_lib:timer()|term(), nksip_call:trans(), nksip_call:call()) ->
    {ok, nksip_call:call()} | nkcb_common().

nkcb_uas_timer(Tag, UAS, Call) ->
	{continue, [Tag, UAS, Call]}.


%% @doc Called to parse specific UAS options
-spec nkcb_parse_uas_opt(nksip:request(), nksip:response(), nksip:optslist()) ->
	{error, term()} | nkcb_common().

nkcb_parse_uas_opt(Req, Resp, Opts) ->
	{continue, [Req, Resp, Opts]}.


%% @doc Called when preparing a UAS dialog response
-spec nkcb_uas_dialog_response(nksip:request(), nksip:response(), 
                               nksip:optslist(), nksip:call()) ->
    {ok, nksip:response(), nksip:optslist()}.

nkcb_uas_dialog_response(_Req, Resp, Opts, _Call) ->
    {ok, Resp, Opts}.


%% @doc Called when the UAS is proceesing a request
-spec nkcb_uas_process(nksip_call:trans(), nksip_call:call()) ->
    {ok, nksip:call()} | {continue, list()}.

nkcb_uas_process(UAS, Call) ->
	{continue, [UAS, Call]}.


%% @doc Called when a dialog must update its internal state
-spec nkcb_dialog_update(term(), nksip:dialog(), nksip_call:call()) ->
    {ok, nksip_call:call()} | nkcb_common().

nkcb_dialog_update(Type, Dialog, Call) ->
	{continue, [Type, Dialog, Call]}.


%% @doc Called when a proxy is preparing a routing
-spec nkcb_route(nksip:uri_set(), nksip:optslist(), 
                 nksip_call:trans(), nksip_call:call()) -> 
    {continue, list()} | {reply, nksip:sipreply(), nksip_call:call()}.

nkcb_route(UriList, ProxyOpts, UAS, Call) ->
	{continue, [UriList, ProxyOpts, UAS, Call]}.


%% @doc Called when a new message has been sent
-spec nkcb_connection_sent(nksip:request()|nksip:response(), binary()) ->
	ok | nkcb_common().

nkcb_connection_sent(_SipMsg, _Packet) ->
	ok.


%% @doc Called when a new message has been received and parsed
-spec nkcb_connection_recv(nksip:app_id(), nksip:call_id(), 
					       nksip:transport(), binary()) ->
    ok | nkcb_common().

nkcb_connection_recv(_AppId, _CallId, _Transp, _Packet) ->
	ok.


%% @doc Called when the transport has just sent a response
-spec nkcb_transport_uas_sent(nksip:response()) ->
    ok | nkcb_common().

nkcb_transport_uas_sent(_Resp) ->
	ok.


%% @doc Called when the SipApp process receives a handle_call/3.
%% Return {ok, NewPluginState} (should call gen_server:reply/2) or continue.
-spec nkcb_handle_call(term(), from(), nksip_sipapp_srv:state()) ->
	{ok, nksip_sipapp_srv:state()} | nkcb_common().

nkcb_handle_call(Msg, From, SipAppState) ->
	{continue, [Msg, From, SipAppState]}.


%% @doc Called when the SipApp process receives a handle_cast/3.
%% Return {ok, NewPluginState} or continue.
-spec nkcb_handle_cast(term(), nksip_sipapp_srv:state()) ->
	{ok, nksip_sipapp_srv:state()} | nkcb_common().

nkcb_handle_cast(Msg, SipAppState) ->
	{continue, [Msg, SipAppState]}.


%% @doc Called when the SipApp process receives a handle_info/3.
%% Return {ok, NewPluginState} or continue.
-spec nkcb_handle_info(term(), nksip_sipapp_srv:state()) ->
	{ok, nksip_sipapp_srv:state()} | nkcb_common().

nkcb_handle_info(Msg, SipAppState) ->
	{continue, [Msg, SipAppState]}.


%% @doc Called when the SipApp is updated with a new configuration
-spec nkcb_sipapp_updated(nksip_sipapp_srv:state()) ->
	{ok, nksip_sipapp_srv:state()} | nkcb_common().

nkcb_sipapp_updated(SipAppState) ->
	{ok, SipAppState}.


%% doc Called at specific debug points
-spec nkcb_debug(nksip:app_id(), nksip:call_id(), term()) ->
    ok | nkcb_common().

nkcb_debug(_AppId, _CallId, _Info) ->
    ok.


