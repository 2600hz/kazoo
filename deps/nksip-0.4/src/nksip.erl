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

%% @doc SipApps management module.

-module(nksip).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([start/4, stop/1, stop_all/0, get_all/0, update/2]).
-export([get/2, get/3, put/3, del/2]).
-export([get_pid/1, find_app_id/1, call/3, call/2, cast/2, config/1]).
-export([get_uuid/1]).

-include("nksip.hrl").

-export_type([app_name/0, app_id/0, handle/0]).
-export_type([request/0, response/0, sipreply/0, optslist/0]).
-export_type([call/0, transport/0, uri/0, user_uri/0]).
-export_type([header/0, header_name/0, header_value/0]).
-export_type([scheme/0, protocol/0, method/0, sip_code/0, via/0]).
-export_type([call_id/0, cseq/0, tag/0, body/0, uri_set/0, aor/0]).
-export_type([dialog/0, invite/0, subscription/0, token/0, error_reason/0]).



%% ===================================================================
%% Types
%% ===================================================================

%% User Name of each started SipApp
-type app_name() :: term().

%% Interna Name of each started SipApp
-type app_id() :: atom().

%% External handle for a request, response, dialog or event
%% It is a binary starting with:
%% R_: requests
%% S_: responses
%% D_: dialogs
%% U_: subscriptions
-type handle() :: binary().

%% Parsed SIP Request
-type request() :: #sipmsg{}.

%% Parsed SIP Response
-type response() :: #sipmsg{}.

%% Full call 
-type call() :: nksip_call:call().

%% User's response to a request
-type sipreply() :: nksip_reply:sipreply().

%% Generic options list
-type optslist() :: nksip:optslist().

%% Transport
-type transport() :: #transport{}.

%% Parsed SIP Uri
-type uri() :: #uri{}.

%% User specified uri
-type user_uri() :: string() | binary() | uri().

%% Parsed SIP Via
-type via() :: #via{}.

%% Token
-type token() :: {name(), [{name(), value()}]}.

%% Sip Generic Header Name
-type header_name() :: name().

% Util types
-type header_value() :: 
    value() | uri() | token() | via() | [value() | uri() | token() | via()].

%% SIP Generic Header
-type header() :: {header_name(), header_value()}.

%% Recognized transport schemes
-type protocol() :: udp | tcp | tls | sctp | ws | wss | binary().

%% Recognized SIP schemes
-type scheme() :: sip | sips | tel | mailto | binary().

%% SIP Method
-type method() :: 'INVITE' | 'ACK' | 'CANCEL' | 'BYE' | 'REGISTER' | 'OPTIONS' |
                  'SUBSCRIBE' | 'NOTIFY' | 'PUBLISH' | 'REFER' | 'MESSAGE' |
                  'INFO' | 'PRACK' | 'UPDATE' | binary().

%% SIP Response's Code
-type sip_code() :: 100..699.


%% SIP Message's Call-ID
-type call_id() :: binary().

%% SIP Message's CSeq
-type cseq() :: pos_integer().

%% Tag in From and To headers
-type tag() :: binary().

%% SIP Message body
-type body() :: binary() | string() | nksip_sdp:sdp() | term().

%% Uri Set used to order proxies
-type uri_set() :: nksip:user_uri() | [nksip:user_uri() | [nksip:user_uri()]].

%% Address of Record
-type aor() :: {Scheme::scheme(), User::binary(), Domain::binary()}.

%% Dialog
-type dialog() :: #dialog{}.

%% Dialog
-type subscription() :: {user_subs, #subscription{}, #dialog{}}.

%% Dialog
-type invite() :: #invite{}.

%% Reason
-type error_reason() :: 
    {sip|q850, pos_integer()} |
    {sip|q850, pos_integer(), string()|binary()}.


%% Generic Name
-type name() :: binary() | string() | atom().

% Generic Value
-type value() :: binary() | string() | atom() | integer().


%% ===================================================================
%% Public functions
%% ===================================================================

%% @doc Starts a new SipApp.
-spec start(app_name(), atom(), term(), optslist()) -> 
	{ok, app_id()} | {error, term()}.

start(AppName, Module, Args, Opts) ->
    case get_pid(AppName) of
        undefined ->
            Opts1 = [{name, AppName}, {module, Module}|Opts],
            case nksip_sipapp_config:start(Opts1) of
                {ok, AppId} ->
                    case nksip_sup:start_sipapp(AppId, Args) of
                        ok -> {ok, AppId};
                        {error, Error} -> {error, Error}
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        _ ->
            {error, already_started}
    end.


%% @doc Stops a started SipApp, stopping any registered transports.
-spec stop(app_name()|app_id()) -> 
    ok | {error, not_found}.

stop(App) ->
    case find_app_id(App) of
        {ok, AppId} ->
            case nksip_sup:stop_sipapp(AppId) of
                ok -> 
                    ok;
                error -> 
                    {error, not_found}
            end;
        _ ->
            {error, not_found}
    end.


%% @doc Stops all started SipApps.
-spec stop_all() -> 
    ok.

stop_all() ->
    lists:foreach(fun({_, AppId}) -> stop(AppId) end, get_all()).


%% @doc Updates the callback module or options of a running SipApp.
%% It is not allowed to change transports
-spec update(app_name()|app_id(), optslist()) ->
    {ok, app_id()} | {error, term()}.

update(App, Opts) ->
    case find_app_id(App) of
        {ok, AppId} -> 
            nksip_sipapp_config:update(AppId, Opts);
        not_found ->
            {error, not_found}
    end.

    

%% @doc Gets the user and internal ids of all started SipApps.
-spec get_all() ->
    [{AppName::term(), AppId::app_id()}].

get_all() ->
    [{AppId:name(), AppId} 
      || {AppId, _Pid} <- nksip_proc:values(nksip_sipapps)].


%% @doc Gets a value from SipApp's store
-spec get(nksip:app_name()|nksip:app_id(), term()) ->
    {ok, term()} | undefined | {error, term()}.

get(App, Key) ->
    case find_app_id(App) of
        {ok, AppId} -> nksip_sipapp_srv:get(AppId, Key);
        not_found -> {error, not_found}
    end.


%% @doc Gets a value from SipApp's store, using a default if not found
-spec get(nksip:app_name()|nksip:app_id(), term(), term()) ->
    {ok, term()} | {error, term()}.

get(AppId, Key, Default) ->
    case get(AppId, Key) of
        undefined -> {ok, Default};
        {ok, Value} -> {ok, Value};
        {error, Error} -> {error, Error}
    end.


%% @doc Inserts a value in SipApp's store
-spec put(nksip:app_name()|nksip:app_id(), term(), term()) ->
    ok | {error, term()}.

put(App, Key, Value) ->
    case find_app_id(App) of
        {ok, AppId} -> nksip_sipapp_srv:put(AppId, Key, Value);
        not_found -> {error, not_found}
    end.


%% @doc Deletes a value from SipApp's store
-spec del(nksip:app_name()|nksip:app_id(), term()) ->
    ok | {error, term()}.

del(App, Key) ->
    case find_app_id(App) of
        {ok, AppId} -> nksip_sipapp_srv:del(AppId, Key);
        not_found -> {error, not_found}
    end.


%% @doc Gets the SipApp's gen_server process pid().
-spec get_pid(app_name()|app_id()) -> 
    pid() | undefined.

get_pid(App) ->
    case find_app_id(App) of
        {ok, AppId} -> whereis(AppId);
        _ -> undefined
    end.


%% @doc Synchronous call to the SipApp's gen_server process
-spec call(app_name()|app_id(), term()) ->
    term().

call(App, Term) ->
    call(App, Term, default).


%% @doc Synchronous call to the SipApp's gen_server process with a timeout
-spec call(app_name()|app_id(), term(), pos_integer()|infinity|default) ->
    term().

call(App, Term, Time) ->
    case find_app_id(App) of
        {ok, AppId} -> 
            Time1 = case Time of 
                default -> nksip_config_cache:sync_call_time();
                _ -> Time
            end,
            gen_server:call(AppId, Term, Time1);
        not_found -> 
            error(sipapp_not_found)
    end.


%% @doc Asynchronous call to the SipApp's gen_server process
-spec cast(app_name()|app_id(), term()) ->
    term().

cast(App, Term) ->
    case find_app_id(App) of
        {ok, AppId} -> gen_server:cast(AppId, Term);
        not_found -> error(sipapp_not_found)
    end.


%% @doc Gets the internal name of an existing SipApp
-spec find_app_id(term()) ->
    {ok, app_id()} | not_found.

find_app_id(App) when is_atom(App) ->
    case erlang:function_exported(App, config_local_host, 0) of
        true ->
            {ok, App};
        false ->
            case nksip_proc:values({nksip_sipapp_name, App}) of
                [] -> not_found;
                [{AppId, _}] -> {ok, AppId}
            end
    end;

find_app_id(App) ->
    case nksip_proc:values({nksip_sipapp_name, App}) of
        [] -> not_found;
        [{AppId, _}] -> {ok, AppId}
    end.


%% @doc Gets current SipApp configuration
-spec config(app_name()|app_id()) ->
    nksip:optslist().

config(App) ->
    case find_app_id(App) of
        {ok, AppId} -> AppId:config();
        not_found -> error(sipapp_not_found)
    end.


%% @doc Gets SipApp's UUID
-spec get_uuid(nksip:app_name()|nksip:app_id()) -> 
    {ok, binary()} | {error, term()}.

get_uuid(App) ->
    case find_app_id(App) of
        {ok, AppId} -> 
            UUID = AppId:uuid(),
            {ok, <<"<urn:uuid:", UUID/binary, ">">>};
        not_found -> 
            {error, not_found}
    end.


