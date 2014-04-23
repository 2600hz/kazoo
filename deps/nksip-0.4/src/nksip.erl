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
-export([call/2, call/3, cast/2, reply/2, get_pid/1, get_port/3, find_app/1]).
-export([get_uuid/1, get_gruu_pub/1, get_gruu_temp/1]).

-include("nksip.hrl").

-export_type([app_id/0, id/0, request/0, response/0, transport/0, sipreply/0]).
-export_type([uri/0, user_uri/0, header/0, header_value/0]).
-export_type([scheme/0, protocol/0, method/0, response_code/0, via/0]).
-export_type([call_id/0, cseq/0, tag/0, body/0, uri_set/0, aor/0]).
-export_type([dialog/0, invite/0, subscription/0, token/0, error_reason/0]).



%% ===================================================================
%% Types
%% ===================================================================

% Util types
-type name() :: binary() | string() | atom().
-type value() :: binary() | string() | token() | atom() | integer().


%% Unique Id of each started SipApp
-type app_id() :: term().

%% External request, response, dialog or event id
-type id() :: binary().

%% Parsed SIP Request
-type request() :: #sipmsg{}.

%% Parsed SIP Response
-type response() :: #sipmsg{}.

%% User's response to a request
-type sipreply() :: nksip_reply:sipreply().

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

%% SIP Generic Header Value
-type header_value() :: value() | uri() | via() | [value() | uri() | via()].

%% SIP Generic Header
-type header() :: {name(), header_value()}.

%% Recognized transport schemes
-type protocol() :: udp | tcp | tls | sctp | ws | wss | binary().

%% Recognized SIP schemes
-type scheme() :: sip | sips | tel | mailto | binary().

%% SIP Method
-type method() :: 'INVITE' | 'ACK' | 'CANCEL' | 'BYE' | 'REGISTER' | 'OPTIONS' |
                  'SUBSCRIBE' | 'NOTIFY' | 'PUBLISH' | 'REFER' | 'MESSAGE' |
                  'INFO' | 'PRACK' | 'UPDATE' | binary().

%% SIP Response's Code
-type response_code() :: 100..699.


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
-type subscription() :: #subscription{}.

%% Dialog
-type invite() :: #invite{}.

%% Reason
-type error_reason() :: 
    {sip|q850, pos_integer()} |
    {sip|q850, pos_integer(), string()|binary()}.


%% ===================================================================
%% Public functions
%% ===================================================================

%% @doc Starts a new SipApp.
%% A <b>SipApp</b> is a SIP application started by NkSIP, listening on one or several
%% sets of transport protocol, IP and port of the host. You must supply an `AppId' 
%% for the SipApp, a <i>callbacke</i> `Module' with {@link nksip_sipapp} behaviour, 
%% an `Args' for calling `init/1' and a set of `Options'
%%
%% The recognized options are:<br/><br/>
%% <table border="1">
%%      <tr><th>Key</th><th>Type</th><th>Default</th><th>Description</th></tr>
%%      <tr>
%%          <td>`from'</td>
%%          <td>{@link user_uri()}</td>
%%          <td>`"NkSIP App <sip:user@nksip>"'</td>
%%          <td>Default <i>From</i> to use in the requests.</td>
%%      </tr>
%%      <tr>
%%          <td>`pass'</td>
%%          <td>`Pass | {Pass, Realm} | [Pass | {Pass, Realm}]'<br/>
%%              `Pass::binary(), Realm::binary()'</td>
%%          <td></td>
%%          <td>Passwords to use in case of receiving an <i>authenticate</i> response
%%          using {@link nksip_uac} functions.<br/>
%%          The first password matching the response's realm will be used, 
%%          or the first without any realm if none matches. <br/>
%%          A hash of the password can be used instead 
%%          (see {@link nksip_auth:make_ha1/3}).</td>
%%      </tr>
%%      <tr>
%%          <td>`register'</td>
%%          <td>{@link user_uri()}</td>
%%          <td></td>
%%          <td>NkSIP will try to <i>REGISTER</i> the SipApp with this registrar server
%%          or servers (i.e. "sips:sip2sip.info,sips:other.com"). <br/> 
%%          If the SipApp supports outbound (RFC5626), a new reg_id will be generated 
%%          for each one, a flow will be stablished, and,
%%          if the remote party also supports outbound, keep alive messages will be
%%          sent over each flow.
%%          See {@link nksip_sipapp_auto:get_registers/1}
%%          and {@link nksip_sipapp:register_update/3}.</td>
%%      </tr>
%%      <tr>
%%          <td>`register_expires'</td>
%%          <td>`integer()'</td> 
%%          <td>`300'</td>
%%          <td>In case of register, registration interval (secs).</td>
%%      </tr>
%%      <tr>
%%          <td>`transports'</td>
%%          <td>
%%              `[{Proto, Ip, Port}]'<br/>
%%              <code>Proto::{@link protocol()}</code><br/>
%%              `Ip::inet:ip_address()|string()|binary()|any|any6'<br/>
%%              `Port::inet:port_number()|all'
%%          </td>
%%          <td>`[{udp, any, all}, {tls, any, all}]'</td>
%%          <td>The SipApp can start any number of transports. 
%%          If an UDP transport is started, a TCP transport on the same IP and port
%%          will be started automatically.<br/>
%%          Use `any' to use <i>all</i> available IPv4 addresses and 
%%          `any6' for all IPv6 addresses, and `all' to use
%%          any available port.</td>
%%      </tr>
%%      <tr>
%%          <td>`listeners'</td>
%%          <td>`integer()'</td>
%%          <td>`1'</td>
%%          <td>Number of pre-started listeners for TCP and TLS
%%          (see <a href="http://ninenines.eu/docs/en/ranch/HEAD/guide/introduction">Ranch's</a> documentation).</td>
%%      </tr>
%%      <tr>
%%          <td>`certfile'</td>
%%          <td>`string()'</td>
%%          <td>`"(privdir)/cert.pem"'</td>
%%          <td> Path to the certificate file for TLS.</td>
%%      </tr>
%%      <tr>
%%          <td>`keyfile'</td>
%%          <td>`string()'</td>
%%          <td>`"(privdir)/key.pem"'</td>
%%          <td>Path to the key file for TLS.</td>
%%      </tr>
%%      <tr>
%%          <td>`route'</td>
%%          <td>{@link user_uri()}</td>
%%          <td></td>
%%          <td> Route (outbound proxy) to use. Generates one or more `Route' headers
%%              in every request, for example `<sip:1.2.3.4;lr>, <sip:abcd;lr>' 
%%              (you will usually append the `lr' option to use <i>loose routing</i>).
%%          </td>
%%      </tr>
%%      <tr>
%%          <td>`local_host'</td>
%%          <td>`auto|string()|binary()'</td>
%%          <td>`auto'</td>
%%          <td>Default host or IP to use in headers like `Via', `Contact' and 
%%          `Record-Route'.<br/>
%%          If set to `auto' NkSIP will use the IP of the
%%          transport selected in every case. If that transport is listening on all
%%          addresses NkSIP will try to find the best IP using the first 
%%          valid IP among the network interfaces `ethX' and 'enX',
%%          or localhost if none is found.</td>
%%      </tr>
%%      <tr>
%%          <td>`local_host6'</td>
%%          <td>`auto|string()|binary()'</td>
%%          <td>`auto'</td>
%%          <td>Default host or IP to use in headers like `Via', `Contact' and 
%%          `Record-Route' for IPv6 transports.<br/>
%%          See `local_host' option.</td>
%%      </tr>
%%      <tr>
%%          <td>`registrar'</td>
%%          <td></td>
%%          <td></td>
%%          <td>If present, allows the automatic processing <i>REGISTER</i> requests, 
%%          even if no `register/3' callback  is defined, using 
%%          {@link nksip_sipapp:register/3}.<br/>
%%          The word <i>REGISTER</i> will also be present in all <i>Allow</i> headers.
%%          </td>
%%      </tr>
%%      <tr>
%%          <td>`no_100'</td>
%%          <td></td>
%%          <td></td>
%%          <td>If present, forbids the generation of automatic `100-type' responses
%%          for INVITE requests.</td>
%%      </tr>
%%      <tr>
%%          <td>`supported'</td>
%%          <td>`string()|binary()'</td>
%%          <td>`"100rel"'</td>
%%          <td>If present, these tokens will be used in Supported headers instead of
%%          the default supported list, for example
%%          "my_token1, mytoken2, 100rel".</td>
%%      </tr>
%%      <tr>
%%          <td>`event'</td>
%%          <td>`string()|binary()'</td>
%%          <td>`""'</td>
%%          <td>Lists the Event Packages this SipApp supports.</td>
%%      </tr>
%%      <tr>
%%          <td>`accept'</td>
%%          <td>`string()|binary()'</td>
%%          <td>`"*/*"'</td>
%%          <td>If defined, this value will be used instead of default when 
%%          option `accept' is used</td>
%%      </tr>
%%  </table>
%%
%% <br/>
-spec start(term(), atom(), term(), nksip_lib:optslist()) -> 
	{ok, app_id()} | {error, term()}.

start(AppName, Module, Args, Opts) ->
    case get_pid(AppName) of
        not_found ->
            Config = nksip_config_cache:app_config(),
            Opts1 = Config ++ [{name, AppName}, {module, Module}|Opts],
            case nksip_sipapp_config:parse_config(Opts1) of
                {ok, AppId} ->
                    case nksip_sup:start_core(AppId, Args) of
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
-spec stop(term()|app_id()) -> 
    ok | error.

stop(App) ->
    case find_app(App) of
        {ok, AppId} ->
            case nksip_sup:stop_core(AppId) of
                ok ->
                    nksip_registrar:clear(AppId),
                    ok;
                error ->
                    error
            end;
        _ ->
            error
    end.


%% @doc Stops all started SipApps.
-spec stop_all() -> 
    ok.

stop_all() ->
    lists:foreach(fun({_, AppId}) -> stop(AppId) end, get_all()).


%% @doc Updates the callback module or options of a running SipApp
%% It is not allowed to change transports
-spec update(term()|app_id(), nksip_lib:optslist()) ->
    {ok, app_id()} | {error, term()}.

update(App, Opts) ->
    case find_app(App) of
        {ok, AppId} ->
            Opts1 = nksip_lib:delete(Opts, transport),
            Opts2 = AppId:config() ++ Opts1,
            nksip_sipapp_config:parse_config(Opts2);
        not_found ->
            {error, sipapp_not_found}
    end.

    

%% @doc Gets the `AppIds' of all started SipApps.
-spec get_all() ->
    [{AppName::term(), AppId::app_id()}].

get_all() ->
    [{AppId:name(), AppId} 
      || {AppId, _Pid} <- nksip_proc:values(nksip_sipapps)].


%% @doc Sends a response from a synchronous callback function.
%% Eequivalent to `gen_server:reply/2'.
-spec reply({reference(), pid()} | {fsm, reference(), pid()}, term()) -> 
    term().

reply(From, Reply) ->
    nksip_sipapp_srv:reply(From, Reply).


%% @doc Gets a value from SipApp's store
-spec get(term()|nksip:app_id(), term()) ->
    {ok, term()} | not_found | error.

get(App, Key) ->
    case find_app(App) of
        {ok, AppId} -> nksip_sipapp_srv:get(AppId, Key);
        not_found -> error
    end.


%% @doc Gets a value from SipApp's store, using a default if not found
-spec get(term()|nksip:app_id(), term(), term()) ->
    {ok, term()} | error.

get(AppId, Key, Default) ->
    case get(AppId, Key) of
        not_found -> {ok, Default};
        {ok, Value} -> {ok, Value};
        error -> error
    end.


%% @doc Inserts a value in SipApp's store
-spec put(term()|nksip:app_id(), term(), term()) ->
    ok | error.

put(App, Key, Value) ->
    case find_app(App) of
        {ok, AppId} -> nksip_sipapp_srv:put(AppId, Key, Value);
        not_found -> error
    end.


%% @doc Deletes a value from SipApp's store
-spec del(term()|nksip:app_id(), term()) ->
    ok | error.

del(App, Key) ->
    case find_app(App) of
        {ok, AppId} -> nksip_sipapp_srv:del(AppId, Key);
        not_found -> error
    end.


%% @doc Sends a synchronous message to the SipApp's process, 
%% similar to `gen_server:call/2'.
%% The SipApp's callback module must implement `handle_call/3'.
-spec call(term()|app_id(), term()) ->
    any().

call(App, Msg) ->
    call(App, Msg, 5000).


%% @doc Sends a synchronous message to the SipApp's process with a timeout, 
%% similar to `gen_server:call/3'.
%% The SipApp's callback module must implement `handle_call/3'.
-spec call(term()|app_id(), term(), infinity|pos_integer()) ->
    any().

call(App, Msg, Timeout) ->
    case get_pid(App) of
        not_found -> error(core_not_found);
        Pid -> gen_server:call(Pid, Msg, Timeout)
    end.


%% @doc Sends an asynchronous message to the SipApp's process, 
%% similar to `gen_server:cast/2'.
%% The SipApp's callback module must implement `handle_cast/2'.
-spec cast(term()|app_id(), term()) ->
    ok.

cast(App, Msg) ->
    case get_pid(App) of
        not_found -> error(core_not_found);
        Pid -> gen_server:cast(Pid, Msg)
    end.


%% @doc Gets the SipApp's process `pid()'.
-spec get_pid(term()|app_id()) -> 
    pid() | not_found.

get_pid(App) ->
    case find_app(App) of
        {ok, AppId} -> nksip_sipapp_srv:get_pid(AppId);
        _ -> not_found
    end.


%% @doc Gets SipApp's first listening port on this transport protocol.
-spec get_port(term()|app_id(), protocol(), ipv4|ipv6) -> 
    inet:port_number() | not_found.

get_port(App, Proto, Class) ->
    case find_app(App) of
        {ok, AppId} -> 
            case nksip_transport:get_listening(AppId, Proto, Class) of
                [{#transport{listen_port=Port}, _Pid}|_] -> Port;
                _ -> not_found
            end;
        not_found ->
            not_found
    end.


%% @private
-spec find_app(term()) ->
    {ok, app_id()} | not_found.

find_app(App) when is_atom(App) ->
    case erlang:function_exported(App, init, 1) of
        true ->
            {ok, App};
        false ->
            case nksip_proc:values({nksip_sipapp_name, App}) of
                [] -> not_found;
                [{AppId, _}] -> {ok, AppId}
            end
    end;

find_app(App) ->
    case nksip_proc:values({nksip_sipapp_name, App}) of
        [] -> not_found;
        [{AppId, _}] -> {ok, AppId}
    end.



%% @doc Gets SipApp's module and pid
-spec get_uuid(term()|nksip:app_id()) -> 
    {ok, binary()} | {error, not_found}.

get_uuid(App) ->
    case find_app(App) of
        {ok, AppId} ->
            case nksip_proc:values({nksip_sipapp_uuid, AppId}) of
                [{UUID, _Pid}] -> {ok, <<"<urn:uuid:", UUID/binary, ">">>};
                [] -> {error, not_found}
            end;
        not_found -> 
            {error, not_found}
    end.


%% @doc Gets the last detected public GRUU
-spec get_gruu_pub(term()|nksip:app_id()) ->
    undefined | nksip:uri() | error.

get_gruu_pub(App) ->
    case find_app(App) of
        {ok, AppId} -> nksip_config:get({nksip_gruu_pub, AppId});
        _ -> error
    end.


%% @doc Gets the last detected temporary GRUU
-spec get_gruu_temp(term()|nksip:app_id()) ->
    undefined | nksip:uri() | error.

get_gruu_temp(App) ->
    case find_app(App) of
        {ok, AppId} -> nksip_config:get({nksip_gruu_temp, AppId});
        _ -> error
    end.







%% ===================================================================
%% Private
%% ===================================================================


