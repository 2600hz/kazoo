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

%% @doc User Request Management Functions.

-module(nksip_request).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([field/2, fields/2, header/2]).
-export([body/1, method/1, dialog_id/1, call_id/1, get_request/1]).
-export([is_local_route/1, reply/2]).
-export_type([field/0]).

-include("nksip.hrl").



%% ===================================================================
%% Types
%% ===================================================================

-type field() ::  app_id | method | call_id | vias | parsed_vias | 
                  ruri | ruri_scheme | ruri_user | ruri_domain | parsed_ruri | aor |
                  from | from_scheme | from_user | from_domain | parsed_from | 
                  to | to_scheme | to_user | to_domain | parsed_to | 
                  cseq | parsed_cseq | cseq_num | cseq_method | forwards |
                  routes | parsed_routes | contacts | parsed_contacts | 
                  content_type | parsed_content_type | 
                  require | parsed_require | 
                  supported | parsed_supported | 
                  expires | parsed_expires | event | parsed_event |
                  all_headers | body | dialog_id | local | remote |
                  binary().



%% ===================================================================
%% Public
%% ===================================================================

%% @doc Gets specific information from the `Request'. 
%% The available fields are:
%%  
%% <table border="1">
%%      <tr><th>Field</th><th>Type</th><th>Description</th></tr>
%%      <tr>
%%          <td>`app_id'</td>
%%          <td>{@link nksip:app_id()}</td>
%%          <td>SipApp this request belongs to</td>
%%      </tr>
%%      <tr>
%%          <td>`method'</td>
%%          <td>{@link nksip:method()}</td>
%%          <td>Method</td>
%%      </tr>
%%      <tr>
%%          <td>`ruri'</td>
%%          <td>`binary()'</td>
%%          <td>Request-Uri</td>
%%      </tr>
%%      <tr>
%%          <td>`ruri_scheme'</td>
%%          <td>`nksip:scheme()'</td>
%%          <td>Request-Uri Scheme</td>
%%      </tr>
%%      <tr>
%%          <td>`ruri_user'</td>
%%          <td>`binary()'</td>
%%          <td>Request-Uri User</td>
%%      </tr>
%%      <tr>
%%          <td>`ruri_domain'</td>
%%          <td>`binary()'</td>
%%          <td>Request-Uri Domain</td>
%%      </tr>
%%      <tr>
%%          <td>`parsed_ruri'</td>
%%          <td>{@link nksip:uri()}</td>
%%          <td>Request-Uri</td>
%%      </tr>
%%      <tr>
%%          <td>`aor'</td>
%%          <td>{@link nksip:aor()}</td>
%%          <td>Address-Of-Record of the Request-Uri</td>
%%      </tr>
%%      <tr>
%%          <td>`call_id'</td>
%%          <td>{@link nksip:call_id()}</td>
%%          <td>Call-ID Header</td>
%%      </tr>
%%      <tr>
%%          <td>`vias'</td>
%%          <td>`[binary()]'</td>
%%          <td>Via Headers</td>
%%      </tr>
%%      <tr>
%%          <td>`parsed_vias'</td>
%%          <td>`['{@link nksip:via()}`]'</td>
%%          <td>Via Headers</td>
%%      </tr>
%%      <tr>
%%          <td>`from'</td>
%%          <td>`binary()'</td>
%%          <td>From Header</td>
%%      </tr>
%%      <tr>
%%          <td>`from_scheme'</td>
%%          <td>`nksip:scheme()'</td>
%%          <td>From Scheme</td>
%%      </tr>
%%      <tr>
%%          <td>`from_user'</td>
%%          <td>`binary()'</td>
%%          <td>From User</td>
%%      </tr>
%%      <tr>
%%          <td>`from_domain'</td>
%%          <td>`binary()'</td>
%%          <td>From Domain</td>
%%      </tr>
%%      <tr>
%%          <td>`parsed_from'</td>
%%          <td>{@link nksip:uri()}</td>
%%          <td>From Header</td>
%%      </tr>
%%      <tr>
%%          <td>`to'</td>
%%          <td>`binary()'</td>
%%          <td>To Header</td>
%%      </tr>
%%      <tr>
%%          <td>`to_scheme'</td>
%%          <td>`nksip:scheme()'</td>
%%          <td>To Scheme</td>
%%      </tr>
%%      <tr>
%%          <td>`to_user'</td>
%%          <td>`binary()'</td>
%%          <td>To User</td>
%%      </tr>
%%      <tr>
%%          <td>`to_domain'</td>
%%          <td>`binary()'</td>
%%          <td>To Domain</td>
%%      </tr>
%%      <tr>
%%          <td>`parsed_to'</td>
%%          <td>{@link nksip:uri()}</td>
%%          <td>To Header</td>
%%      </tr>
%%      <tr>
%%          <td>`cseq'</td>
%%          <td>`binary()'</td>
%%          <td>CSeq Header</td>
%%      </tr>
%%      <tr>
%%          <td>`parsed_cseq'</td>
%%          <td>`{integer(), '{@link nksip:method()}`}'</td>
%%          <td>CSeq Header</td>
%%      </tr>
%%      <tr>
%%          <td>`forwards'</td>
%%          <td>`integer()'</td>
%%          <td>Forwards</td>
%%      </tr>
%%      <tr>
%%          <td>`routes'</td>
%%          <td>`[binary()]'</td>
%%          <td>Route Headers</td>
%%      </tr>
%%      <tr>
%%          <td>`parsed_routes'</td>
%%          <td>`['{@link nksip:uri()}`]'</td>
%%          <td>Route Headers</td>
%%      </tr>
%%      <tr>
%%          <td>`contacts'</td>
%%          <td>`[binary()]'</td>
%%          <td>Contact Headers</td>
%%      </tr>
%%      <tr>
%%          <td>`parsed_contacts'</td>
%%          <td>`['{@link nksip:uri()}`]'</td>
%%          <td>Contact Headers</td>
%%      </tr>
%%      <tr>
%%          <td>`content_type'</td>
%%          <td>`binary()'</td>
%%          <td>Content-Type Header</td>
%%      </tr>
%%      <tr>
%%          <td>`parsed_content_type'</td>
%%          <td>`['{@link nksip:token()}`]'</td>
%%          <td>Content-Type Header</td>
%%      </tr>
%%      <tr>
%%          <td>`require'</td>
%%          <td>`binary()'</td>
%%          <td>Require Header</td>
%%      </tr>
%%      <tr>
%%          <td>`parsed_require'</td>
%%          <td>`['{@link nksip:token()}`]'</td>
%%          <td>Require Header</td>
%%      </tr>
%%      <tr>
%%          <td>`expires'</td>
%%          <td>`binary()'</td>
%%          <td>Expires Header</td>
%%      </tr>
%%      <tr>
%%          <td>`parsed_expires'</td>
%%          <td>`undefined | integer()'</td>
%%          <td>Expires Header</td>
%%      </tr>
%%      <tr>
%%          <td>`supported'</td>
%%          <td>`binary()'</td>
%%          <td>Supported Header</td>
%%      </tr>
%%      <tr>
%%          <td>`parsed_supported'</td>
%%          <td>`['{@link nksip:token()}`]'</td>
%%          <td>Supported Header</td>
%%      </tr>
%%      <tr>
%%          <td>`event</td>
%%          <td>`undefined | binary()'</td>
%%          <td>Event Header</td>
%%      </tr>
%%      <tr>
%%          <td>`parsed_event'</td>
%%          <td><code>undefined | {@link nksip:token()}'</code></td>
%%          <td>Event Header</td>
%%      </tr>
%%      <tr>
%%          <td>`all_headers'</td>
%%          <td>`[{binary(), binary()}]'</td>
%%          <td>All headers in the request</td>
%%      </tr>
%%      <tr>
%%          <td>`body'</td>
%%          <td>{@link nksip:body()}</td>
%%          <td>Parsed Body</td>
%%      </tr>
%%      <tr>
%%          <td>`dialog_id'</td>
%%          <td>{@link nksip_dialog:id()}</td>
%%          <td>Dialog's Id (if the request has To Tag)</td>
%%      </tr>
%%      <tr>
%%          <td>`local'</td>
%%          <td>`{'{@link nksip:protocol()}, {@link inet:ip_address()}, 
%%                  {@link inet:port_number()}`}'</td>
%%          <td>Local transport protocol, ip and port of a request</td>
%%      </tr>
%%      <tr>
%%          <td>`remote'</td>
%%          <td>`{'{@link nksip:protocol()}, {@link inet:ip_address()}, 
%%                  {@link inet:port_number()}`}'</td>
%%          <td>Remote transport protocol, ip and port of a request</td>
%%      </tr>
%%      <tr>
%%          <td>`binary()'</td>
%%          <td>`{binary(), [binary()]}'</td>
%%          <td>If you use a binary as a field name, NkSIP will return all the values
%%              of this header, or `[]' if it is not present</td>
%%      </tr>
%% </table>
-spec field(nksip:id(), field()) ->
    term() | error.

field(Id, Field) -> 
    case fields(Id, [Field]) of
        [{Field, Value}] -> Value;
        error -> error
    end.


%% @doc Gets some fields from a request.
-spec fields(nksip:id(), [field()]) ->
    [{atom(), term()}] | error.

fields(<<"R_", _/binary>>=Id, Fields) -> 
    Fun = fun(Req) -> {ok, lists:zip(Fields, nksip_sipmsg:fields(Req, Fields))} end,
    case nksip_call_router:apply_sipmsg(Id, Fun) of
        {ok, Values} -> Values;
        _ -> error
    end.


%% @doc Gets values for a header in a request.
-spec header(nksip:id(), binary()) ->
    [binary()] | error.

header(<<"R_", _/binary>>=Id, Name) -> 
    Fun = fun(Req) -> {ok, nksip_sipmsg:header(Req, Name)} end,
    case nksip_call_router:apply_sipmsg(Id, Fun) of
        {ok, Values} -> Values;
        _ -> error
    end.


%% @doc Gets the <i>method</i> of a request.
-spec method(nksip:id()) ->
    nksip:method() | error.

method(Id) -> 
    field(Id, method).


%% @doc Gets the <i>body</i> of a request.
-spec body(nksip:id()) ->
    nksip:body() | error.

body(Id) -> 
    field(Id, body).


%% @doc Gets the <i>dialog_id</i> of a request.
-spec dialog_id(nksip:id()) ->
    nksip_dialog:id() | error.

dialog_id(Id) -> 
    field(Id, dialog_id).


%% @private
-spec get_request(nksip:id()) ->
    nksip:request() | error.

get_request(<<"R_", _/binary>>=Id) ->
    Fun = fun(Req) -> {ok, Req} end,
    case nksip_call_router:apply_sipmsg(Id, Fun) of
        {ok, SipMsg} -> SipMsg;
        _ -> error
    end.


%% @doc Gets the calls's id of a request id
-spec call_id(nksip:id()) ->
    nksip:call_id().

call_id(Id) ->
    {req, _AppId, _MsgId, CallId} = nksip_sipmsg:parse_id(Id),
    CallId.
   

%% @doc Sends a reply to a request.
-spec reply(nksip:id()|nksip:request(), nksip:sipreply()) -> 
    ok | {error, Error}
    when Error :: invalid_call | unknown_call | sipapp_not_found.

reply(<<"R_", _/binary>>=Id, SipReply) ->
    nksip_call:send_reply(Id, SipReply);

reply(#sipmsg{class={req, _}}=Req, SipReply) ->
    reply(nksip_sipmsg:get_id(Req), SipReply).


%% @doc Checks if this request would be sent to a local address in case of beeing proxied.
%% It will return `true' if the first <i>Route</i> header points to a local address
%% or the <i>Request-Uri</i> if there is no <i>Route</i> headers.
-spec is_local_route(nksip:id()|nksip:request()) -> 
    boolean().

is_local_route(<<"R_", _/binary>>=Id) ->
    {req, AppId, _MsgId, _CallId} = nksip_sipmsg:parse_id(Id),
    case fields(Id, [parsed_ruri, parsed_routes]) of
        [{_, RUri}, {_, []}] -> nksip_transport:is_local(AppId, RUri);
        [{_, _RUri}, {_, [Route|_]}] -> nksip_transport:is_local(AppId, Route);
        error -> false
    end;

is_local_route(#sipmsg{class={req, _}, app_id=AppId, ruri=RUri, routes=Routes}) ->
    case Routes of
        [] -> nksip_transport:is_local(AppId, RUri);
        [Route|_] -> nksip_transport:is_local(AppId, Route)
    end.

