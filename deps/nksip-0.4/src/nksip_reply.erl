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

%% @doc User Response generation functions.
%%
%% This module offers helper functions to easy the generation of common SIP responses.
%% Currently the following replies are recognized:
%%
%% <table border="1">
%%   <tr><th>Response</th><th>Code</th><th>Comments</th></tr>
%%   <tr><td>`ringing'</td><td>180</td><td></td></tr>
%%   <tr><td>`{ringing, Body}'</td><td>180</td><td></td></tr>
%%   <tr><td>`session_progress'</td><td>183</td><td></td></tr>
%%   <tr><td>`{session_progress, Body}'</td><td>183</td><td></td></tr>
%%   <tr><td>`rel_ringing'</td><td>180</td>
%%       <td><i>Reliable responses</i> will be used</td></tr>
%%   <tr><td>`{rel_ringing, Body}'</td><td>180</td>
%%       <td><i>Reliable responses</i> will be used</td></tr>
%%   <tr><td>`rel_session_progress'</td><td>183</td>
%%       <td><i>Reliable responses</i> will be used</td></tr>
%%   <tr><td>`{rel_session_progress, Body}'</td>
%%        <td>183</td><td><i>Reliable responses</i> will be used</td></tr>
%%   <tr><td>`ok'</td><td>200</td><td></td></tr>
%%   <tr><td>`{ok, [Header]}'</td><td>200</td>
%%       <td>Response will include provided headers</td></tr>
%%   <tr><td>`{ok, [Header] Body}'</td><td>200</td>
%%       <td>Response will include provided headers and body</td></tr>
%%   <tr><td>`{ok, [Header] Body, Options}'</td><td>200</td>
%%       <td>Response will include provided headers and body, allows options</td></tr>
%%   <tr><td>`answer'</td><td>200</td><td></td></tr>
%%   <tr><td>`{answer, [Header]}'</td><td>200</td>
%%       <td>Response will include provided headers</td></tr>
%%   <tr><td>`{answer, [Header] Body}'</td><td>200</td>
%%       <td>Response will include provided headers and body</td></tr>
%%   <tr><td>`{answer, [Header] Body, Options}'</td><td>200</td>
%%       <td>Response will include provided headers and body allows options</td></tr>
%%   <tr><td>`accepted'</td><td>202</td><td></td></tr>
%%   <tr><td>`{redirect, [Contact]}'</td><td>300</td>
%%       <td>Generates `Contact' headers</td></tr>
%%   <tr><td>`{redirect_permanent, Contact}'</td><td>301</td>
%%       <td>Generates a `Contact' header</td></tr>
%%   <tr><td>`{redirect_temporary, Contact}'</td><td>302</td>
%%       <td>Generates a `Contact' header</td></tr>
%%   <tr><td>`invalid_request'</td><td>400</td><td></td></tr>
%%   <tr><td>`{invalid_request, Text}'</td><td>400</td>
%%       <td>`Text' will be used in SIP reason line</td></tr>
%%   <tr><td>`authenticate'</td><td>401</td>
%%        <td>Generates a new `WWW-Authenticate' header, using current `From' domain 
%%            as `Realm'</td></tr>
%%   <tr><td>`{authenticate, Realm}'</td><td>401</td>
%%       <td>Generates a valid new `WWW-Authenticate' header, using `Realm'</td></tr>
%%   <tr><td>`forbidden'</td><td>403</td><td></td></tr>
%%   <tr><td>`{forbidden, Text}'</td><td>403</td>
%%       <td>`Text' will be used in SIP reason line</td></tr>
%%   <tr><td>`not_found'</td><td>404</td><td></td></tr>
%%   <tr><td>`{not_found, Text}'</td><td>404</td>
%%       <td>`Text' will be used in SIP reason line</td></tr>
%%   <tr><td>`{method_not_allowed, Methods}'</td><td>405</td>
%%       <td>Generates an `Allow' header using `Methods'</td></tr>
%%   <tr><td>`proxy_authenticate'</td><td>407</td>
%%       <td>Generates a valid new `Proxy-Authenticate' header, using current `From' 
%%           domain as `Realm'</td></tr>
%%   <tr><td>`{proxy_authenticate, Realm}'</td>
%%       <td>407</td><td>Generates a valid new `Proxy-Authenticate' header, 
%%                       using `Realm'</td></tr>
%%   <tr><td>`timeout'</td><td>408</td><td></td></tr>
%%   <tr><td>`{timeout, Text}'</td><td>408</td>
%%       <td>`Text' will be used in SIP reason line</td></tr>
%%   <tr><td>`conditional_request_failed</td><td>412</td><td></td></tr>
%%   <tr><td>`too_large'</td><td>413</td><td></td></tr>
%%   <tr><td>`{unsupported_media_type, Types}'</td><td>415</td>
%%       <td>Generates a new `Accept' header using `Types'</td></tr>
%%   <tr><td>`{unsupported_media_encoding, Types}'</td><td>415</td>
%%       <td>Generates a new `Accept-Encoding' header using `Types'</td></tr>
%%   <tr><td>`unsupported_uri_scheme'</td><td>416</td><td></td></tr>
%%   <tr><td>`{bad_extension, Extensions}'</td><td>420</td>
%%       <td>Generates a new `Unsupported' header using `Extensions'</td></tr>
%%   <tr><td>`{extension_required, Extension}'</td><td>421</td>
%%       <td>Generates a new `Require' header using `Extension'</td></tr>
%%   <tr><td>`{interval_too_brief, Time}'</td><td>423</td>
%%        <td>Generates a new `Min-Expires' header using `Time'</td></tr>
%%   <tr><td>`flow_failed'</td><td>430</td><td></td></tr>
%%   <tr><td>`first_hop_lacks_outbound'</td><td>439</td><td></td></tr>
%%   <tr><td>`temporarily_unavailable'</td><td>480</td><td></td></tr>
%%   <tr><td>`no_transaction|unknown_dialog'</td><td>481</td><td></td></tr>
%%   <tr><td>`loop_detected'</td><td>482</td><td></td></tr>
%%   <tr><td>`too_many_hops'</td><td>483</td><td></td></tr>
%%   <tr><td>`ambiguous'</td><td>485</td><td></td></tr>
%%   <tr><td>`busy'</td><td>486</td><td></td></tr>
%%   <tr><td>`request_terminated'</td><td>487</td><td></td></tr>
%%   <tr><td>`{not_acceptable, Reason}'</td><td>488</td>
%%       <td>Generates a new `Warning' header using `Reason'</td></tr>
%%   <tr><td>`bad_event'</td><td>489</td><td></td></tr>
%%   <tr><td>`request_pending'</td><td>491</td><td></td></tr>
%%   <tr><td>`internal_error'</td><td>500</td><td></td></tr>
%%   <tr><td>`{internal_error, Text}'</td><td>500</td>
%%       <td>Text will be used in SIP first line</td></tr>
%%   <tr><td>`busy_eveywhere'</td><td>600</td><td></td></tr>
%%   <tr><td>`decline'</td><td>603</td><td></td></tr>
%%   <tr><td>`Code'</td><td>`Code'</td><td></td></tr>
%%   <tr><td>`{Code, [Header]}'</td><td>`Code'</td>
%%        <td>Will include headers in the response</td></tr>
%%   <tr><td>`{Code, [Header], Body}'</td><td>`Code'</td>
%%        <td>Will include headers and body in response</td></tr>
%%   <tr><td>`{Code, [Header], Body, Options}'</td><td>`Code'</td>
%%       <td>Will include headers and body in response, using options</td></tr>
%% </table> 
%% <br/>
%% With the following types:
%% <table border="1">
%%   <tr><th>Parameter</th><th>Type</th></tr>
%%   <tr><td>`Code'</td><td>{@link nksip:response_code()}</td></tr>
%%   <tr><td>`Header'</td><td>{@link nksip:header()}</td></tr>
%%   <tr><td>`Body'</td><td>{@link nksip:body()}</td></tr>
%%   <tr><td>`Options'</td><td>{@link nksip_lib:optslist()}</td></tr>
%%   <tr><td>`Text'</td><td>`binary()'</td></tr>
%%   <tr><td>`Realm'</td><td>`binary()'</td></tr>
%%   <tr><td>`Methods'</td><td>`binary()'</td></tr>
%%   <tr><td>`Types'</td><td>`binary()'</td></tr>
%%   <tr><td>`Extensions'</td><td>`binary()'</td></tr>
%%   <tr><td>`Min'</td><td>`binary()'</td></tr>
%% </table> 
%% <br/>
%% Some previous replies allow including options. The recognized options are:
%% <ul>
%%  <li>`allow': if present generates an <i>Allow</i> header.</li>
%%  <li>`supported': if present generates a <i>Supported</i> header.</li>
%%  <li>`accept': if present generates an <i>Accept</i> header.</li>
%%  <li>`date': if present generates a <i>Date</i> header.</li>
%%  <li>`contact': if present generates a <i>Contact</i> header.</li>
%%  <li>`{local_host, Host::binary()}': uses this Host instead of the default one for 
%%      <i>Contact</i>, <i>Record-Route</i>, etc.</li>
%%  <li>`{contact, [nksip:user_uri()]}': adds one or more `Contact' headers.</li>
%%  <li>`{reason_phrase, text()}': changes the default response line to `Text'.</li>
%%  <li>`{www_authenticate, Realm::from|binary()}': a <i>WWW-Authenticate</i>
%%       header will be generated for this `Realm' (see 
%%       {@link nksip_auth:make_response/2}).</li>
%%  <li>`{proxy_authenticate, Realm::from|binary()}': a <i>Proxy-Authenticate</i> will be 
%%       generated for this `Realm'.</li>
%%  <li>`{expires, non_neg_integer()': generates a <i>Event</i> header.</li>
%%  <li><code>{reason, {@link nksip:error_reason()}}</code>: 
%%       generates a <i>Reason</i> header.</li>
%%  <li><code>{service_route, {@link nksip:user_uri()}}</code>: 
%%       generates a <i>Service-Route</i> header, only for 2xx to REGISTER</li>
%% </ul>

-module(nksip_reply).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-include("nksip.hrl").
-include("nksip_call.hrl").

-export([reply/2, parse/1, warning/1]).

-export_type([sipreply/0]).



%% ===================================================================
%% Types
%% ===================================================================

-type sipreply() ::
    nksip:response_code() | 
    {nksip:response_code(), nksip_lib:optslist()} |
    ringing | rel_ringing | {rel_ringing, Body::nksip:body()} | 
    session_progress | rel_session_progress | {rel_session_progress, Body::nksip:body()} |
    ok | {ok, Opts::nksip_lib:optslist()} | 
    {answer, Body::nksip:body()} |
    accepted | 
    {redirect, Contact::nksip:user_uri()} | 
    {redirect_permanent, Contacts::nksip:user_uri()} | 
    {redirect_temporary, Contacts::nksip:user_uri()} |
    invalid_request | {invalid_request, Phrase::binary()|string()} | 
    authenticate | {authenticate, Realm::binary()} |
    forbidden | {forbidden, Phrase::binary()|string()} |
    not_found | {not_found, Phrase::binary()|string()} |
    {method_not_allowed, Allow::binary()|string()} |
    proxy_authenticate | {proxy_authenticate, Realm::binary()} |
    timeout | {timeout, Phrase::binary()|string()} | 
    conditional_request_failed |
    request_too_large |
    {unsupported_media_type, Accept::binary()|string()} | 
    {unsupported_media_encoding, AcceptEncoding::binary()|string()} |
    unsupported_uri_scheme | 
    {bad_extension, Unsupported::binary()|string()} |
    {extension_required, Require::binary()|string()} |
    {session_too_small, MinSE::integer()} |
    {interval_too_brief, Min::integer()} |
    flow_failed |
    first_hop_lacks_outbound |
    temporarily_unavailable |
    unknown_dialog | no_transaction |
    loop_detected | 
    too_many_hops |
    ambiguous |
    busy |
    request_terminated |
    {not_acceptable, Warning::binary()|string()} |
    bad_event |
    request_pending |
    internal_error | {internal_error, Phrase::binary()|string()} |
    service_unavailable |
    busy_eveywhere |
    decline.


%% ===================================================================
%% Public
%% ===================================================================

%% @doc Generates a new SIP response and send options using helper replies.
%% Currently recognized replies are described in this module.
%% See {@link nksip_uas_lib:response/5}.
-spec reply(nksip:request(), sipreply() | {nksip:response_code(), nksip_lib:optslist()}) ->
    {nksip:response(), nksip_lib:optslist()}.

reply(Req, {Code, Opts}) 
        when is_integer(Code), Code>=100, Code=<699, is_list(Opts)->
    ?call_debug("user reply to ~p: {~p, ~p}", [element(2, Req#sipmsg.class), Code, Opts]),
    case nksip_uas_lib:make(Req, Code, Opts) of
        {ok, Resp, RespOpts} ->
            {Resp, RespOpts};
        {error, Error} ->
            ?call_error("Error procesing response {~p, ~p}: ~p", [Code, Opts, Error]),
            nksip_uas_lib:make(Req, 500, [])
    end;
    
reply(Req, SipReply) -> 
    case parse(SipReply) of
        {Code, Opts0} ->
            Opts = post(Req, Code, Opts0);
        error -> 
            ?call_warning("Invalid sipreply ~p", [SipReply]),
            {Code, Opts} = {500, [{reason_phrase, <<"Invalid SipApp Response">>}]}
    end,
    reply(Req, {Code, Opts}).




%% ===================================================================
%% Private
%% ===================================================================

%% @private
-spec post(nksip:request(), nksip:response_code(), nksip_lib:optslist()) ->
    nksip_lib:optslist().

post(#sipmsg{class={req, Method}}=Req, Code, Opts) ->
    Opts1 = case Code>100 of
        true -> [timestamp|Opts];
        false -> Opts
    end,
    Opts2 = case 
        Code>100 andalso 
        (Method=='INVITE' orelse Method=='UPDATE' orelse Method=='SUBSCRIBE'
            orelse Method=='REFER')
    of
        true -> [allow, supported | Opts1];
        false -> Opts1
    end,
    Opts3 = case
        Code>100 andalso Code<300 andalso (Method=='INVITE' orelse Method=='NOTIFY')
    of
        true ->
            RR = nksip_sipmsg:header(Req, <<"record-route">>),
            [{replace, <<"record-route">>, RR}|Opts2];
        false ->
            Opts2
    end,
    Opts4 = case 
        Code>100 andalso Code<300 andalso 
        (Method=='INVITE' orelse Method=='UPDATE' orelse Method=='SUBSCRIBE'
            orelse Method=='REFER') andalso
        not lists:member(contact, Opts) andalso
        not lists:keymember(contact, 1, Opts)
    of
        true -> [contact|Opts3];
        false -> Opts3
    end,
    Opts5 = case Code>=200 andalso Code<300 andalso Method=='REGISTER' of
        true -> 
            Path = nksip_sipmsg:header(Req, <<"path">>),
            [{replace, <<"path">>, Path}|Opts4];
        false ->
            Opts4
    end,
    Opts6 = case Method=='SUBSCRIBE' orelse Method=='NOTIFY' orelse Method=='PUBLISH' of
        true -> [{event, Req#sipmsg.event}|Opts5];
        false -> Opts5
    end,
    Opts7 = case Code>=200 andalso Code<300 andalso Method=='SUBSCRIBE' of
        true ->
            Expires = nksip_lib:get_value(expires, Opts6, ?DEFAULT_EVENT_EXPIRES),
            Expires1 = min(Req#sipmsg.expires, Expires),
            [{expires, Expires1} | nksip_lib:delete(Opts6, expires)];
        false ->
            Opts6
    end,
    Opts7.


-spec parse(sipreply()) ->
    {nksip:response_code(), nksip_lib:optslist()} | error.

parse(Term) ->
    case Term of
        Code when is_integer(Code), Code>=100, Code=<699 ->
            {Code, []};
        {Code, Opts} when is_integer(Code), Code>=100, Code=<699, is_list(Opts) ->
            {Code, Opts};
        ringing -> 
            {180, []};
        rel_ringing -> 
            {180, [do100rel]};
        {rel_ringing, Body} -> 
            {180, [{body, Body}, do100rel]};
        session_progress -> 
            {183, []};
        rel_session_progress -> 
            {183, [do100rel]};
        {rel_session_progress, Body} -> 
            {183, [{body, Body}, do100rel]};
        ok -> 
            {200, []};
        {ok, Opts} when is_list(Opts) -> 
            {200, Opts};
        {answer, Body} -> 
            {200, [{body, Body}]};
        accepted ->
            {202, []};
        {redirect, Contacts} ->
            case nksip_parse:uris(Contacts) of
                error -> error;
                PContacts -> {300, [{contact, PContacts}]}
            end;
        {redirect_permanent, Contact} ->
            case nksip_parse:uris(Contact) of
                [PContact] -> {301, [{contact, PContact}]};
                _ -> error
            end;
        {redirect_temporary, Contact} ->
            case nksip_parse:uris(Contact) of
                [PContact] -> {302, [{contact, PContact}]};
                _ -> error
            end;
        invalid_request ->
            {400, [date]};
        {invalid_request, Phrase} when is_binary(Phrase); is_list(Phrase) ->
            {400, [{reason_phrase, Phrase}, date]};
        authenticate ->
            {401, [www_authenticate]};
        {authenticate, Realm} when is_binary(Realm) ->
            {401, [{www_authenticate, Realm}]};
        forbidden ->
            {403, []};
        {forbidden, Phrase} when is_binary(Phrase); is_list(Phrase) ->
            {403, [{reason_phrase, Phrase}]};
        not_found-> 
            {404, []};
        {not_found, Phrase} when is_binary(Phrase); is_list(Phrase) ->
            {404, [{reason_phrase, Phrase}]};
        {method_not_allowed, Allow} when is_binary(Allow); is_list(Allow) ->
            {405, [{replace, <<"allow">>, Allow}]};
        proxy_authenticate ->
            {407, [proxy_authenticate, from]};
        {proxy_authenticate, Realm} ->
            {407, [{proxy_authenticate, Realm}]};
        timeout ->
            {408, []};
        {timeout, Phrase} when is_binary(Phrase); is_list(Phrase) ->
            {408, [{reason_phrase, Phrase}]};
        conditional_request_failed ->
            {412, []};
        request_too_large ->
            {413, []};
        {unsupported_media_type, Accept} when is_binary(Accept); is_list(Accept) ->
            {415, [{accept, Accept}]};
        {unsupported_media_encoding, Encod} when is_binary(Encod); is_list(Encod) ->
            {415, [{replace, <<"accept-encoding">>, Encod}]};
        unsupported_uri_scheme-> 
            {416, []};
        {bad_extension, Unsupp} when is_binary(Unsupp); is_list(Unsupp) ->
            {420, [{replace, <<"unsupported">>, Unsupp}]};
        {extension_required, Require} when is_binary(Require); is_list(Require) ->
            {421, [{require, Require}]};
        {session_too_small, MinSE} when is_integer(MinSE), MinSE>0 ->
            {422, [{add, <<"min-se">>, MinSE}]};
        {interval_too_brief, Min} when is_integer(Min), Min>0 ->
            {423, [{replace, <<"min-expires">>, Min}]};
        flow_failed ->
            {430, []};
        first_hop_lacks_outbound ->
            {439, []};
        temporarily_unavailable ->
            {480, []};
        unknown_dialog ->
            {481, []};
        no_transaction ->
            {481, []};
        loop_detected ->
            {482, []};
        too_many_hops-> 
            {483, []};
        ambiguous ->
            {485, []};
        busy ->
            {486, []};
        request_terminated ->
            {487, []};
        {not_acceptable, Warning} when is_binary(Warning); is_list(Warning) ->
            {488, [{replace, <<"warning">>, Warning}]};
        bad_event ->
            {489, []};
        request_pending ->
            {491, []};
        internal_error ->
            {500, []};
        {internal_error, Phrase} when is_binary(Phrase); is_list(Phrase) ->
            {500, [{reason_phrase, Phrase}, date]};
        service_unavailable ->
            {503, []};
        {service_unavailable, Phrase} when is_binary(Phrase); is_list(Phrase) ->
            {503, [{reason_phrase, Phrase}, date]};
        busy_eveywhere ->
            {600, []};
        decline ->
            {603, []};
        _ ->
            error
    end.


%% @doc Generates a Warning
-spec warning(atom()) -> 
    integer().

warning(Warn) ->
    case Warn of
        incompatible_network_protocol -> 300;
        incompatible_network_address_formats -> 301;
        incompatible_transport_protocol -> 302;
        incompatible_bandwidth_units -> 303;
        media_type_not_available -> 304;
        incompatible_media_format -> 305;
        attribute_not_understood -> 306;
        session_description_parameter_not_understood -> 307;
        multicast_not_available -> 330;
        unicast_not_available -> 331;
        insufficient_bandwidth -> 370;
        sips_not_allowed -> 380;
        sips_required -> 381;
        _ -> 399
    end.




