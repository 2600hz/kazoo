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
%% See documentation in Github page for options description
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
    nksip:sip_code() | 
    {nksip:sip_code(), nksip:optslist()} |
    ringing | rel_ringing | {rel_ringing, Body::nksip:body()} | 
    session_progress | rel_session_progress | {rel_session_progress, Body::nksip:body()} |
    ok | {ok, Opts::nksip:optslist()} | 
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
%% See {@link nksip_call_uas_make:response/5}.
-spec reply(nksip:request(), sipreply() | {nksip:sip_code(), nksip:optslist()}) ->
    {nksip:response(), nksip:optslist()}.

reply(Req, {Code, Opts}) 
        when is_integer(Code), Code>=100, Code=<699, is_list(Opts)->
    ?call_debug("user reply to ~p: {~p, ~p}", [element(2, Req#sipmsg.class), Code, Opts]),
    case nksip_call_uas_make:make(Req, Code, Opts) of
        {ok, Resp, RespOpts} ->
            {Resp, RespOpts};
        {error, Error} ->
            ?call_error("Error procesing response {~p, ~p}: ~p", [Code, Opts, Error]),
            nksip_call_uas_make:make(Req, 500, [])
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
-spec post(nksip:request(), nksip:sip_code(), nksip:optslist()) ->
    nksip:optslist().

post(#sipmsg{app_id=AppId, class={req, Method}}=Req, Code, Opts) ->
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
            RR = nksip_sipmsg:header(<<"record-route">>, Req),
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
            Path = nksip_sipmsg:header(<<"path">>, Req),
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
            Expires = case nksip_lib:get_value(expires, Opts6) of
                undefined -> nksip_sipapp_srv:config(AppId, event_expires);
                Expires0 -> Expires0
            end,
            Expires1 = min(Req#sipmsg.expires, Expires),
            [{expires, Expires1} | nksip_lib:delete(Opts6, expires)];
        false ->
            Opts6
    end,
    Opts7.


-spec parse(sipreply()) ->
    {nksip:sip_code(), nksip:optslist()} | error.

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




