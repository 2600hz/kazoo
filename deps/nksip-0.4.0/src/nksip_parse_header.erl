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

%% @private SIP message parsing functions
%%
%% This module implements several functions to parse sip requests, responses
%% headers, uris, vias, etc.

-module(nksip_parse_header).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-include("nksip.hrl").
-include("nksip_call.hrl").

-export([parse/2, parse/4]).
-export([headers/3, header/2, name/1]).

-define(TWO32, 4294967296).


%% ===================================================================
%% Private
%% ===================================================================


%% @doc Parses a header value. 
%% If Name is binary(), it will is supposed it is canonical; if not sure, 
%% call `name(Name)'. If it is a string() or atom(), it is  converted to canonical form.
%% Throws {invalid, Name} in case of invalid header.
-spec parse(binary(), term()) ->
    {binary(), term()}.

parse(Name, Value) when is_binary(Name) ->
    try
        {Result, _} = header(Name, Value),
        {Name, Result}
    catch
        throw:invalid -> throw({invalid, Name})
    end.


%% @doc Parses a header value. 
%% Similar to `parse/2', but updates the #sipmsg{}.
-spec parse(binary(), term(), #sipmsg{}, pre|replace|post) ->
    {binary(), term()} | #sipmsg{}.

parse(Name, Value, #sipmsg{}=Req, Policy) when is_binary(Name)->
    try
        case header(Name, Value) of
            {Result, Pos} when is_integer(Pos) -> 
                Result1 = case Name of
                    <<"from">> -> update_tag(Result, Req#sipmsg.from);
                    <<"to">> -> update_tag(Result, Req#sipmsg.to);
                    _ -> Result
                end,
                setelement(Pos, Req, Result1);
            {Result, {add, Pos}} ->
                Old = element(Pos, Req),
                Value1 = case Policy of
                    pre when is_list(Old), is_list(Result) -> Result++Old;
                    post when is_list(Old), is_list(Result) -> Old++Result;
                    replace when Value == <<>>; Value== [] -> [];
                    replace -> Result;  
                    _ -> throw(invalid)
                end,
                setelement(Pos, Req, Value1);
            {Result, add} ->
                Old = Req#sipmsg.headers,
                Headers = case Policy of
                    pre -> [{Name, Result}|Old]; 
                    post -> Old++[{Name, Result}];
                    replace when Value == <<>>; Value==[] -> nksip_lib:delete(Old, Name);
                    replace -> [{Name, Result}|nksip_lib:delete(Old, Name)]
                end,
                Req#sipmsg{headers=Headers}
        end
    catch
        throw:invalid -> throw({invalid, Name})
    end.


%% @private
-spec headers([{binary(), term()}], nksip:request(), pre|post|replace) ->
    nksip:request().

headers([], Req, _Policy) ->
    Req;

headers([{<<"body">>, Value}|Rest], Req, Policy) ->
    Body1 = list_to_binary(http_uri:decode(nksip_lib:to_list(Value))), 
    headers(Rest, Req#sipmsg{body=Body1}, Policy);

headers([{Name, Value}|Rest], Req, Policy) ->
    Value1 = http_uri:decode(nksip_lib:to_list(Value)), 
    Req1 = parse(Name, Value1, Req, Policy),
    headers(Rest, Req1, Policy);

headers(_, _, _) ->
    throw({invalid, ruri}).



%% @private
header(<<"from">>, Value) -> 
    From = single_uri(Value),
    FromTag = nksip_lib:get_value(<<"tag">>, From#uri.ext_opts, <<>>),
    {{From, FromTag}, #sipmsg.from};

header(<<"to">>, Value) -> 
    To = single_uri(Value),
    ToTag = nksip_lib:get_value(<<"tag">>, To#uri.ext_opts, <<>>),
    {{To, ToTag}, #sipmsg.to};

header(<<"via">>, Value) -> 
    {vias(Value), #sipmsg.vias};

header(<<"cseq">>, Value) -> 
    {cseq(Value), #sipmsg.cseq};

header(<<"max-forwards">>, Value) -> 
    {integer(Value, 0, 300), #sipmsg.forwards};

header(<<"call-id">>, Value) -> 
    case nksip_lib:to_binary(Value) of
        <<>> -> throw(invalid);
        CallId -> {CallId, #sipmsg.call_id}
    end;

header(<<"route">>, Value) ->
    {uris(Value), {add, #sipmsg.routes}};

header(<<"contact">>, Value) ->
    {uris(Value), #sipmsg.contacts};

header(<<"record-route">>, Value) ->
    {uris(Value), add};

header(<<"path">>, Value) ->
    {uris(Value), add};

header(<<"content-length">>, Value) ->
    {integer(Value, 0, ?TWO32), none};

header(<<"expires">>, Value) ->
    {integer(Value, 0, ?TWO32), #sipmsg.expires};

header(<<"content-type">>, Value) ->
    {single_token(Value), #sipmsg.content_type};
    
header(<<"require">>, Value) ->
    {names(Value), #sipmsg.require};

header(<<"supported">>, Value) ->
    {names(Value), #sipmsg.supported};

header(<<"event">>, Value) ->
    {single_token(Value), #sipmsg.event};

header(<<"reason">>, Value) ->
    case is_binary(Value) of
        true -> 
            Value;
        false ->
            case nksip_unparse:error_reason(Value) of
                error -> throw(invalid);
                Bin -> {Bin, add}
            end
    end;

header(_Name, Value) ->
    {Value, add}.




%% Parsers


single_uri(Data) ->
    case nksip_parse:uris(Data) of
        [#uri{} = Uri] -> Uri;
        _ -> throw(invalid)
    end.

uris(Data) ->
    case nksip_parse:uris(Data) of
        error -> throw(invalid);
        Uris -> Uris
    end.

vias(Data) ->
    case nksip_parse:vias(Data) of
        [_|_] = Vias -> Vias;
        _ -> throw(invalid)
    end.

single_token(Data) ->
    case nksip_parse:tokens(Data) of
        [Token] -> Token;
        _ -> throw(invalid)
    end.

% tokens(Data) ->
%     case nksip_parse:tokens(Data) of
%         error -> throw(invalid);
%         Tokens -> Tokens
%     end.

names(Data) ->
    case nksip_parse:tokens(Data) of
        error -> throw(invalid);
        Tokens -> [Token || {Token, _} <- Tokens]
    end.

cseq(Data) ->
    case nksip_lib:tokens(Data) of
        [CSeq, Method] ->                
            case nksip_lib:to_integer(CSeq) of
                Int when is_integer(Int), Int>=0, Int<4294967296 ->
                    {Int, nksip_parse:method(Method)};
                _ ->
                    throw(invalid)
            end;
        _ -> 
            throw(invalid)
    end.

integer(Data, Min, Max) ->
    case nksip_lib:to_integer(Data) of
        Int when is_integer(Int), Int>=Min, Int=<Max -> Int;
        _ -> throw(invalid)
    end.


%% @private
update_tag({Value, <<>>}, {_, <<>>}) -> 
    {Value, <<>>};

update_tag({#uri{ext_opts=ExtOpts}=Value, <<>>}, {_, Tag}) ->
    ExtOpts1 = nksip_lib:store_value(<<"tag">>, Tag, ExtOpts),
    {Value#uri{ext_opts=ExtOpts1}, Tag};

update_tag({Value, Tag}, _) ->
    {Value, Tag}.


%% @private
-spec name(atom()|list()|binary()) ->
    binary().

name(Name) when is_binary(Name) ->
    << 
        << (case Ch>=$A andalso Ch=<$Z of true -> Ch+32; false -> Ch end) >> 
        || << Ch >> <= Name 
    >>;

name(Name) when is_atom(Name) ->
    List = [
        case Ch of 
            $_ -> $-; 
            _ when Ch>=$A, Ch=<$Z -> Ch+32;
            _ -> Ch 
        end 
        || Ch <- atom_to_list(Name)
    ],
    list_to_binary(List);

name(Name) when is_list(Name) ->
    name(list_to_binary(Name)).





% %% @private
% raw_name(Name) ->
%     case Name of
%         "a" -> <<"Accept-Contact">>;
%         "b" -> <<"Referred-By">>;
%         "c" -> <<"Content-Type">>;
%         "d" -> <<"Request-Disposition">>;
%         "e" -> <<"Content-Encoding">>;
%         "f" -> <<"From">>;
%         "i" -> <<"Call-ID">>;
%         "j" -> <<"Reject-Contact">>;
%         "k" -> <<"Supported">>;
%         "l" -> <<"Content-Length">>;
%         "m" -> <<"Contact">>;
%         "n" -> <<"Identity-Info">>;
%         "o" -> <<"Event">>;
%         "r" -> <<"Refer-To">>;
%         "s" -> <<"Subject">>;
%         "t" -> <<"To">>;
%         "u" -> <<"Allow-Events">>;
%         "v" -> <<"Via">>;
%         "x" -> <<"Session-Expires">>;
%         "y" -> <<"Identity">>;

%         "x-"++_ -> unknown;

%         "accept" -> <<"Accept">>;
%         "allow" -> <<"Allow">>;
%         "allow-events" -> <<"Allow-Events">>;
%         "authorization" -> <<"Authorization">>;
%         "call-id" -> <<"Call-ID">>;
%         "contact" -> <<"Contact">>;
%         "content-length" -> <<"Content-Length">>;
%         "content-type" -> <<"Content-Type">>;
%         "cseq" -> <<"CSeq">>;
%         "event" -> <<"Event">>;
%         "expires" -> <<"Expires">>;
%         "from" -> <<"From">>;
%         "max-forwards" -> <<"Max-Forwards">>;
%         "path" -> <<"Path">>;
%         "proxy-authenticate" -> <<"Proxy-Authenticate">>;
%         "proxy-authorization" -> <<"Proxy-Authorization">>;
%         "rack" -> <<"RAck">>;
%         "record-route" -> <<"Record-Route">>;
%         "require" -> <<"Require">>;
%         "route" -> <<"Route">>;
%         "rseq" -> <<"RSeq">>;
%         "session-expires" -> <<"Session-Expires">>;
%         "subscription-state" -> <<"Subscription-State">>;
%         "supported" -> <<"Supported">>;
%         "to" -> <<"To">>;
%         "user-agent" -> <<"User-Agent">>;
%         "via" -> <<"Via">>;
%         "www-authenticate" -> <<"WWW-Authenticate">>;

%         "accept-contact" -> <<"Accept-Contact">>;
%         "accept-encoding" -> <<"Accept-Encoding">>;
%         "accept-language" -> <<"Accept-Language">>;
%         "accept-resource-priority" -> <<"Accept-Resource-Priority">>;
%         "alert-info" -> <<"Alert-Info">>;
%         "answer-mode" -> <<"Answer-Mode">>;
%         "authentication-info" -> <<"Authentication-Info">>;
%         "call-info" ->  <<"Call-Info">>;
%         "content-disposition" -> <<"Content-Disposition">>;
%         "content-encoding" -> <<"Content-Encoding">>;
%         "date" -> <<"Date">>;
%         "encryption" -> <<"Encryption">>;
%         "error-info" -> <<"Error-Info">>;
%         "feature-caps" -> <<"Feature-Caps">>;
%         "flow-timer" -> <<"Flow-Timer">>;
%         "geolocation" -> <<"Geolocation">>;
%         "geolocation-error" -> <<"Geolocation-Error">>;
%         "geolocation-routing" -> <<"Geolocation-Routing">>;
%         "hide" -> <<"Hide">>;
%         "history-info" -> <<"History-Info">>;
%         "identity" -> <<"Identity">>;
%         "identity-info" -> <<"Identity-Info">>;
%         "info-package" -> <<"Info-Package">>;
%         "in-reply-to" -> <<"In-Reply-To">>;
%         "join" -> <<"Join">>;
%         "max-breadth" -> <<"Max-Breadth">>;
%         "mime-version" -> <<"MIME-Version">>;
%         "min-expires" -> <<"Min-Expires">>;
%         "min-se" -> <<"Min-SE">>;
%         "organization" -> <<"Organization">>;
%         "permission-missing" -> <<"Permission-Missing">>;
%         "policy-contact" -> <<"Policy-Contact">>;
%         "policy-id" -> <<"Policy-ID">>;
%         "priority" -> <<"Priority">>;
%         "proxy-require" -> <<"Proxy-Require">>;
%         "reason" -> <<"Reason">>;
%         "reason-phrase" -> <<"Reason-Phrase">>;
%         "recv-info" -> <<"Recv-Info">>;
%         "refer-sub" -> <<"Refer-Sub">>;
%         "refer-to" -> <<"Refer-To">>;
%         "referred-by" -> <<"Referred-By">>;
%         "reject-contact" -> <<"Reject-Contact">>;
%         "replaces" -> <<"Replaces">>;
%         "reply-to" -> <<"Reply-To">>;
%         "request-disposition" -> <<"Request-Disposition">>;
%         "resource-priority" -> <<"Resource-Priority">>;
%         "response-key" -> <<"Response-Key">>;
%         "retry-after" -> <<"Retry-After">>;
%         "security-client" -> <<"Security-Client">>;
%         "security-server" -> <<"Security-Server">>;
%         "security-verify" -> <<"Security-Verify">>;
%         "server" -> <<"Server">>;
%         "service-route" -> <<"Service-Route">>;
%         "sip-etag" -> <<"SIP-ETag">>;
%         "sip-if-match" -> <<"SIP-If-Match">>;
%         "subject" -> <<"Subject">>;
%         "timestamp" -> <<"Timestamp">>;
%         "trigger-consent" -> <<"Trigger-Consent">>;
%         "unsupported" -> <<"Unsupported">>;
%         "warning" -> <<"Warning">>;

%         "p-access-network-info" -> <<"P-Access-Network-Info">>;
%         "p-answer-state" -> <<"P-Answer-State">>;
%         "p-asserted-identity" -> <<"P-Asserted-Identity">>;
%         "p-asserted-service" -> <<"P-Asserted-Service">>;
%         "p-associated-uri" -> <<"P-Associated-URI">>;
%         "p-called-party-id" -> <<"P-Called-Party-ID">>;
%         "p-charging-function-addresses" -> <<"P-Charging-Function-Addresses">>;
%         "p-charging-vector" -> <<"P-Charging-Vector">>;
%         "p-dcs-trace-party-id" -> <<"P-DCS-Trace-Party-ID">>;
%         "p-dcs-osps" -> <<"P-DCS-OSPS">>;
%         "p-dcs-billing-info" -> <<"P-DCS-Billing-Info">>;
%         "p-dcs-laes" -> <<"P-DCS-LAES">>;
%         "p-dcs-redirect" -> <<"P-DCS-Redirect">>;
%         "p-early-media" -> <<"P-Early-Media">>;
%         "p-media-authorization" -> <<"P-Media-Authorization">>;
%         "p-preferred-identity" -> <<"P-Preferred-Identity">>;
%         "p-preferred-service" -> <<"P-Preferred-Service">>;
%         "p-profile-key" -> <<"P-Profile-Key">>;
%         "p-refused-uri-list" -> <<"P-Refused-URI-List">>;
%         "p-served-user" -> <<"P-Served-User">>;
%         "p-user-database" -> <<"P-User-Database">>;
%         "p-visited-network-id" -> <<"P-Visited-Network-ID">>;

%         _ -> unknown
%     end.


%% ===================================================================
%% EUnit tests
%% ===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

uri_test() ->
    Uri = 
        "<sip:host?FROM=sip:u1%40from&to=sip:to&contact=sip:a"
        "&user1=data1&call-ID=abc&user-Agent=user"
        "&content-type = application/sdp"
        "&Require=a;b;c&supported=d;e & expires=5"
        "&cseq=100%20INVITE&max-forwards=69"
        "&route=sip%3Ar1%2Csip%3Ar2"
        "&body=my%20body&user1=data2"
        "&Route=sip%3Ar3>",
    [#uri{headers=UriHeaders}] = nksip_parse:uris(Uri),

    Base = #sipmsg{
        from = {#uri{domain = <<"f">>, ext_opts=[{<<"tag">>, <<"f">>}]}, <<"f">>},
        to = {#uri{domain = <<"t">>, ext_opts=[{<<"tag">>, <<"t">>}]}, <<"t">>},
        headers = [{<<"previous">>, <<"term">>}],
        routes = [#uri{domain = <<"previous">>}]
    },

    Req1 = headers(UriHeaders, Base, post),
    #sipmsg{
        vias = [],
        from = {#uri{disp = <<>>, scheme = sip, user = <<"u1">>, domain = <<"from">>, 
                    ext_opts = [{<<"tag">>, <<"f">>}]}, <<"f">>},
        to = {#uri{domain = <<"to">>, ext_opts = [{<<"tag">>, <<"t">>}]}, <<"t">>},
        call_id = <<"abc">>,
        cseq = {100,'INVITE'},
        forwards = 69,
        routes = [
            #uri{domain = <<"previous">>},
            #uri{domain = <<"r1">>},
            #uri{domain = <<"r2">>},
            #uri{domain = <<"r3">>}
        ],
        contacts = [
            #uri{domain = <<"a">>}
        ],
        content_type = {<<"application/sdp">>,[]},
        require = [<<"a">>],
        supported = [<<"d">>],
        expires = 5,
        headers = [
            {<<"previous">>, <<"term">>},
            {<<"user1">>, "data1"},
            {<<"user-agent">>, "user"},
            {<<"user1">>, "data2"}
        ],
        body = <<"my body">>
    } = Req1,

    Req2 = headers(UriHeaders, Base, pre),
    #sipmsg{
        routes = [
            #uri{domain = <<"r3">>},
            #uri{domain = <<"r1">>},
            #uri{domain = <<"r2">>},
            #uri{domain = <<"previous">>}
        ],
        headers = [
            {<<"user1">>, "data2"},
            {<<"user-agent">>, "user"},
            {<<"user1">>, "data1"},
            {<<"previous">>, <<"term">>}
        ]
    } = Req2,

    Req3 = headers(UriHeaders, Base, replace),
    #sipmsg{
        routes = [#uri{domain = <<"r3">>}],
        headers = [
            {<<"user1">>, "data2"},
            {<<"user-agent">>, "user"},
            {<<"previous">>, <<"term">>}
        ]
    } = Req3,
    ok.
       
-endif.
