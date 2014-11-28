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

%% @doc SIP message parsing functions
%%
%% This module implements several functions to parse sip requests, responses
%% headers, uris, vias, etc.

-module(nksip_parse).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-include("nksip.hrl").
-include("nksip_call.hrl").

-export([method/1, scheme/1, aors/1, uris/1, ruris/1, vias/1]).
-export([tokens/1, integers/1, dates/1]).
-export([uri_method/2]).
-export([transport/1]).
-export([packet/4, packet/3]).

-export_type([msg_class/0]).

-type msg_class() :: {req, nksip:method(), binary()} | 
                     {resp, nksip:sip_code(), binary()}.



%% ===================================================================
%% Public
%% ===================================================================

%% @doc Parses any `term()' into a valid `nksip:method()'. If recognized it will be an
%% `atom', or a `binary' if not.
-spec method(binary() | atom() | string()) -> 
    nksip:method() | binary().

method(Method) when is_atom(Method) ->
    Method;
method(Method) when is_list(Method) ->
    method(list_to_binary(Method));
method(Method) when is_binary(Method) ->
    case Method of
        <<"INVITE">> -> 'INVITE';
        <<"REGISTER">> -> 'REGISTER';
        <<"BYE">> -> 'BYE';
        <<"ACK">> -> 'ACK';
        <<"CANCEL">> -> 'CANCEL';
        <<"OPTIONS">> -> 'OPTIONS';
        <<"SUBSCRIBE">> -> 'SUBSCRIBE';
        <<"NOTIFY">> -> 'NOTIFY';
        <<"PUBLISH">> -> 'PUBLISH';
        <<"REFER">> -> 'REFER';
        <<"MESSAGE">> -> 'MESSAGE';
        <<"INFO">> -> 'INFO';
        <<"PRACK">> -> 'PRACK';
        <<"UPDATE">> -> 'UPDATE';
        _ -> Method 
    end.


%% @doc Parses all AORs found in `Term'.
-spec aors(Term :: nksip:user_uri() | [nksip:user_uri()]) -> 
    [nksip:aor()].
                
aors(Term) ->
    [{Scheme, User, Domain} || 
     #uri{scheme=Scheme, user=User, domain=Domain} <- uris(Term)].


%% @doc Parses all URIs found in `Term'.
-spec uris(Term :: nksip:user_uri() | [nksip:user_uri()]) -> 
    [nksip:uri()] | error.
                
uris(#uri{}=Uri) -> [Uri];
uris([#uri{}=Uri]) -> [Uri];
uris([]) -> [];
uris([First|_]=String) when is_integer(First) -> uris([String]);    % It's a string
uris(List) when is_list(List) -> parse_uris(List, []);
uris(Term) -> uris([Term]).


%% @doc Parses all URIs found in `Term'.
-spec ruris(Term :: nksip:user_uri() | [nksip:user_uri()]) -> 
    [nksip:uri()] | error.
                
ruris(RUris) -> 
    case uris(RUris) of
        error -> error;
        Uris -> parse_ruris(Uris, [])
    end.
          

%% @doc Extracts all `via()' found in `Term'
-spec vias(Term :: binary() | string() | [binary() | string()]) -> 
    [nksip:via()] | error.

vias([]) -> [];
vias([First|_]=String) when is_integer(First) -> vias([String]);    % It's a string
vias(List) when is_list(List) -> parse_vias(List, []);
vias(Term) -> vias([Term]).


%% @doc Gets a list of `tokens()' from `Term'
-spec tokens(Term :: binary() | string() | [binary() | string()]) -> 
    [nksip:token()] | error.

tokens([]) -> [];
tokens([First|_]=String) when is_integer(First) -> tokens([String]);  
tokens(List) when is_list(List) -> parse_tokens(List, []);
tokens(Term) -> tokens([Term]).


%% @doc Gets a list of `integer()' from `Term'
-spec integers(Term :: binary() | string() | [binary() | string()]) -> 
    [integer()] | error.

integers([]) -> [];
integers([First|_]=String) when is_integer(First) -> integers([String]);  
integers(List) when is_list(List) -> parse_integers(List, []);
integers(Term) -> integers([Term]).


%% @doc Gets a list of `calendar:datetime()' from `Term'
-spec dates(Term :: binary() | string() | [binary() | string()]) -> 
    [calendar:datetime()] | error.

dates([]) -> [];
dates([First|_]=String) when is_integer(First) -> dates([String]);  
dates(List) when is_list(List) -> parse_dates(List, []);
dates(Term) -> dates([Term]).


%% @private Gets the scheme, host and port from an `nksip:uri()' or `via()'
-spec transport(nksip:uri()|nksip:via()) -> 
    {Proto::nksip:protocol(), Host::binary(), Port::inet:port_number()}.

transport(#uri{scheme=Scheme, domain=Host, port=Port, opts=Opts}) ->
    Proto1 = case nksip_lib:get_value(<<"transport">>, Opts) of
        Atom when is_atom(Atom) -> 
            Atom;
        Other ->
            LcTransp = string:to_lower(nksip_lib:to_list(Other)),
            case catch list_to_existing_atom(LcTransp) of
                {'EXIT', _} -> nksip_lib:to_binary(Other);
                Atom -> Atom
            end
    end,
    Proto2 = case Proto1 of
        undefined when Scheme==sips -> tls;
        undefined -> udp;
        Other2 -> Other2
    end,
    Port1 = case Port > 0 of
        true -> Port;
        _ -> nksip_transport:default_port(Proto2)
    end,
    {Proto2, Host, Port1};

transport(#via{proto=Proto, domain=Host, port=Port}) ->
    Port1 = case Port > 0 of
        true -> Port;
        _ -> nksip_transport:default_port(Proto)
    end,
    {Proto, Host, Port1}.

% subscription_state(ST) ->
    


%     % Event options
%         {subscription_state, ST} when is_tuple(ST) ->
%             case catch nksip_unparse:token(ST) of
%                 Bin when is_binary(Bin) ->
%                     {replace, <<"subscription-state">>, Bin};
%                 _ ->
%                     throw({invalid, subscription_state})
%             end;





%% ===================================================================
%% Internal
%% ===================================================================

%% @private First-stage SIP message parser
%% 50K/sec on i7
-spec packet(nksip:app_id(), nksip:call_id(), nksip_transport:transport(), binary()) ->
    {ok, #sipmsg{}} | {error, term()} | {reply_error, term(), binary()}.

packet(AppId, CallId, Transp, Packet) ->
    Start = nksip_lib:l_timestamp(),
    case nksip_parse_sipmsg:parse(Packet) of
        {ok, Class, Headers, Body} ->
            try 
                MsgClass = case Class of
                    {req, Method, RUri} ->
                        case uris(RUri) of
                            [RUri1] -> 
                                [RUri1];
                            _ -> RUri1 = 
                                throw({invalid, <<"Request-URI">>})
                        end,
                        {req, Method};
                    {resp, Code, Reason} ->
                        case catch list_to_integer(Code) of
                            Code1 when is_integer(Code1), Code1>=100, Code1<700 -> 
                                ok;
                            _ -> 
                                Code1 = throw({invalid, <<"Code">>})
                        end,
                        RUri1 = undefined,
                        {resp, Code1, Reason}
                end,
                Req0 = #sipmsg{
                    id = nksip_lib:uid(),
                    class = MsgClass,
                    app_id = AppId,
                    ruri = RUri1,
                    call_id = CallId,
                    body = Body,
                    transport = Transp,
                    start = Start
                },
                {ok, parse_sipmsg(Req0, Headers)}
            catch
                throw:{invalid, InvHeader} ->
                    case Class of
                        {req, _, _} ->
                            Msg = <<"Invalid ", InvHeader/binary>>,
                            Resp = nksip_unparse:response(Headers, 400, Msg),
                            {reply_error, {invalid, InvHeader}, Resp};
                        _ ->
                            {error, {invalid, InvHeader}}
                    end
            end;
        error ->
            {error, invalid_message}
    end.



%% @private First-stage SIP message parser
%% 50K/sec on i7
-spec packet(nksip:app_id(), nksip_transport:transport(), binary()) ->
    {ok, #sipmsg{}, binary()} | partial | {error, term()} |
    {reply_error, term(), binary()}.

packet(AppId, #transport{proto=Proto}=Transp, Packet) ->
    Start = nksip_lib:l_timestamp(),
    case nksip_parse_sipmsg:parse(Proto, Packet) of
        {ok, Class, Headers, Body, Rest} ->
            try 
                CallId = case nksip_lib:get_value(<<"call-id">>, Headers) of
                    CallId0 when byte_size(CallId0) > 0 -> CallId0;
                    _ -> throw({invalid, <<"Call-ID">>})
                end,
                MsgClass = case Class of
                    {req, Method, RUri} ->
                        case uris(RUri) of
                            [RUri1] -> [RUri1];
                            _ -> RUri1 = throw({invalid, <<"Request-URI">>})
                        end,
                        {req, Method};
                    {resp, Code, Reason} ->
                        case catch list_to_integer(Code) of
                            Code1 when is_integer(Code1), Code1>=100, Code1<700 -> ok;
                            _ -> Code1 = throw({invalid, <<"Code">>})
                        end,
                        RUri1 = undefined,
                        {resp, Code1, Reason}
                end,
                Req0 = #sipmsg{
                    id = nksip_lib:uid(),
                    class = MsgClass,
                    app_id = AppId,
                    ruri = RUri1,
                    call_id = CallId,
                    body = Body,
                    transport = Transp,
                    start = Start
                },
                {ok, parse_sipmsg(Req0, Headers), Rest}
            catch
                throw:{invalid, InvHeader} ->
                    case Class of
                        {req, _, _} ->
                            Msg = <<"Invalid ", InvHeader/binary>>,
                            Resp = nksip_unparse:response(Headers, 400, Msg),
                            {reply_error, {invalid, InvHeader}, Resp};
                        _ ->
                            {error, {invalid, InvHeader}}
                    end
            end;
        partial ->
            partial;
        error ->
            {error, invalid_message};
        {reply, {req, _, _}, Headers, InvHeader} ->
            Msg = <<"Invalid ", InvHeader/binary>>,
            Resp = nksip_unparse:response(Headers, 400, Msg),
            {reply_error, {invalid, InvHeader}, Resp};
        {reply, _, _, InvHeader} ->
            {error, {invalid, InvHeader}}
    end.
  

%% @private
-spec parse_sipmsg(#sipmsg{}, [nksip:header()]) -> 
    #sipmsg{}.

parse_sipmsg(SipMsg, Headers) ->
    From = case uris(proplists:get_all_values(<<"from">>, Headers)) of
        [From0] -> From0;
        _ -> throw({invalid, <<"From">>})
    end,
    FromTag = nksip_lib:get_value(<<"tag">>, From#uri.ext_opts, <<>>),
    To = case uris(proplists:get_all_values(<<"to">>, Headers)) of
        [To0] -> To0;
        _ -> throw({invalid, <<"To">>})
    end,
    ToTag = nksip_lib:get_value(<<"tag">>, To#uri.ext_opts, <<>>),
    Vias = case vias(proplists:get_all_values(<<"via">>, Headers)) of
        [] -> throw({invalid, <<"via">>});
        error -> throw({invalid, <<"Via">>});
        Vias0 -> Vias0
    end,
    CSeq = case proplists:get_all_values(<<"cseq">>, Headers) of
        [CSeq0] -> 
            case nksip_lib:tokens(CSeq0) of
                [CSeqNum, CSeqMethod] -> 
                    CSeqMethod1 = nksip_parse:method(CSeqMethod),
                    case SipMsg#sipmsg.class of
                        {req, CSeqMethod1} -> ok;
                        {req, _} -> throw({invalid, <<"CSeq">>});
                        {resp, _, _} -> ok
                    end,
                    case nksip_lib:to_integer(CSeqNum) of
                        CSeqInt 
                            when is_integer(CSeqInt), CSeqInt>=0, CSeqInt<4294967296 ->
                            {CSeqInt, CSeqMethod1};
                        _ ->
                            throw({invalid, <<"CSeq">>})
                    end;
                _ ->
                    throw({invalid, <<"CSeq">>})
            end;
        _ -> 
            throw({invalid, <<"CSeq">>})
    end,
    Forwards = case integers(proplists:get_all_values(<<"max-forwards">>, Headers)) of
        [] -> 70;
        [Forwards0] when Forwards0>=0, Forwards0<300 -> Forwards0;
        _ -> throw({invalid, <<"Max-Forwards">>})
    end,
    Routes = case uris(proplists:get_all_values(<<"route">>, Headers)) of
        error -> throw({invalid, <<"Route">>});
        Routes0 -> Routes0
    end,
    Contacts = case uris(proplists:get_all_values(<<"contact">>, Headers)) of
        error -> throw({invalid, <<"Contact">>});
        Contacts0 -> Contacts0
    end,
    Expires = case integers(proplists:get_all_values(<<"expires">>, Headers)) of
        [] -> undefined;
        [Expires0] when Expires0>=0 -> Expires0;
        _ -> throw({invalid, <<"Expires">>})
    end,
    ContentType = case tokens(proplists:get_all_values(<<"content-type">>, Headers)) of
        [] -> undefined;
        [ContentType0] -> ContentType0;
        _ -> throw({invalid, <<"Content-Type">>})
    end,
    Require = case tokens(proplists:get_all_values(<<"require">>, Headers)) of
        error -> throw({invalid, <<"Require">>});
        Require0 -> [N || {N, _} <- Require0]
    end,
    Supported = case tokens(proplists:get_all_values(<<"supported">>, Headers)) of
        error -> throw({invalid, <<"Supported">>});
        Supported0 -> [N || {N, _} <- Supported0]
    end,
    Event = case tokens(proplists:get_all_values(<<"event">>, Headers)) of
        [] ->
            case SipMsg#sipmsg.class of
                {req, 'SUBSCRIBE'} -> throw({invalid, <<"Event">>});
                {req, 'NOTIFY'} -> throw({invalid, <<"Event">>});
                _ -> undefined
            end;
        [Event0] -> 
            Event0;
        _ -> 
            throw({invalid, <<"Event">>})
    end,
    RestHeaders = lists:filter(
        fun({Name, _}) ->
            case Name of
                <<"from">> -> false;
                <<"to">> -> false;
                <<"call-id">> -> false;
                <<"via">> -> false;
                <<"cseq">> -> false;
                <<"max-forwards">> -> false;
                <<"route">> -> false;
                <<"contact">> -> false;
                <<"expires">> -> false;
                <<"require">> -> false;
                <<"supported">> -> false;
                <<"event">> -> false;
                <<"content-type">> -> false;
                <<"content-length">> -> false;
                _ -> true
            end
        end, Headers),
    #sipmsg{body=Body} = SipMsg,
    ParsedBody = case ContentType of
        {<<"application/sdp">>, _} ->
            case nksip_sdp:parse(Body) of
                error -> Body;
                SDP -> SDP
            end;
        {<<"application/nksip.ebf.base64">>, _} ->
            case catch binary_to_term(base64:decode(Body)) of
                {'EXIT', _} -> Body;
                ErlBody -> ErlBody
            end;
        _ ->
            Body
    end,
    SipMsg#sipmsg{
        from = {From, FromTag},
        to = {To, ToTag},
        vias = Vias,
        cseq = CSeq,
        forwards = Forwards,
        routes = Routes,
        contacts = Contacts,
        expires = Expires,
        content_type = ContentType,
        require = Require,
        supported = Supported,
        event = Event,
        headers = RestHeaders,
        body = ParsedBody,
        to_tag_candidate = <<>>
    }.

          
%% @private
-spec scheme(term()) ->
    nksip:scheme().

scheme(sip) ->
    sip;
scheme(sips) ->
    sips;
scheme(tel) ->
    tel;
scheme(mailto) ->
    mailto;
scheme(Other) ->
    case string:to_lower(nksip_lib:to_list(Other)) of 
        "sip" -> sip;
        "sips" -> sips;
        "tel" -> tel;
        "mailto" -> mailto;
        _ -> list_to_binary(Other)
    end.


%% @private
-spec parse_uris([#uri{}|binary()|string()], [#uri{}]) ->
    [#uri{}] | error.

parse_uris([], Acc) ->
    Acc;

parse_uris([Next|Rest], Acc) ->
    case nksip_parse_uri:uris(Next) of
        error -> error;
        UriList -> parse_uris(Rest, Acc++UriList)
    end.


%% @private
-spec parse_ruris([#uri{}], [#uri{}]) ->
    [#uri{}] | error.

parse_ruris([], Acc) ->
    lists:reverse(Acc);

parse_ruris([#uri{opts=[], headers=[], ext_opts=Opts}=Uri|Rest], Acc) ->
    parse_uris(Rest, [Uri#uri{opts=Opts, ext_opts=[], ext_headers=[]}|Acc]);

parse_ruris(_, _) ->
    error.



%% @private
-spec parse_vias([#via{}|binary()|string()], [#via{}]) ->
    [#via{}] | error.

parse_vias([], Acc) ->
    Acc;

parse_vias([Next|Rest], Acc) ->
    case nksip_parse_via:vias(Next) of
        error -> error;
        UriList -> parse_vias(Rest, Acc++UriList)
    end.


%% @private
-spec parse_tokens([binary()|string()], [nksip:token()]) ->
    [nksip:token()] | error.

parse_tokens([], Acc) ->
    Acc;

parse_tokens([Next|Rest], Acc) ->
    case nksip_parse_tokens:tokens(Next) of
        error -> error;
        TokenList -> parse_tokens(Rest, Acc++TokenList)
    end.


%% @private
-spec parse_integers([binary()|string()], [integer()]) ->
    [integer()] | error.

parse_integers([], Acc) ->
    Acc;

parse_integers([Next|Rest], Acc) ->
    case catch list_to_integer(string:strip(nksip_lib:to_list(Next))) of
        {'EXIT', _} -> error;
        Integer -> parse_integers(Rest, Acc++[Integer])
    end.


%% @private
-spec parse_dates([binary()|string()], [calendar:datetime()]) ->
    [calendar:datetime()] | error.

parse_dates([], Acc) ->
    Acc;

parse_dates([Next|Rest], Acc) ->
    Base = string:strip(nksip_lib:to_list(Next)),
    case lists:reverse(Base) of
        "TMG " ++ _ ->               % Should be in "GMT"
            case catch httpd_util:convert_request_date(Base) of
                {_, _} = Date -> parse_dates(Rest, Acc++[Date]);
                _ -> error
            end;
        _ ->
            error
    end.


%% @doc Modifies a request based on uri options
-spec uri_method(nksip:user_uri(), nksip:method()) ->
    {nksip:method(), nksip:uri()} | error.

uri_method(RawUri, Default) ->
    case nksip_parse:uris(RawUri) of
        [#uri{opts=UriOpts}=Uri] ->
            case lists:keytake(<<"method">>, 1, UriOpts) of
                false ->
                    {Default, Uri};
                {value, {_, RawMethod}, Rest} ->
                    case nksip_parse:method(RawMethod) of
                        Method when is_atom(Method) -> {Method, Uri#uri{opts=Rest}};
                        _ -> error
                    end;
                _ ->
                    error
            end;
        _ ->
            error
    end.

