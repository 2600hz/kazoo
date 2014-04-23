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

%% @doc Internal request and responses management.
%% This module allows to work with raw requests and responses (#sipmsg{} records)

-module(nksip_sipmsg).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([field/2, fields/2, named_fields/2, header/2, header/3, supported/2, require/2]).
-export([is_dialog_forming/1, get_id/1, parse_id/1]).
-export_type([id/0]).
-include("nksip.hrl").

-type id() :: binary().
-type field() :: nksip_request:field() | nksip_response:field().



%% ===================================================================
%% Private
%% ===================================================================


%% @doc Extracts a specific field from a sipmsg.
%% Valid fields are defined in {@link nksip_request:field()} and 
%% {@link nksip_response:field()}.
-spec field(nksip:request() | nksip:response(), 
            nksip_request:field() | nksip_response:field()) -> 
    term().

field(#sipmsg{}=S, Field) ->
    #sipmsg{class=Class, ruri=RUri, from={From, _}, to={To, _}, transport=T} = S,
    case Field of
        id -> get_id(S);
        internal_id -> S#sipmsg.id;
        app_name -> apply(S#sipmsg.app_id, name, []);
        app_id -> S#sipmsg.app_id;
        dialog_id -> nksip_dialog:get_id(S);
        proto -> case T of #transport{proto=P} -> P; _ -> undefined end;
        local -> 
            case T of 
                #transport{proto=P, local_ip=Ip, local_port=Port, resource=Res} -> 
                    {P, Ip, Port, Res};
                _ -> 
                    undefined
            end;
        remote -> 
            case T of 
                #transport{proto=P, remote_ip=Ip, remote_port=Port, resource=Res} -> 
                    {P, Ip, Port, Res};
                _ -> 
                    undefined
            end;
        method -> case Class of {req, Method} -> Method; _ -> undefined end;
        ruri -> nksip_unparse:uri(RUri);
        ruri_scheme -> RUri#uri.scheme;
        ruri_user -> RUri#uri.user;
        ruri_domain -> RUri#uri.domain;
        parsed_ruri -> S#sipmsg.ruri;
        scheme -> RUri#uri.scheme;
        aor -> {RUri#uri.scheme, RUri#uri.user, RUri#uri.domain};
        call_id -> S#sipmsg.call_id;
        vias -> [nksip_lib:to_binary(Via) || Via <- S#sipmsg.vias];
        parsed_vias -> S#sipmsg.vias;
        from -> nksip_unparse:uri(From);
        from_scheme -> From#uri.scheme;
        from_user -> From#uri.user;
        from_domain -> From#uri.domain;
        parsed_from -> From;
        to -> nksip_unparse:uri(To);
        to_scheme -> To#uri.scheme;
        to_user -> To#uri.user;
        to_domain -> To#uri.domain;
        parsed_to -> To;
        cseq -> 
            #sipmsg{cseq={CSeqNum, Method}} = S,
            <<(nksip_lib:to_binary(CSeqNum))/binary, 32, 
              (nksip_lib:to_binary(Method))/binary>>;
        parsed_cseq -> S#sipmsg.cseq;
        cseq_num -> element(1, S#sipmsg.cseq);
        cseq_method -> element(2, S#sipmsg.cseq);
        forwards -> S#sipmsg.forwards;
        routes -> [nksip_lib:to_binary(Route) || Route <- S#sipmsg.routes];
        parsed_routes -> S#sipmsg.routes;
        contacts -> [nksip_lib:to_binary(Contact) || Contact <- S#sipmsg.contacts];
        parsed_contacts -> S#sipmsg.contacts;
        require -> nksip_lib:bjoin(S#sipmsg.require);
        parsed_require -> S#sipmsg.require;
        supported -> nksip_lib:bjoin(S#sipmsg.supported);
        parsed_supported -> S#sipmsg.supported;
        allow -> header(S, <<"allow">>);
        body -> S#sipmsg.body;
        expires -> nksip_lib:to_binary(S#sipmsg.expires);
        parsed_expires -> S#sipmsg.expires;
        event -> nksip_unparse:token(S#sipmsg.event);
        parsed_event -> S#sipmsg.event;
        all_headers -> all_headers(S);
        code -> case Class of {resp, Code, _Reason} -> Code; _ -> 0 end;
        reason_phrase -> case Class of {resp, _Code, Reason} -> Reason; _ -> <<>> end;
        realms -> nksip_auth:realms(S);
        rseq_num -> 
            case header(S, <<"rseq">>, integers) of [RSeq] -> RSeq; _ -> undefined end;
        content_type -> nksip_unparse:token(S#sipmsg.content_type);
        parsed_content_type -> S#sipmsg.content_type;
        parsed_rack ->
            case header(S, <<"rack">>) of 
                [RAck] ->
                    case nksip_lib:tokens(RAck) of
                        [RSeq, CSeq, Method] ->
                            {
                                nksip_lib:to_integer(RSeq),
                                nksip_lib:to_integer(CSeq),
                                nksip_parse:method(Method)
                            };
                        _ ->
                            undefined
                    end;
                _ ->
                
                    undefined
            end;
        subscription_id -> nksip_subscription:get_id(S);
        _ when is_binary(Field) -> header(S, Field);
        {value, _Name, Value} -> Value;
        _ -> invalid_field 
    end.



%% @doc Extracts a group of fields from a #sipmsg.
-spec fields(nksip:request()|nksip:response(), [field()]) ->
    [term()].

fields(#sipmsg{}=SipMsg, Fields) when is_list(Fields) ->
    [field(SipMsg, Field) || Field <- Fields].


%% @doc Extracts a group of fields from a #sipmsg.
-spec named_fields(nksip:request()|nksip:response(), [field()]) ->
    [term()].

named_fields(#sipmsg{}=SipMsg, Fields) when is_list(Fields) ->
    [
        {
            case Field of 
                {value, Name, _} -> Name;
                _ -> Field
            end,
            field(SipMsg, Field)
        } 
        || Field <- Fields
    ].


%% @doc Extracts a header from a #sipmsg.
-spec header(nksip:request() | nksip:response(),
                 binary() | string()) -> 
    [binary()].

header(#sipmsg{headers=Headers}=SipMsg, Name) ->
    case nksip_lib:to_binary(Name) of
        <<"call-id">> -> [field(SipMsg, call_id)];
        <<"via">> -> field(SipMsg, vias);
        <<"from">> -> [field(SipMsg, from)];
        <<"to">> -> [field(SipMsg, to)];
        <<"cseq">> -> [field(SipMsg, cseq)];
        <<"forwards">> -> [nksip_lib:to_binary(field(SipMsg, forwards))];
        <<"route">> -> field(SipMsg, routes);
        <<"contact">> -> field(SipMsg, contacts);
        <<"content-type">> -> [field(SipMsg, content_type)];
        <<"require">> -> case field(SipMsg, require) of <<>> -> []; R -> [R] end;
        <<"supported">> -> case field(SipMsg, supported) of <<>> -> []; S -> [S] end;
        <<"expires">> -> case field(SipMsg, expires) of <<>> -> []; E -> [E] end;
        <<"event">> -> case field(SipMsg, event) of <<>> -> []; E -> [E] end;
        Name1 -> 
            [nksip_unparse:header(Value) || 
                Value <- proplists:get_all_values(Name1, Headers)]
    end.


%% @doc Extracts a header from a #sipmsg and formats it.
-spec header(nksip:request() | nksip:response(), binary(), uris|tokens|integers|dates) ->
    [term()] | error.

header(#sipmsg{}=SipMsg, Name, Type) ->
    Raw = header(SipMsg, Name),
    case Type of
        uris -> nksip_parse:uris(Raw);
        tokens -> nksip_parse:tokens(Raw);
        integers -> nksip_parse:integers(Raw);
        dates -> nksip_parse:dates(Raw)
    end.


%% @private
all_headers(SipMsg) ->
    lists:flatten([
        {<<"call-id">>, [field(SipMsg, call_id)]},
        {<<"via">>, field(SipMsg, vias)},
        {<<"from">>, [field(SipMsg, from)]},
        {<<"to">>, [field(SipMsg, to)]},
        {<<"cseq">>, [field(SipMsg, cseq)]},
        {<<"forwards">>, [nksip_lib:to_binary(field(SipMsg, forwards))]},
        case field(SipMsg, routes) of
            [] -> [];
            Routes -> {<<"route">>, Routes}
        end,
        case field(SipMsg, contacts) of
            [] -> [];
            Contacts -> {<<"contact">>, Contacts}
        end,
        case field(SipMsg, content_type) of
            <<>> -> [];
            ContentType -> {<<"content-type">>, ContentType}
        end,
        case field(SipMsg, require) of
            <<>> -> [];
            Require -> {<<"require">>, Require}
        end,
        case field(SipMsg, supported) of
            <<>> -> [];
            Supported -> {<<"supported">>, Supported}
        end,
        case field(SipMsg, expires) of
            <<>> -> [];
            Expires -> {<<"expires">>, Expires}
        end,
        case field(SipMsg, event) of
            <<>> -> [];
            Event -> {<<"event">>, Event}
        end,
        SipMsg#sipmsg.headers
    ]).

%% @doc Checks if a token is in Supported header
-spec supported(nksip:request()|nksip:response(), binary()) ->
    boolean().

supported(#sipmsg{supported=Supported}, Token) ->
    lists:member(Token, Supported).


%% @doc Checks if a token is in Require header
-spec require(nksip:request()|nksip:response(), binary()) ->
    boolean().

require(#sipmsg{require=Require}, Token) ->
    lists:member(Token, Require).


%% @doc
-spec is_dialog_forming(nksip:request()) ->
    boolean().

is_dialog_forming(#sipmsg{class={req, Method}, to={_, ToTag}}) ->
    Method == 'NOTIFY' orelse
    (ToTag == <<>> andalso 
        (Method == 'INVITE' orelse Method == 'SUBSCRIBE' orelse Method=='REFER'));

is_dialog_forming(_)  ->
    false.


%% @private
-spec get_id(nksip:request()|nksip:response()) ->
    nksip:id().

get_id(#sipmsg{app_id=AppId, class=Class, id=MsgId, call_id=CallId}) ->
    <<
        case Class of
            {req, _} -> $R;
            {resp, _, _} -> $S
        end,
        $_,
        MsgId/binary,
        $_,
        (atom_to_binary(AppId, latin1))/binary,
        $_,
        CallId/binary
    >>.


%% @private
-spec parse_id(binary()) -> 
    {req|resp, nksip:app_id(), binary(), nksip:call_id()}.

parse_id(Bin) ->
    <<Ch, $_, Id:6/binary, $_, App:7/binary, $_, CallId/binary>> = Bin,
    Class = case Ch of
        $R -> req;
        $S -> resp
    end,
    {Class, binary_to_existing_atom(App, latin1), Id, CallId}.





