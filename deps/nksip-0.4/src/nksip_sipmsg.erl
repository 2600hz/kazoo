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

-export([meta/2, metas/2, header/2, header/3, all_headers/1]).
-export([supported/2, require/2, is_dialog_forming/1, get_handle/1, parse_handle/1]).
-export([remote_meta/2, remote_metas/2]).
-export_type([id/0, field/0]).
-include("nksip.hrl").

-type id() :: binary().

-type field() ::  
    handle | internal_id | app_id | app_name | dialog_handle | subscription_handle |
    proto | local | remote | method | ruri | scheme | user | domain | aor |
    code | reason_phrase | content_type | body | call_id | vias | 
    from | from_tag | from_scheme | from_user | from_domain | 
    to | to_tag | to_scheme | to_user | to_domain | 
    cseq_num | cseq_method | forwards | routes | contacts | require | supported | 
    expires | expired | retry_after | event | refer_to | realms | rseq_num | rack | 
    {header, string()|binary()} | all_headers | string() | binary().



%% ===================================================================
%% Private
%% ===================================================================




%% @doc Extracts a specific metadata from a request or response
%% Valid fields are defined in {@link nksip_request:field()} and 
%% {@link nksip_response:field()}.
-spec meta(field(), nksip:request()|nksip:response()) ->
    term().

meta(Name, #sipmsg{}=SipMsg) when is_list(Name); is_binary(Name) ->
    header(Name, SipMsg);

meta(Name, #sipmsg{class=Class, ruri=RUri, from=From, to=To}=S) ->
    case Name of
        handle -> get_handle(S);
        internal_id -> S#sipmsg.id;
        app_id -> S#sipmsg.app_id;
        app_name -> apply(S#sipmsg.app_id, name, []);
        dialog_handle -> nksip_dialog_lib:get_handle(S);
        subscription_handle -> nksip_subscription_lib:get_handle(S);
        proto -> 
            case S#sipmsg.transport of
                #transport{proto=P} -> P; 
                _ -> undefined 
            end;
        local -> 
            case S#sipmsg.transport of 
                #transport{proto=P, local_ip=Ip, local_port=Port, resource=Res} -> 
                    {P, Ip, Port, Res};
                _ -> 
                    undefined
            end;
        remote -> 
            case S#sipmsg.transport of 
                #transport{proto=P, remote_ip=Ip, remote_port=Port, resource=Res} -> 
                    {P, Ip, Port, Res};
                _ -> 
                    undefined
            end;
        method -> case Class of {req, Method} -> Method; _ -> undefined end;
        ruri -> S#sipmsg.ruri;
        scheme -> RUri#uri.scheme;
        user -> RUri#uri.user;
        domain -> RUri#uri.domain;
        aor -> {RUri#uri.scheme, RUri#uri.user, RUri#uri.domain};
        code -> case Class of {resp, Code, _Reason} -> Code; _ -> 0 end;
        reason_phrase -> case Class of {resp, _Code, Reason} -> Reason; _ -> <<>> end;
        content_type -> S#sipmsg.content_type;
        body -> S#sipmsg.body;
        call_id -> S#sipmsg.call_id;
        vias -> S#sipmsg.vias;
        from -> element(1, From);
        from_tag -> element(2, From);
        from_scheme -> (element(1, From))#uri.scheme;
        from_user -> (element(1, From))#uri.user;
        from_domain -> (element(1, From))#uri.domain;
        to -> element(1, To);
        to_tag -> element(2, To);
        to_scheme -> (element(1, To))#uri.scheme;
        to_user -> (element(1, To))#uri.user;
        to_domain -> (element(1, To))#uri.domain;
        cseq_num -> element(1, S#sipmsg.cseq);
        cseq_method -> element(2, S#sipmsg.cseq);
        forwards -> S#sipmsg.forwards;
        routes -> S#sipmsg.routes;
        contacts -> S#sipmsg.contacts;
        require -> S#sipmsg.require;
        supported -> S#sipmsg.supported;
        expires -> S#sipmsg.expires;
        expired -> expired(S);
        retry_after -> 
            case header(<<"retry-after">>, S, integers) of
                [] -> undefined;
                [Retry] -> Retry;
                _ -> error
            end;
        event -> S#sipmsg.event;
        refer_to -> 
            case header(<<"refer-to">>, S, uris) of
                [ReferTo] -> ReferTo;
                _ -> error
            end;
        realms -> nksip_auth:realms(S);
        rseq_num -> 
            case header(<<"rseq">>, S, integers) of [RSeq] -> RSeq; _ -> undefined end;
        rack ->
            case header(<<"rack">>, S) of 
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
        all_headers -> all_headers(S);
        {header, HeaderName} -> header(HeaderName, S);
        _ -> error({invalid_field, Name})
    end.


%% @doc Extracts a group of metadatas from a request or response
-spec metas([field()], nksip:request()|nksip:response()) ->
    [{field(), term()}].

metas(Fields, #sipmsg{}=SipMsg) when is_list(Fields) ->
    [{Field, meta(Field, SipMsg)} || Field <- Fields].


%% @doc Extracts a header from a request or response
-spec header(string()|binary(), nksip:request()|nksip:response()) ->
    [binary()].

header(Name, SipMsg) when is_list(Name) ->
    header(list_to_binary(Name), SipMsg);

header(Name, S) ->
    case Name of
        <<"call-id">> -> 
            S#sipmsg.call_id;
        <<"via">> -> 
            [nksip_lib:to_binary(Via) || Via <- S#sipmsg.vias];
        <<"from">> -> 
            [nksip_unparse:uri(element(1, S#sipmsg.from))];
        <<"to">> -> 
            [nksip_unparse:uri(element(1, S#sipmsg.to))];
        <<"cseq">> ->
            #sipmsg{cseq={CSeqNum, Method}} = S,
            [<<(nksip_lib:to_binary(CSeqNum))/binary, 32, 
              (nksip_lib:to_binary(Method))/binary>>];
        <<"forwards">> -> 
            [nksip_lib:to_binary(S#sipmsg.forwards)];
        <<"route">> -> 
            [nksip_lib:to_binary(Route) || Route <- S#sipmsg.routes];
        <<"contact">> -> 
            [nksip_lib:to_binary(Contact) || Contact <- S#sipmsg.contacts];
        <<"content-type">> -> 
            case S#sipmsg.content_type of
                undefined -> [];
                ContentType -> [nksip_unparse:token(ContentType)]
            end;
        <<"require">> -> 
            case S#sipmsg.require of
                [] -> [];
                Require -> [nksip_lib:bjoin(Require)]
            end;
        <<"supported">> -> 
            case S#sipmsg.supported of
                [] -> [];
                Supported -> [nksip_lib:bjoin(Supported)]
            end;
        <<"expires">> -> 
            case S#sipmsg.expires of
                undefined -> [];
                Expires -> [nksip_lib:to_binary(Expires)]
            end;
        <<"event">> -> 
            case S#sipmsg.event of
                undefined -> [];
                Event -> [nksip_unparse:token(Event)]
            end;
        _ -> 
            [nksip_unparse:header(Value) || 
                Value <- proplists:get_all_values(Name, S#sipmsg.headers)]
    end.


%% @doc Extracts a header from a request or response and formats it.
-spec header(string()|binary(), nksip:request()|nksip:response(), 
             uris|tokens|integers|dates) ->
    [term()] | error.

header(Name, #sipmsg{}=SipMsg, Type) ->
    Raw = header(Name, SipMsg),
    case Type of
        uris -> nksip_parse:uris(Raw);
        tokens -> nksip_parse:tokens(Raw);
        integers -> nksip_parse:integers(Raw);
        dates -> nksip_parse:dates(Raw)
    end.


%% @private
all_headers(SipMsg) ->
    lists:flatten([
        {<<"call-id">>, header(<<"call-id">>, SipMsg)},
        {<<"via">>, header(<<"via">>, SipMsg)},
        {<<"from">>, header(<<"from">>, SipMsg)},
        {<<"to">>, header(<<"to">>, SipMsg)},
        {<<"cseq">>, header(<<"cseq">>, SipMsg)},
        {<<"forwards">>, header(<<"forwards">>, SipMsg)},
        case SipMsg#sipmsg.routes of
            [] -> [];
            _ -> {<<"route">>, header(<<"route">>, SipMsg)}
        end,
        case SipMsg#sipmsg.contacts of
            [] -> [];
            _ -> {<<"contact">>, header(<<"contact">>, SipMsg)}
        end,
        case SipMsg#sipmsg.content_type of
            undefined -> [];
            _ -> {<<"content-type">>, header(<<"content-type">>, SipMsg)}
        end,
        case SipMsg#sipmsg.require of
            [] -> [];
            _ -> {<<"require">>, header(<<"require">>, SipMsg)}
        end,
        case SipMsg#sipmsg.supported of
            [] -> [];
            _ -> {<<"supported">>, header(<<"supported">>, SipMsg)}
        end,
        case SipMsg#sipmsg.expires of
            undefined -> [];
            _ -> {<<"expires">>, header(<<"expires">>, SipMsg)}
        end,
        case SipMsg#sipmsg.event of
            undefined -> [];
            _ -> {<<"event">>, header(<<"event">>, SipMsg)}
        end,
        SipMsg#sipmsg.headers
    ]).


%% @doc Checks if a token is in Supported header
-spec supported(binary(), nksip:request()|nksip:response()) ->
    boolean().

supported(Token, #sipmsg{supported=Supported}) ->
    lists:member(Token, Supported).


%% @doc Checks if a token is in Require header
-spec require(binary(), nksip:request()|nksip:response()) ->
    boolean().

require(Token, #sipmsg{require=Require}) ->
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


%% @doc Checks if a request has expired looking at its Expires header
%% and Data header or received date if missing
-spec expired(nksip:request()) ->
    boolean().

expired(#sipmsg{expires=Expires, start=Start}=Req) ->
    case is_integer(Expires) of
        true ->
            case nksip_sipmsg:header(<<"date">>, Req, dates) of
                [Date] ->
                    Final = nksip_lib:gmt_to_timestamp(Date) + Expires,
                    case nksip_lib:timestamp() of
                        TS when TS > Final -> true;
                        _ -> false
                    end;
                _ ->
                    Final = Start/1000 + Expires,
                    case nksip_lib:timestamp() of
                        TS when TS > Final -> true;
                        _ -> false
                    end
            end;
        false ->
            false
    end.



%% @private
-spec get_handle(nksip:request()|nksip:response()|nksip:handle()) ->
    nksip:handle().

get_handle(<<Ch, _/binary>>=Handle) when Ch==$R; Ch==$S ->
    Handle;

get_handle(#sipmsg{app_id=AppId, class=Class, id=MsgId, call_id=CallId}) ->
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
    >>;

get_handle(_) ->
    error(invalid_handle).
    

%% @private
-spec parse_handle(nksip:handle()) -> 
    {req|resp, nksip:app_id(), id(), nksip:call_id()}.

parse_handle(<<Ch, $_, Id:6/binary, $_, App:7/binary, $_, CallId/binary>>)
         when Ch==$R; Ch==$S ->
    Class = case Ch of
        $R -> req;
        $S -> resp
    end,
    {Class, binary_to_existing_atom(App, latin1), Id, CallId};

parse_handle(_) ->
    error(invalid_handle).


%% @doc Extracts remote meta
-spec remote_meta(field(), nksip:handle()) ->
    {ok, term()} | {error, term()}.

remote_meta(Field, Handle) ->
    case remote_metas([Field], Handle) of
        {ok, [{_, Value}]} -> {ok, Value};
        {error, Error} -> {error, Error}
    end.


%% @doc Extracts remote metas
-spec remote_metas([field()], id()) ->
    {ok, [{field(), term()}]} | {error, term()}.

remote_metas(Fields, Handle) when is_list(Fields) ->
    {_Class, AppId, MsgId, CallId} = parse_handle(Handle),
    Fun = fun(SipMsg) ->
        case catch metas(Fields, SipMsg) of
            {'EXIT', {{invalid_field, Field}, _}} -> 
                {error, {invalid_field, Field}};
            Values -> 
                {ok, Values}
        end
    end,
    case nksip_call:apply_sipmsg(AppId, CallId, MsgId, Fun) of
        {apply, {ok, Values}} -> 
            {ok, Values};
        {apply, {error, {invalid_field, Field}}} -> 
            error({invalid_field, Field});
        {error, Error} -> 
            {error, Error}
    end.




