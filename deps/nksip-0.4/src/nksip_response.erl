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

%% @doc User Response Management Functions
-module(nksip_response).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-include("nksip.hrl").

-export([get_handle/1, app_id/1, app_name/1, code/1, body/1, call_id/1]).
-export([meta/2, metas/2, header/2]).
-export([wait_491/0]).

-include("nksip.hrl").
-include("nksip_call.hrl").


%% ===================================================================
%% Public
%% ===================================================================



%% @doc Gets response's id
-spec get_handle(nksip:response()|nksip:handle()) ->
    {ok, nksip:handle()}.

get_handle(Term) ->
    case nksip_sipmsg:get_handle(Term) of
        <<"S_", _/binary>> = Handle -> {ok, Handle};
        _ -> error(invalid_response)
    end.


%% @doc Gets internal app's id
-spec app_id(nksip:response()|nksip:handle()) -> 
    {ok, nksip:app_id()}.

app_id(#sipmsg{class={resp, _, _}, app_id=AppId}) ->
    {ok, AppId};
app_id(Handle) ->
    case nksip_sipmsg:parse_handle(Handle) of
        {resp, AppId, _Id, _CallId} -> {ok, AppId};
        _ -> error(invalid_response)
    end.


%% @doc Gets app's name
-spec app_name(nksip:response()|nksip:handle()) -> 
    {ok, nksip:app_name()}.

app_name(Req) -> 
    {ok, AppId} = app_id(Req),
    {ok, AppId:name()}.


%% @doc Gets the calls's id of a response id
-spec call_id(nksip:response()|nksip:handle()) ->
    {ok, nksip:call_id()}.

call_id(#sipmsg{class={resp, _, _}, call_id=CallId}) ->
    {ok, CallId};
call_id(Handle) ->
    case nksip_sipmsg:parse_handle(Handle) of
        {resp, _AppId, _Id, CallId} -> {ok, CallId};
        _ -> error(invalid_response)
    end.


%% @doc Gets the response's code
-spec code(nksip:response()|nksip:handle()) ->
    {ok, nksip:sip_code()} | {error, term()}.

code(#sipmsg{class={resp, Code, _Phrase}}) -> 
    {ok, Code};
code(Term) when is_binary(Term) ->
    meta(code, Term).


%% @doc Gets the body of the response
-spec body(nksip:response()|nksip:handle()) ->
    {ok, nksip:body()} | {error, term()}.

body(#sipmsg{class={resp, _, _}, body=Body}) -> 
    {ok, Body};
body(Handle) ->
    meta(body, Handle).


%% @doc Get a specific metadata
-spec meta(nksip_sipmsg:field(), nksip:response()|nksip:handle()) ->
    {ok, term()} | {error, term()}.

meta(Field, #sipmsg{class={resp, _, _}}=Req) -> 
    {ok, nksip_sipmsg:meta(Field, Req)};
meta(Field, Handle) ->
    nksip_sipmsg:remote_meta(Field, Handle).


%% @doc Get a group of specific metadata
-spec metas([nksip_sipmsg:field()], nksip:response()|nksip:handle()) ->
    {ok, [{nksip_sipmsg:field(), term()}]} | {error, term()}.

metas(Fields, #sipmsg{class={resp, _, _}}=Req) when is_list(Fields) ->
    {ok, nksip_sipmsg:metas(Fields, Req)};
metas(Fields, Handle) when is_list(Fields) ->
    nksip_sipmsg:remote_metas(Fields, Handle).


%% @doc Gets values for a header in a response.
-spec header(string()|binary(), nksip:response()|nksip:handle()) -> 
    {ok, [binary()]} | {error, term()}.

header(Name, #sipmsg{class={resp, _, _}}=Req) -> 
    {ok, nksip_sipmsg:header(Name, Req)};
header(Name, Handle) when is_binary(Handle) ->
    meta(nksip_lib:to_binary(Name), Handle).


%% @doc Sleeps a random time between 2.1 and 4 secs. It should be called after
%% receiving a 491 response and before trying the response again.
-spec wait_491() -> 
    ok.
wait_491() ->
    timer:sleep(10*crypto:rand_uniform(210, 400)).


