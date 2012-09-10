%% @author Justin Sheehy <justin@basho.com>
%% @copyright 2007-2009 Basho Technologies
%%
%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%%
%%        http://www.apache.org/licenses/LICENSE-2.0
%%
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.

-module(wrq).
-author('Justin Sheehy <justin@basho.com>').

-export([create/4,load_dispatch_data/7]).
-export([method/1,version/1,peer/1,disp_path/1,path/1,raw_path/1,path_info/1,
         response_code/1,req_cookie/1,req_qs/1,req_headers/1,req_body/1,
         stream_req_body/2,resp_redirect/1,resp_headers/1,resp_body/1,
         app_root/1,path_tokens/1, host_tokens/1, port/1]).
-export([path_info/2,get_req_header/2,do_redirect/2,fresh_resp_headers/2,
         get_resp_header/2,set_resp_header/3,set_resp_headers/2,
         set_disp_path/2,set_req_body/2,set_resp_body/2,set_response_code/2,
         merge_resp_headers/2,remove_resp_header/2,
         append_to_resp_body/2,append_to_response_body/2,
         max_recv_body/1,set_max_recv_body/2,
         get_cookie_value/2,get_qs_value/2,get_qs_value/3,set_peer/2,
         add_note/3, get_notes/1]).

% @type reqdata(). The opaque data type used for req/resp data structures.
-include_lib("include/wm_reqdata.hrl").
-include_lib("include/wm_reqstate.hrl").
-include_lib("eunit/include/eunit.hrl").


create(Method,Version,RawPath,Headers) ->
    create(#wm_reqdata{method=Method,version=Version,
                       raw_path=RawPath,req_headers=Headers,
      wm_state=defined_on_call,
      path="defined_in_create",
      req_cookie=defined_in_create,
      req_qs=defined_in_create,
      peer="defined_in_wm_req_srv_init",
      req_body=not_fetched_yet,
      max_recv_body=(1024*(1024*1024)),
      % Stolen from R13B03 inet_drv.c's TCP_MAX_PACKET_SIZE definition
      max_recv_hunk=(64*(1024*1024)),
      app_root="defined_in_load_dispatch_data",
      path_info=dict:new(),
      path_tokens=defined_in_load_dispatch_data,
      disp_path=defined_in_load_dispatch_data,
      resp_redirect=false, resp_headers=mochiweb_headers:empty(),
      resp_body = <<>>, response_code=500,
      notes=[]}).
create(RD = #wm_reqdata{raw_path=RawPath}) ->
    {Path, _, _} = mochiweb_util:urlsplit_path(RawPath),
    Cookie = case get_req_header("cookie", RD) of
                 undefined -> [];
                 Value -> mochiweb_cookies:parse_cookie(Value)
             end,
    {_, QueryString, _} = mochiweb_util:urlsplit_path(RawPath),
    ReqQS = mochiweb_util:parse_qs(QueryString),
    RD#wm_reqdata{path=Path,req_cookie=Cookie,req_qs=ReqQS}.
load_dispatch_data(PathInfo, HostTokens, Port, PathTokens, AppRoot,
                   DispPath, RD) ->
    RD#wm_reqdata{path_info=PathInfo,host_tokens=HostTokens,
                  port=Port,path_tokens=PathTokens,
                  app_root=AppRoot,disp_path=DispPath}.

method(_RD = #wm_reqdata{method=Method}) -> Method.

version(_RD = #wm_reqdata{version=Version})
  when is_tuple(Version), size(Version) == 2,
     is_integer(element(1,Version)), is_integer(element(2,Version)) -> Version.

peer(_RD = #wm_reqdata{peer=Peer}) when is_list(Peer) -> Peer.

app_root(_RD = #wm_reqdata{app_root=AR}) when is_list(AR) -> AR.

% all three paths below are strings
disp_path(_RD = #wm_reqdata{disp_path=DP}) when is_list(DP) -> DP.

path(_RD = #wm_reqdata{path=Path}) when is_list(Path) -> Path.

raw_path(_RD = #wm_reqdata{raw_path=RawPath}) when is_list(RawPath) -> RawPath.

path_info(_RD = #wm_reqdata{path_info=PathInfo}) -> PathInfo. % dict

path_tokens(_RD = #wm_reqdata{path_tokens=PathT}) -> PathT. % list of strings

host_tokens(_RD = #wm_reqdata{host_tokens=HostT}) -> HostT. % list of strings

port(_RD = #wm_reqdata{port=Port}) -> Port. % integer

response_code(_RD = #wm_reqdata{response_code=C}) when is_integer(C) -> C.

req_cookie(_RD = #wm_reqdata{req_cookie=C}) when is_list(C) -> C. % string

%% @spec req_qs(reqdata()) -> [{Key, Value}]
req_qs(_RD = #wm_reqdata{req_qs=QS}) when is_list(QS) -> QS.

req_headers(_RD = #wm_reqdata{req_headers=ReqH}) -> ReqH. % mochiheaders

req_body(_RD = #wm_reqdata{wm_state=ReqState0,max_recv_body=MRB}) ->
    Req = webmachine_request:new(ReqState0),
    {ReqResp, ReqState} = Req:req_body(MRB),
    put(tmp_reqstate, ReqState),
    maybe_conflict_body(ReqResp).

stream_req_body(_RD = #wm_reqdata{wm_state=ReqState0}, MaxHunk) ->
    Req = webmachine_request:new(ReqState0),
    {ReqResp, ReqState} = Req:stream_req_body(MaxHunk),
    put(tmp_reqstate, ReqState),
    maybe_conflict_body(ReqResp).

max_recv_body(_RD = #wm_reqdata{max_recv_body=X}) when is_integer(X) -> X.

set_max_recv_body(X, RD) when is_integer(X) -> RD#wm_reqdata{max_recv_body=X}.

maybe_conflict_body(BodyResponse) ->
    case BodyResponse of
        stream_conflict ->
            exit("wrq:req_body and wrq:stream_req_body conflict");
        {error, req_body_too_large} ->
            exit("request body too large");
        _ ->
            BodyResponse
    end.

resp_redirect(_RD = #wm_reqdata{resp_redirect=true}) -> true;
resp_redirect(_RD = #wm_reqdata{resp_redirect=false}) -> false.

resp_headers(_RD = #wm_reqdata{resp_headers=RespH}) -> RespH. % mochiheaders

resp_body(_RD = #wm_reqdata{resp_body=undefined}) -> undefined;
resp_body(_RD = #wm_reqdata{resp_body={stream,X}}) -> {stream,X};
resp_body(_RD = #wm_reqdata{resp_body={stream,X,Y}}) -> {stream,X,Y};
resp_body(_RD = #wm_reqdata{resp_body={writer,X}}) -> {writer,X};
resp_body(_RD = #wm_reqdata{resp_body=RespB}) when is_binary(RespB) -> RespB;
resp_body(_RD = #wm_reqdata{resp_body=RespB}) -> iolist_to_binary(RespB).

%% --

path_info(Key, RD) when is_atom(Key) ->
    case dict:find(Key, path_info(RD)) of
        {ok, Value} when is_list(Value); is_integer(Value) ->
            Value; % string (for host or path match)
                   % or integer (for port match)
        error -> undefined
    end.

get_req_header(HdrName, RD) -> % string->string
    mochiweb_headers:get_value(HdrName, req_headers(RD)).

do_redirect(true, RD) ->  RD#wm_reqdata{resp_redirect=true};
do_redirect(false, RD) -> RD#wm_reqdata{resp_redirect=false}.

set_peer(P, RD) when is_list(P) -> RD#wm_reqdata{peer=P}. % string

set_disp_path(P, RD) when is_list(P) -> RD#wm_reqdata{disp_path=P}. % string

set_req_body(Body, RD) -> RD#wm_reqdata{req_body=Body}.

set_resp_body(Body, RD) -> RD#wm_reqdata{resp_body=Body}.

set_response_code(Code, RD) when is_integer(Code) ->
    RD#wm_reqdata{response_code=Code}.

get_resp_header(HdrName, _RD=#wm_reqdata{resp_headers=RespH}) ->
    mochiweb_headers:get_value(HdrName, RespH).
set_resp_header(K, V, RD=#wm_reqdata{resp_headers=RespH})
  when is_list(K),is_list(V) ->
    RD#wm_reqdata{resp_headers=mochiweb_headers:enter(K, V, RespH)}.
set_resp_headers(Hdrs, RD=#wm_reqdata{resp_headers=RespH}) ->
    F = fun({K, V}, Acc) -> mochiweb_headers:enter(K, V, Acc) end,
    RD#wm_reqdata{resp_headers=lists:foldl(F, RespH, Hdrs)}.
fresh_resp_headers(Hdrs, RD) ->
    F = fun({K, V}, Acc) -> mochiweb_headers:enter(K, V, Acc) end,
    RD#wm_reqdata{resp_headers=lists:foldl(F, mochiweb_headers:empty(), Hdrs)}.
remove_resp_header(K, RD=#wm_reqdata{resp_headers=RespH}) when is_list(K) ->
    RD#wm_reqdata{resp_headers=mochiweb_headers:from_list(
                                 proplists:delete(K,
                                     mochiweb_headers:to_list(RespH)))}.

merge_resp_headers(Hdrs, RD=#wm_reqdata{resp_headers=RespH}) ->
    F = fun({K, V}, Acc) -> mochiweb_headers:insert(K, V, Acc) end,
    NewHdrs = lists:foldl(F, RespH, Hdrs),
    RD#wm_reqdata{resp_headers=NewHdrs}.

append_to_resp_body(Data, RD) -> append_to_response_body(Data, RD).
append_to_response_body(Data, RD=#wm_reqdata{resp_body=RespB}) ->
    case is_binary(Data) of
        true ->
            Data0 = RespB,
            Data1 = <<Data0/binary,Data/binary>>,
            RD#wm_reqdata{resp_body=Data1};
        false -> % MUST BE an iolist! else, fail.
            append_to_response_body(iolist_to_binary(Data), RD)
    end.

get_cookie_value(Key, RD) when is_list(Key) -> % string
    proplists:get_value(Key, req_cookie(RD)).

get_qs_value(Key, RD) when is_list(Key) -> % string
    proplists:get_value(Key, req_qs(RD)).

get_qs_value(Key, Default, RD) when is_list(Key) ->
    proplists:get_value(Key, req_qs(RD), Default).

add_note(K, V, RD) -> RD#wm_reqdata{notes=[{K, V} | RD#wm_reqdata.notes]}.

get_notes(RD) -> RD#wm_reqdata.notes.

make_wrq(Method, RawPath, Headers) ->
    create(Method, {1,1}, RawPath, mochiweb_headers:from_list(Headers)).

accessor_test() ->
    R0 = make_wrq('GET', "/foo?a=1&b=2", [{"Cookie", "foo=bar"}]),
    R = set_peer("127.0.0.1", R0),
    ?assertEqual('GET', method(R)),
    ?assertEqual({1,1}, version(R)),
    ?assertEqual("/foo", path(R)),
    ?assertEqual("/foo?a=1&b=2", raw_path(R)),     
    ?assertEqual([{"a", "1"}, {"b", "2"}], req_qs(R)),
    ?assertEqual({"1", "2"}, {get_qs_value("a", R), get_qs_value("b", R)}),
    ?assertEqual("3", get_qs_value("c", "3", R)),
    ?assertEqual([{"foo", "bar"}], req_cookie(R)),
    ?assertEqual("bar", get_cookie_value("foo", R)),
    ?assertEqual("127.0.0.1", peer(R)).

    
simple_dispatch_test() ->
    R0 = make_wrq('GET', "/foo?a=1&b=2", [{"Cookie", "foo=bar"}]),
    R1 = set_peer("127.0.0.1", R0),    
    {_, _, HostTokens, Port, PathTokens, Bindings, AppRoot, StringPath} = 
        webmachine_dispatcher:dispatch("127.0.0.1", "/foo", 
                                       [{["foo"], foo_resource, []}]),
    R = load_dispatch_data(Bindings,
                           HostTokens,
                           Port,
                           PathTokens,
                           AppRoot,
                           StringPath,
                           R1),
    ?assertEqual(".", app_root(R)),
    ?assertEqual(80, port(R)).
