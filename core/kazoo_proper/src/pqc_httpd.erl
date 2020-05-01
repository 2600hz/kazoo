%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_httpd).
-behaviour(cowboy_handler).
-behaviour(gen_server).

-export([fetch_req/1, fetch_req/2
        ,get_req/1
        ,wait_for_req/1, wait_for_req/2
        ,update_req/2

        ,base_url/0
        ,status/0
        ,stop/0
        ]).

%% gen_server
-export([start_link/0, start_link/1
        ,init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,code_change/3
        ,terminate/2
        ]).

%% Cowboy callbacks
-export([init/2
        ,handle/2
        ,terminate/3
        ]).

-include("kazoo_proper.hrl").

-define(LISTENER, 'kazoo_proper_httpd').

%% {{Pid, MRef}, TRef, JSONPath | {JSONPath, sender function}}
-type wait_path() :: kz_json:path() | {kz_json:path(), fun()}.
-type wait() :: {kz_term:pid_ref(), reference(), wait_path()}.
-type waits() :: [wait()].
-record(state, {requests = kz_json:new() :: kz_json:object()
               ,waits = [] :: waits()
               }).
-type state() :: #state{}.

-spec start_link() -> {'ok', pid()}.
start_link() ->
    start_link(kz_binary:rand_hex(5)).

-spec start_link(kz_term:ne_binary()) -> {'ok', pid()}.
start_link(LogId) ->
    gen_server:start_link({'local', ?MODULE}, ?MODULE, [LogId], []).

-spec status() -> kz_json:object().
status() ->
    gen_server:call(?MODULE, 'status').

-spec stop() -> 'ok'.
stop() ->
    case whereis(?MODULE) of
        'undefined' -> stop_listener();
        Pid -> gen_server:stop(Pid)
    end.

-spec stop_listener() -> 'ok'.
stop_listener() ->
    cowboy:stop_listener(?LISTENER).

%% @doc fetches the value and removes it from the state if found
-spec fetch_req(kz_json:path()) -> kz_json:api_json_term().
fetch_req(Path) ->
    fetch_req(Path, 'undefined').

%% @doc fetches the value and removes it from the state if found
-spec fetch_req(kz_json:path(), kz_term:api_pos_integer()) -> kz_json:api_json_term().
fetch_req(Path, TimeoutMs) ->
    gen_server:call(?MODULE, {'fetch_req', Path, TimeoutMs}, timeout_or_default(TimeoutMs)).

-spec timeout_or_default(kz_term:api_pos_integer()) -> pos_integer().
timeout_or_default('undefined') -> 5 * ?MILLISECONDS_IN_SECOND;
timeout_or_default(TimeoutMs) when is_integer(TimeoutMs), TimeoutMs > 0 -> TimeoutMs + 100.

%% @doc reads the value and leaves it in the state if found
-spec get_req(kz_json:path()) -> kz_json:api_json_term().
get_req(Path) ->
    gen_server:call(?MODULE, {'get_req', Path}).

%% @doc waits until the request can be fulfilled then returns the value, leaving in state
-spec wait_for_req(kz_json:path()) -> kz_json:api_json_term() |
                                      {'error', 'timeout'}.
wait_for_req(Path) ->
    wait_for_req(Path, 5 * ?MILLISECONDS_IN_SECOND).

%% @doc waits until the request can be fulfilled then returns the value, leaving in state
-spec wait_for_req(kz_json:path(), pos_integer()) ->
                          kz_json:api_json_term() |
                          {'error', 'timeout'}.
wait_for_req([_|_]=Path, TimeoutMs) when is_integer(TimeoutMs), TimeoutMs > 0 ->
    gen_server:call(?MODULE, {'wait_for_req', Path, TimeoutMs}, TimeoutMs + 100).

%% @doc updates the state to store Content at the Path location
-spec update_req(kz_json:path(), binary()) -> 'ok'.
update_req(Path, <<Content/binary>>) ->
    Store = try base64:decode(Content) of
                Decoded -> Decoded
            catch
                'error':_ -> Content
            end,
    ?INFO("trying to store ~p: ~s", [Path, Store]),
    gen_server:call(?MODULE, {'req', Path, Store}).

log_meta(LogId) ->
    kz_log:put_callid(LogId),
    lager:md([{'request_id', LogId}]),
    put('start_time', kz_time:start_time()),
    'ok'.

-spec init(list()) -> {'ok', state()}.
init([LogId]) ->
    log_meta(LogId),
    io:format("starting HTTPD with ~p~n", [LogId]),

    Dispatch = cowboy_router:compile(routes(LogId)),
    {'ok', _Pid} = start_plaintext(Dispatch),
    ?INFO("started HTTPD(~p) at ~s", [_Pid, base_url()]),
    {'ok', #state{}}.

-spec routes(kz_term:ne_binary()) -> cowboy_router:routes().
routes(LogId) -> [{'_', paths_list(LogId)}].

paths_list(LogId) ->
    [default_path(LogId)].

default_path(LogId) ->
    {'_', 'pqc_httpd', [{'log_id', LogId}]}.

start_plaintext(Dispatch) ->
    cowboy:start_clear(?LISTENER
                      ,#{'num_acceptors' => 5}
                      ,#{'env' => #{'dispatch' => Dispatch}}
                      ).

-spec base_url() -> kz_term:ne_binary().
base_url() ->
    Port = ranch:get_port(?LISTENER),
    Host = kz_network_utils:get_hostname(),
    kz_term:to_binary(["http://", Host, $:, integer_to_list(Port), $/]).

-spec init(cowboy_req:req(), kz_term:proplist()) ->
          {'ok', cowboy_req:req(), 'undefined'}.
init(Req, HandlerOpts) ->
    log_meta(props:get_value('log_id', HandlerOpts)),
    handle(Req, HandlerOpts).

-spec handle(cowboy_req:req(), State) -> {'ok', cowboy_req:req(), State}.
handle(Req, State) ->
    put('start_time', kz_time:start_time()),
    handle(Req, State, cowboy_req:method(Req)).

handle(Req, State, <<"POST">>) ->
    add_req_to_state(Req, State);
handle(Req, State, <<"PUT">>) ->
    add_req_to_state(Req, State);
handle(Req, State, <<"GET">>) ->
    get_from_state(Req, State).

get_from_state(Req, State) ->
    Path = cowboy_req:path(Req), % <<"/foo/bar/baz">>
    PathParts = tl(binary:split(Path, <<"/">>, ['global'])),

    {RespCode, Body} =
        case get_req(PathParts) of
            'undefined' -> {404, <<>>};
            Value -> {200, Value}
        end,

    ?INFO("GET req ~s: ~p", [Path, RespCode]),

    Req1 = cowboy_req:reply(RespCode, #{}, Body, Req),
    {'ok', Req1, State}.

add_req_to_state(Req, State) ->
    Path = cowboy_req:path(Req), % <<"/foo/bar/baz">>
    PathParts = tl(binary:split(Path, <<"/">>, ['global'])),

    {Req1, ReqBody} = maybe_handle_multipart(Req),

    RespCode = case get_req(PathParts) of
                   'undefined' -> 201;
                   _Value -> 200
               end,

    ?INFO("PUT req ~s: ~p: ~s", [Path, RespCode, ReqBody]),
    update_req(PathParts, iolist_to_binary(ReqBody)),

    Headers = #{<<"content-type">> => <<"application/json">>},

    Req2 = cowboy_req:reply(RespCode, Headers, <<"{}">>, Req1),
    {'ok', Req2, State}.

-spec read_body({'ok', binary(), cowboy_req:req()} |
                {'more', binary(), cowboy_req:req()}
               ) -> {cowboy_req:req(), iodata()}.
read_body({'ok', BodyPart, Req}) ->
    {Req, BodyPart};
read_body({'more', BodyPart, Req}) ->
    {Req1, Rest} = read_body(cowboy_req:read_body(Req)),
    {Req1, [BodyPart, Rest]}.

-spec maybe_handle_multipart(cowboy_req:req()) -> {cowboy_req:req(), iodata()}.
maybe_handle_multipart(Req) ->
    maybe_handle_multipart(Req, cowboy_req:parse_header(<<"content-type">>, Req)).

maybe_handle_multipart(Req, {<<"multipart">>, <<"form-data">>, _Boundary}) ->
    ?INFO("handle multipart body with boundary: ~p", [_Boundary]),
    handle_multipart(Req);
maybe_handle_multipart(Req, _CT) ->
    ?INFO("req has content-type: ~p", [_CT]),
    read_body(cowboy_req:read_body(Req)).

handle_multipart(Req0) ->
    case cowboy_req:read_part(Req0) of
        {'ok', Headers, Req1} ->
            ?INFO("recv part headers: ~p", [Headers]),
            handle_part_headers(Req1, Headers);
        {'done', Req1} ->
            ?INFO("finished reading parts, no body"),
            {Req1, <<>>}
    end.

handle_part_headers(Req, #{<<"content-type">> := <<"application/json">>}) ->
    ?INFO("skipping JSON metadata"),
    handle_multipart(Req);
handle_part_headers(Req, Headers) ->
    case cow_multipart:form_data(Headers) of
        {'data', Field} ->
            ?INFO("field: ~p", [Field]),
            {'ok', Body, Req1} = cowboy_req:read_part_body(Req),
            ?INFO("body: ~p", [Body]),
            {Req1, Body};
        {'file', _FieldName, _Filename, _CType} ->
            ?INFO("file ~p: ~p: ~p", [_FieldName, _Filename, _CType]),
            {'ok', Body, Req1} = cowboy_req:read_part_body(Req),
            ?INFO("body: ~p", [Body]),
            {Req1, Body}
    end.

-spec terminate(any(), cowboy_req:req(), any()) -> 'ok'.
terminate(_Reason, _Req, _State) ->
    ?INFO("finished req ~p", [kz_time:elapsed_ms(get('start_time'))]).

-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    stop_listener(),
    lager:debug("terminating: ~p", [_Reason]),
    lager:debug("state: ~p", [_State]).

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

-spec handle_call(any(), kz_term:pid_ref(), state()) ->
          {'noreply', state()} |
          {'reply', kz_json:api_json_term(), state()}.
handle_call({'wait_for_req', Path, TimeoutMs}
           ,From
           ,#state{requests=Requests
                  ,waits=Waits
                  }=State
           ) ->
    case kz_json:get_value(Path, Requests) of
        'undefined' ->
            {'noreply', State#state{waits=[new_wait(From, Path, TimeoutMs) | Waits]}};
        Value ->
            {'reply', Value, State}
    end;
handle_call('status', _From, State) ->
    {'reply', State, State};

handle_call({'fetch_req', Path, 'undefined'}, _From, #state{requests=Requests}=State) ->
    case kz_json:take_value(Path, Requests) of
        'false' ->
            ?INFO("failed to fetch ~p", [Path]),
            {'reply', 'undefined', State};
        {'value', Value, NewRequests} ->
            ?INFO("fetched ~p: ~s", [Path, Value]),
            {'reply', Value, State#state{requests=NewRequests}}
    end;
handle_call({'fetch_req', Path, TimeoutMs}
           ,From
           ,#state{requests=Requests
                  ,waits=Waits
                  }=State
           ) when is_integer(TimeoutMs) ->
    case kz_json:take_value(Path, Requests) of
        'false' ->
            {'noreply', State#state{waits=[new_wait(From, {Path, fun fetch_req/1}, TimeoutMs) | Waits]}};
        {'value', Value, NewRequests} ->
            ?INFO("fetched ~p: ~s", [Path, Value]),
            {'reply', Value, State#state{requests=NewRequests}}
    end;

handle_call({'get_req', Path}, _From, #state{requests=Requests}=State) ->
    ?INFO("getting ~p", [Path]),
    {'reply', kz_json:get_value(Path, Requests), State};

handle_call({'req', PathInfo, ReqBody}, _From, State) ->
    NewState = handle_req_update(PathInfo, ReqBody, State),
    {'reply', 'ok', NewState};
handle_call(_Req, _From, State) ->
    {'noreply', State}.

handle_req_update(PathInfo, ReqBody, #state{requests=Requests
                                           ,waits=Waits
                                           }=State) ->
    ?INFO("storing to ~p: ~s", [PathInfo, ReqBody]),
    UpdatedReqs = kz_json:set_value(PathInfo, ReqBody, Requests),

    {Relays, StillWaiting} =
        lists:splitwith(fun({_F, _T, {P, _Fun}}) -> lists:prefix(P, PathInfo);
                           ({_F, _T, P}) -> lists:prefix(P, PathInfo)
                        end
                       ,Waits
                       ),

    _ = relay(Relays, UpdatedReqs),

    State#state{requests=UpdatedReqs
               ,waits=StillWaiting
               }.

-spec handle_cast(any(), state()) -> {'noreply', state()}.
handle_cast({'req', PathInfo, ReqBody}, State) ->
    NewState = handle_req_update(PathInfo, ReqBody, State),
    {'noreply', NewState};
handle_cast(_Msg, State) ->
    {'noreply', State}.

-spec handle_info(any(), state()) -> {'noreply', state()}.
handle_info({'DOWN', MRef, 'process', Pid, _Reason}
           ,#state{waits=Waits}=State
           ) ->
    {'noreply', State#state{waits=[Wait || {{P, R}, _, _}=Wait <- Waits, P =/= Pid, R =/= MRef]}};
handle_info({'EXIT', Pid, _Reason}
           ,#state{waits=Waits}=State
           ) ->
    {'noreply', State#state{waits=[Wait || {{P, _R}, _, _}=Wait <- Waits, P =/= Pid]}};
handle_info({'timeout', TRef, {From, Path}}
           ,#state{waits=Waits}=State
           ) ->
    {Relays, StillWaiting}
        = lists:splitwith(fun({F, T, P}) -> F =:= From
                                                andalso T =:= TRef
                                                andalso P =:= Path
                          end
                         ,Waits
                         ),
    _ = relay(Relays, {'error', 'timeout'}),

    {'noreply', State#state{waits=StillWaiting}};
handle_info(_Msg, State) ->
    {'noreply', State}.

-spec relay(waits(), kz_json:object() | {'error', 'timeout'}) -> ['ok' | pid()].
relay(Relays, {'error', _}=Msg) ->
    [gen_server:reply(From, Msg) || {From, _, _} <- Relays];
relay(Relays, Requests) ->
    [begin
         _ = erlang:cancel_timer(TRef),
         reply(From, Path, Requests)
     end
     || {From, TRef, Path} <- Relays
    ].

reply(From, {Path, Fun}, _Requests) ->
    %% spawn a function that asks the server for the payload and replies to the waiting caller
    _ = spawn(fun() -> gen_server:reply(From, Fun(Path)) end);
reply(From, Path, Requests) ->
    gen_server:reply(From, kz_json:get_value(Path, Requests)).

-spec new_wait(kz_term:pid_ref(), kz_json:path() | {kz_json:path(), fun()}, pos_integer()) -> wait().
new_wait(From, Path, TimeoutMs) ->
    TRef = erlang:start_timer(TimeoutMs, self(), {From, Path}),
    {From, TRef, Path}.
