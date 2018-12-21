-module(pqc_httpd).
-behaviour(cowboy_handler).
-behaviour(gen_server).

-export([fetch_req/1
        ,get_req/1
        ,base_url/0
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

%% {[URIPath], Body}
-type state() :: kz_json:object().

-spec start_link() -> {'ok', pid()}.
start_link() ->
    start_link(kz_binary:rand_hex(5)).

-spec start_link(kz_term:ne_binary()) -> {'ok', pid()}.
start_link(LogId) ->
    gen_server:start_link({'local', ?MODULE}, ?MODULE, [LogId], []).

-spec stop() -> 'ok'.
stop() ->
    case whereis(?MODULE) of
        'undefined' -> stop_listener();
        Pid -> gen_server:stop(Pid)
    end.

-spec stop_listener() -> 'ok'.
stop_listener() ->
    cowboy:stop_listener(?LISTENER).

-spec fetch_req(kz_json:path()) -> kz_json:api_json_term().
fetch_req(Path) ->
    gen_server:call(?MODULE, {'fetch_req', Path}).

-spec get_req(kz_json:path()) -> kz_json:api_json_term().
get_req(Path) ->
    gen_server:call(?MODULE, {'get_req', Path}).

log_meta(LogId) ->
    kz_util:put_callid(LogId),
    lager:md([{'request_id', LogId}]),
    put('now', kz_time:now()),
    'ok'.

-spec init(list()) -> {'ok', state()}.
init([LogId]) ->
    log_meta(LogId),
    io:format("starting HTTPD with ~p~n", [LogId]),

    Dispatch = cowboy_router:compile(routes(LogId)),
    {'ok', _Pid} = start_plaintext(Dispatch),
    ?INFO("started HTTPD(~p) at ~s", [_Pid, base_url()]),
    {'ok', kz_json:new()}.

-spec routes(kz_term:ne_binary()) -> cowboy_router:routes().
routes(LogId) -> [{'_', paths_list(LogId)}].

paths_list(LogId) ->
    [default_path(LogId)].

default_path(LogId) ->
    {'_', 'pqc_httpd', [{'log_id', LogId}]}.

start_plaintext(Dispatch) ->
    cowboy:start_clear(?LISTENER
                      ,[{'num_acceptors', 5}]
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

    {RespCode, RespBody} =
        case get_req(PathParts) of
            'undefined' -> {404, <<>>};
            Value -> {200, Value}
        end,

    ?INFO("GET req ~s: ~p", [Path, RespCode]),

    Req1 = cowboy_req:reply(RespCode, #{}, RespBody, Req),
    {'ok', Req1, State}.

add_req_to_state(Req, State) ->
    Path = cowboy_req:path(Req), % <<"/foo/bar/baz">>
    PathParts = tl(binary:split(Path, <<"/">>, ['global'])),
    ReqBody = read_body(cowboy_req:read_body(Req)),

    RespCode = case get_req(PathParts) of
                   'undefined' -> 201;
                   _Value -> 200
               end,

    ?INFO("PUT req ~s: ~p: ~s", [Path, RespCode, ReqBody]),
    gen_server:cast(?MODULE, {'req', PathParts, iolist_to_binary(ReqBody)}),

    Headers = #{<<"content-type">> => <<"application/json">>},

    Req1 = cowboy_req:reply(RespCode, Headers, <<"{}">>, Req),
    {'ok', Req1, State}.

-spec read_body({'ok', binary(), cowboy_req:req()} |
                {'more', binary(), cowboy_req:req()}
               ) -> iodata().
read_body({'ok', BodyPart, _Req}) ->
    BodyPart;
read_body({'more', BodyPart, Req}) ->
    [BodyPart, read_body(cowboy_req:read_body(Req))].

-spec terminate(any(), cowboy_req:req(), any()) -> 'ok'.
terminate(_Reason, _Req, _State) -> 'ok'.

-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    stop_listener(),
    lager:debug("terminating: ~p", [_Reason]).

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

-spec handle_call(any(), kz_term:pid_ref(), state()) ->
                         {'noreply', state()} |
                         {'reply', kz_json:api_json_term(), state()}.
handle_call({'fetch_req', Path}, _From, State) ->
    case kz_json:take_value(Path, State) of
        'false' -> {'reply', 'undefined', State};
        {'value', Value, NewState} -> {'reply', Value, NewState}
    end;
handle_call({'get_req', Path}, _From, State) ->
    {'reply', kz_json:get_value(Path, State), State};
handle_call(_Req, _From, State) ->
    {'noreply', State}.

-spec handle_cast(any(), state()) -> {'noreply', state()}.
handle_cast({'req', PathInfo, ReqBody}, State) ->
    {'noreply', kz_json:set_value(PathInfo, ReqBody, State)};
handle_cast(_Msg, State) ->
    {'noreply', State}.

-spec handle_info(any(), state()) -> {'noreply', state()}.
handle_info(_Msg, State) ->
    {'noreply', State}.
