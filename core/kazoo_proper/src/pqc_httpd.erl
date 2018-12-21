-module(pqc_httpd).
-behaviour(cowboy_handler).
-behaviour(gen_server).

-export([fetch_req/1
        ,base_url/0
        ,stop/0
        ]).

%% gen_server
-export([start_link/0
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
    gen_server:start_link({'local', ?MODULE}, ?MODULE, [], []).

-spec stop() -> 'ok'.
stop() ->
    cowboy:stop_listener(?LISTENER),
    gen_server:stop(?MODULE).

-spec fetch_req(kz_json:path()) -> kz_json:api_json_term().
fetch_req(Path) ->
    gen_server:call(?MODULE, {'fetch_req', Path}).

-spec init([]) -> {'ok', state()}.
init(_) ->
    kz_util:put_callid(<<?MODULE_STRING>>),
    Dispatch = cowboy_router:compile(routes()),
    {'ok', _} = start_plaintext(Dispatch),
    ?INFO("started HTTPD at ~s", [base_url()]),
    {'ok', kz_json:new()}.

-spec routes() -> cowboy_router:routes().
routes() -> [{'_', paths_list()}].

paths_list() ->
    [default_path()].

default_path() ->
    {'_', 'pqc_httpd', []}.

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
    handle(Req, HandlerOpts).

-spec handle(cowboy_req:req(), State) -> {'ok', cowboy_req:req(), State}.
handle(Req, State) ->
    Path = cowboy_req:path(Req), % <<"/foo/bar/baz">>
    PathParts = tl(binary:split(Path, <<"/">>, ['global'])),
    ReqBody = read_body(cowboy_req:read_body(Req)),

    gen_server:cast(?MODULE, {'req', PathParts, iolist_to_binary(ReqBody)}),
    io:format("req ~p: ~s~n", [PathParts, ReqBody]),

    Headers = #{<<"content-type">> => <<"application/json">>},

    Req1 = cowboy_req:reply(200, Headers, <<"{}">>, Req),
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
    lager:debug("terminating: ~p", [_Reason]).

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

-spec handle_call(any(), kz_term:pid_ref(), state()) ->
                         {'noreply', state()} |
                         {'reply', kz_json:api_json_term(), state()}.
handle_call({'fetch_req', Path}, _From, State) ->
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
