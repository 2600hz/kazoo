-module(pm_google).
-behaviour(gen_server).

-include("../pusher.hrl").

-export([start_link/0]).

-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-record(state, {tab :: ets:tid()}).
-type state() :: #state{}.

-spec start_link() -> startlink_ret().
start_link() ->
  gen_server:start_link({'local', ?MODULE}, ?MODULE, [],[]).

-spec init([]) -> {'ok', state()}.
init([]) ->
    wh_util:put_callid(?MODULE),
    lager:debug("starting server"),
    {'ok', #state{tab=ets:new(?MODULE, [])}}.

-spec handle_call(term(), pid_ref(), state()) -> handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

-spec handle_cast(term(), state()) -> handle_cast_ret_state(state()).
handle_cast({'push', JObj}, #state{tab=ETS}=State) ->
    lager:debug("process a push"),
    TokenApp = wh_json:get_value(<<"Token-App">>, JObj),
    maybe_send_push_notification(get_gcm(TokenApp, ETS), JObj),
    {'noreply', State};
handle_cast('stop', State) ->
    {'stop', 'normal', State}.

-spec handle_info(term(), state()) -> handle_info_ret_state(state()).
handle_info(_Request, State) ->
    {'noreply', State}.

-spec terminate(term(), state()) -> 'ok'.
terminate(_Reason, #state{tab=ETS}) ->
    ets:delete(ETS),
    'ok'.

-spec code_change(term(), state(), term()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

-spec maybe_send_push_notification(api_pid(), wh_json:object()) -> any().
maybe_send_push_notification('undefined', _JObj) -> lager:debug("no pid to send push");
maybe_send_push_notification(Pid, JObj) ->
    TokenID = wh_json:get_value(<<"Token-ID">>, JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    Message = wh_json:from_list([{<<"data">>,wh_json:from_list([{<<"Call-ID">>, CallId}])}]),

    lager:debug("pushing to ~p: ~s: ~p", [Pid, TokenID, Message]),

    gcm:push(Pid, [TokenID], Message).

-spec get_gcm(api_binary(), ets:tid()) -> api_pid().
get_gcm('undefined', _) -> 'undefined';
get_gcm(App, ETS) ->
    case ets:lookup(ETS, App) of
        [] -> maybe_load_gcm(App, ETS);
        [{App, Pid}] -> Pid
    end.

-spec maybe_load_gcm(api_binary(), ets:tid()) -> api_pid().
maybe_load_gcm(App, ETS) ->
    lager:debug("loading gcm secret for ~s", [App]),
    maybe_load_gcm(App, ETS, whapps_config:get_binary(?CONFIG_CAT, <<"google">>, 'undefined', App)).

-spec maybe_load_gcm(api_binary(), ets:tid(), api_binary()) -> api_pid().
maybe_load_gcm(App, _, 'undefined') ->
    lager:debug("google pusher secret for app ~s not found", [App]),
    'undefined';
maybe_load_gcm(App, ETS, Secret) ->
    case gcm:start(wh_util:to_atom(App, 'true'), Secret) of
        {'ok', Pid} ->
            ets:insert(ETS, {App, Pid}),
            Pid;
        {'error', {'already_started', Pid}} ->
            ets:insert(ETS, {App, Pid}),
            Pid;
        {'error', Reason} ->
            lager:error("Error loading gcm ~p", [Reason]),
            'undefined'
    end.
