-module(pm_google).
-behaviour(gen_server).

-include("../pusher.hrl").
-include("../gen_server_spec.hrl").

-export([start_link/0]).

-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-record(state, {tab :: ets:tid()}).

-spec start_link() -> startlink_ret().
start_link() ->
  gen_server:start_link({'local', ?MODULE}, ?MODULE, [],[]).

init([]) ->
    put('callid', ?MODULE),
    {'ok', #state{tab=ets:new(?MODULE, [])}}.

handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

handle_cast({'push', JObj}, #state{tab=ETS}=State) ->
    TokenApp = wh_json:get_value(<<"Token-App">>, JObj),
    maybe_send_push_notification(get_gcm(TokenApp, ETS), JObj),
    {'noreply', State};
handle_cast('stop', State) ->
    {'stop', 'normal', State}.

handle_info(_Request, State) ->
    {'noreply', State}.

terminate(_Reason, #state{tab=ETS}) ->
    ets:delete(ETS),
    'ok'.

code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

-spec maybe_send_push_notification(api_pid(), wh_json:object()) -> any().
maybe_send_push_notification('undefined', _JObj) -> 'ok';
maybe_send_push_notification(Pid, JObj) ->
    TokenID = wh_json:get_value(<<"Token-ID">>, JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    Message = wh_json:from_list([{<<"data">>
                                      ,wh_json:from_list([{<<"Call-ID">>, CallId}])
                                 }
                                ]),
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
