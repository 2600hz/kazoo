-module(pm_apple).
-behaviour(gen_server).

-include("pusher.hrl").

-define(SERVER, ?MODULE).

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
    gen_server:start_link({'local', ?SERVER}, ?MODULE, [],[]).

-spec init([]) -> {'ok', state()}.
init([]) ->
    kz_util:put_callid(?MODULE),
    {'ok', #state{tab=ets:new(?MODULE, [])}}.

-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
handle_cast({'push', JObj}, #state{tab=ETS}=State) ->
    TokenApp = kz_json:get_value(<<"Token-App">>, JObj),
    maybe_send_push_notification(get_apns(TokenApp, ETS), JObj),
    {'noreply', State};
handle_cast('stop', State) ->
    {'stop', 'normal', State}.

-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info(_Request, State) ->
    {'noreply', State}.

-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{tab=ETS}) ->
    ets:delete(ETS),
    'ok'.

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

-spec maybe_send_push_notification(api_pid(), kz_json:object()) -> any().
maybe_send_push_notification('undefined', _) -> 'ok';
maybe_send_push_notification(Pid, JObj) ->
    TokenID = kz_json:get_value(<<"Token-ID">>, JObj),
    Sender = kz_json:get_value(<<"Alert-Body">>, JObj),
    CallId = kz_json:get_value(<<"Call-ID">>, JObj),
    APNsTopic = apns_topic(JObj),
    apns:push_notification(Pid
                          ,TokenID
                          ,#{aps => #{alert => #{'loc-key' => <<"IC_MSG">>
                                                ,'loc-args' => [Sender]
                                                }
                                     ,sound => <<"ring.caf">>
                                     }
                            ,'call-id' => CallId
                            }
                          ,#{apns_topic => APNsTopic}
                          ).

-spec get_apns(api_binary(), ets:tid()) -> api_pid().
get_apns('undefined', _) -> 'undefined';
get_apns(App, ETS) ->
    case ets:lookup(ETS, App) of
        [] -> maybe_load_apns(App, ETS);
        [{App, Pid}] -> Pid
    end.

-spec maybe_load_apns(api_binary(), ets:tid()) -> api_pid().
maybe_load_apns(App, ETS) ->
    CertBin = kapps_config:get_ne_binary(?CONFIG_CAT, [<<"apple">>, <<"certificate">>], 'undefined', App),
    Host = kapps_config:get_ne_binary(?CONFIG_CAT, [<<"apple">>, <<"host">>], ?DEFAULT_APNS_HOST, App),
    maybe_load_apns(App, ETS, CertBin, Host).

-spec maybe_load_apns(api_binary(), ets:tid(), api_ne_binary(), ne_binary()) -> api_pid().
maybe_load_apns(App, _, 'undefined', _) ->
    lager:debug("apple pusher certificate for app ~s not found", [App]),
    'undefined';
maybe_load_apns(App, ETS, CertBin, Host) ->
    {Key, Cert} = pusher_util:binary_to_keycert(CertBin),
    Connection = #{name => 'undefined'
                  ,apple_host => kz_term:to_list(Host)
                  ,apple_port => 443
                  ,certdata => Cert
                  ,keydata => Key
                  ,timeout => 10000
                  ,type => 'certdata'
                  },
    case apns:connect(Connection) of
        {'ok', Pid} ->
            ets:insert(ETS, {App, Pid}),
            Pid;
        {'error', {'already_started', Pid}} ->
            ets:insert(ETS, {App, Pid}),
            Pid;
        {'error', Reason} ->
            lager:error("Error loading apns ~p", [Reason]),
            'undefined'
    end.

-spec apns_topic(kz_json:object()) -> binary().
apns_topic(JObj) ->
    TokenApp = kz_json:get_ne_binary_value(<<"Token-App">>, JObj),
    TokenType = kz_json:get_ne_binary_value(<<"Token-Type">>, JObj),
    case kapps_config:get_ne_binary(<<"pusher">>
                                   ,[TokenType, <<"apns_topic">>]
                                   ,'undefined'
                                   ,TokenApp
                                   )
    of
        'undefined' -> default_apns_topic(TokenApp);
        APNsTopic -> APNsTopic
    end.

%% Retains the old behaviour
-spec default_apns_topic(ne_binary()) -> ne_binary().
default_apns_topic(TokenApp) ->
    re:replace(TokenApp, <<"\\.(?:dev|prod)$">>, <<>>, [{'return', 'binary'}]).
