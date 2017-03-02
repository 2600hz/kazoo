-module(teletype_bindings).
-behaviour(gen_server).

-export([start_link/0]).
-export([bind/3, bind/4
        ,flush_mod/1
        ,notification/1
        ]).

% gen_server callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include("teletype.hrl").

-define(SERVER, ?MODULE).

-type event() :: {api_binary(), api_binary()}.
-type responder() :: {module(), atom()}.
%% Set would be better suited, but lists are more convenient
-type responders() :: [responder()].

-record(state, {bindings=#{} :: #{event() => responders()}}).
-type state() :: #state{}.

-spec start_link() -> startlink_ret().
start_link() ->
    Ret = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    start_modules(),
    Ret.

-spec bind(ne_binary(), module(), atom()) -> 'ok'.
-spec bind(api_binary(), api_binary(), module(), atom()) -> 'ok'.
bind(EventName, Module, Fun) ->
    bind(<<"notification">>, EventName, Module, Fun).
bind(EventCategory, EventName, Module, Fun) ->
    lager:debug("binding event {~s, ~s} to {~s, ~s}", [EventCategory, EventName, Module, Fun]),
    gen_server:call(?SERVER, {'bind', {EventCategory, EventName}, {Module, Fun}}).

-spec flush_mod(module()) -> 'ok'.
flush_mod(Module) ->
    lager:debug("flushing module ~s", [Module]),
    gen_server:call(?SERVER, {'flush_mod', Module}).

-spec notification(kz_json:object()) -> 'ok'.
notification(JObj) ->
    {EventCategory, EventName} = Event = kz_util:get_event_type(JObj),
    lager:debug("dispatching notification {~s, ~s}", [EventCategory, EventName]),
    gen_server:call(?SERVER, {'notification', Event, JObj}).

%% gen_server callbacks
-spec init([]) -> {'ok', state()}.
init([]) ->
    {'ok', #state{}}.

-spec handle_call({'bind', event(), responder()}, pid_ref(), state()) -> handle_call_ret_state(state());
                 ({'flush_mod', module()}, pid_ref(), state()) -> handle_call_ret_state(state());
                 ({'notification', event(), kz_json:object()}, pid_ref(), state()) -> handle_call_ret_state(state()).
handle_call({'bind', Event, Responder}, _From, #state{bindings=BindMap}=State) ->
    CurrentResponders = maps:get(Event, BindMap, []),
    %% Prevent duplicate bindings
    NewResponders = case lists:member(Responder, CurrentResponders) of
                         'true' -> CurrentResponders;
                         'false' -> [Responder | CurrentResponders]
                    end,
    {'reply', 'ok', State#state{bindings=BindMap#{Event => NewResponders}}};
handle_call({'flush_mod', Module}, _From, #state{bindings=BindMap}=State) ->
    %% Slow, but will be rarely called, and binding structure should be small.
    NewBindMap = maps:fold(fun(K, V, Map) ->
                               Map#{K => [B || {Mod, _}=B <- V, Mod =/= Module]}
                           end, BindMap, BindMap),
    {'reply', 'ok', State#state{bindings=NewBindMap}};
handle_call({'notification', Event, JObj}, _From, #state{bindings=BindMap}=State) ->
    dispatch_notification(JObj, Event, maps:get(Event, BindMap, [])),
    {'reply', 'ok', State};
handle_call(_Request, _From, State) ->
    {'reply', 'ignored', State}.

-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
handle_cast(_Msg, State) ->
    {'noreply', State}.

-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info(_Info, State) ->
    {'noreply', State}.

-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    'ok'.

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%% Internal functions
-spec dispatch_notification(kz_json:object(), event(), responders()) -> 'ok'.
dispatch_notification(JObj, {EventCat, EventName}=Event, [{Module, Fun} | Remaining]) ->
    lager:debug("sending {~s, ~s} notification to fun ~s:~s/1", [EventCat, EventName, Module, Fun]),
    erlang:spawn(Module, Fun, [JObj]),
    dispatch_notification(JObj, Event, Remaining);
dispatch_notification(_JObj, {EventCategory, EventName}, []) ->
    lager:debug("finished dispatching notification {~s, ~s}", [EventCategory, EventName]),
    'ok'.

-spec start_modules() -> 'ok'.
start_modules() ->
    start_modules(?AUTOLOAD_MODULES).

-spec start_modules([module()]) -> 'ok'.
start_modules([Module | Remaining]) ->
    teletype_maintenance:start_module(Module),
    start_modules(Remaining);
start_modules([]) ->
    lager:info("started all teletype modules").
