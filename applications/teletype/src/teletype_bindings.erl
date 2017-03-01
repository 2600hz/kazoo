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

-record(state, {bindings=#{}}).
-type state() :: #state{}.

-spec start_link() -> startlink_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec bind(ne_binary(), module(), atom()) -> 'ok'.
-spec bind(ne_binary(), ne_binary(), module(), atom()) -> 'ok'.
bind(EventName, Module, Fun) ->
    bind(<<"notification">>, EventName, Module, Fun).
bind(EventCategory, EventName, Module, Fun) ->
    gen_server:call(?SERVER, {'bind', {EventCategory, EventName}, {Module, Fun}}).

-spec flush_mod(module()) -> 'ok'.
flush_mod(Module) ->
    gen_server:call(?SERVER, {'flush_mod', Module}).

-spec notification(kz_json:object()) -> 'ok'.
notification(JObj) ->
    gen_server:call(?SERVER, {'notification', JObj}).

%% gen_server callbacks
-spec init([]) -> {'ok', state()}.
init([]) ->
    {'ok', #state{}}.

-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
handle_call({'bind', Event, Binding}, _From, #state{bindings=BindMap}=State) ->
    %% Need to handle duplicate bindings. Sets don't have comprehensions though!
    NewEventBinding = [Binding | maps:get(Event, BindMap, [])],
    {'reply', 'ok', State#state{bindings=BindMap#{Event => NewEventBinding}}};
handle_call({'flush_mod', Module}, _From, #state{bindings=BindMap}=State) ->
    %% Slow, but will be rarely called, and structure should be small.
    NewBindMap = maps:fold(fun(K, V, Map) ->
                               Map#{K => [B || {Mod, _}=B <- V, Mod =/= Module]}
                            end, BindMap, BindMap),
    {'reply', 'ok', State#state{bindings=NewBindMap}};
handle_call({'notification', JObj}, _From, #state{bindings=BindMap}=State) ->
    Event = kz_util:get_event_type(JObj),
    dispatch_notification(JObj, maps:get(Event, BindMap, [])),
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
dispatch_notification(JObj, [{Module, Fun} | Remaining]) ->
    erlang:spawn(Module, Fun, [JObj]),
    dispatch_notification(JObj, Remaining);
dispatch_notification(_JObj, []) ->
    'ok'.
