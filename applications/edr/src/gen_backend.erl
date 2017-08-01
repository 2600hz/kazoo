%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%% Behaviour of backend modules
%%% @end
%%% @contributors
%%%    SIPLABS, LLC (Vorontsov Nikita) <info@siplabs.ru>
%%%-------------------------------------------------------------------
-module(gen_backend).

-behaviour(gen_server).

-include("edr.hrl").

-export([push/2
        ,stop/1
        ]).

-export([formatter/2
        ,formatter_options/1
        ]).

%% gen_server callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ,start_link/3
        ]).

-record(state, {module_state :: any()
               ,tags :: kz_json:object()
               ,module :: module()
               ,name :: ne_binary()
               }).
-type state() :: #state{}.

%%%-------------------------------------------------------------------
%%% Callbacks
%%%-------------------------------------------------------------------
-callback(init(backend())-> init_ret(ModuleState :: any())).
-callback(push(ModuleState :: any(), Event :: event()) -> work_result()).
-callback(stop(ModuleState :: any(), Reason :: any()) -> 'ok').
-callback(async_response_handler(Response :: any()) -> work_result()).
%%-------------------------------------------------------------------
%%
%%-------------------------------------------------------------------
-spec start_link(module(), backend(), any()) -> startlink_ret().
start_link(Module, #backend{}=Args, Opts)->
    gen_server:start_link(?MODULE, {Module, Args}, Opts);
start_link(_Module,_Other,_Opts) ->
    lager:error("not started: bad backend format: ~p", [_Other]),
    'ignore'.

-spec init({module(), backend()})-> init_ret(state()).
init({Mod, #backend{enabled='true', tags=Tags, name=Name} = Backend})->
    case Mod:init(Backend) of
        {'ok', ModState} -> {'ok', #state{module_state=ModState, tags=Tags, module=Mod, name=Name}};
        _ -> 'ignore'
    end;
init(_Other) ->
    'ignore'.

-spec push(pid(), event())-> 'ok'.
push(Pid, Event)->
    gen_server:cast(Pid, {'push', Event}).

-spec stop(pid())-> 'ok'.
stop(Pid)->
    gen_server:cast(Pid, 'stop').

-spec formatter(kz_json:object(), module()) -> module().
formatter(Options, Default)->
    case kz_json:get_value([<<"formatter">>, <<"type">>], Options) of
        'undefined' -> Default;
        Type -> kz_term:to_atom("edr_fmt_" ++ binary_to_list(Type))
    end.

-spec formatter_options(kz_json:object()) -> kz_json:object().
formatter_options(Options)->
    kz_json:get_value([<<"formatter">>, <<"options">>], Options, kz_json:new()).

%%--------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}
%%--------------------------------------------------------------------
-spec handle_call(any(), any(), state()) -> handle_call_ret_state(state()).
handle_call(_Request, _From, _State) ->
    lager:debug("unhandled call ~p", [_Request]),
    {'stop', {'error', 'not implemented'}, _State}.

%%--------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}
%%--------------------------------------------------------------------
-spec handle_cast(any(), state()) -> {'noreply', state()}.
handle_cast({'push', Event=#event{}}, State=#state{module_state=ModState, tags=_Tags, module=Mod}) ->
    WorkResult = case should_push(State, Event) of
                     'true' -> Mod:push(ModState, Event);
                     _False -> 'ok'
                 end,
    case WorkResult of
        {'exit', Reason}-> {'stop', Reason, State};
        {'error',Reason}-> {'noreply', handle_error(Reason, State)};
        _Ok -> {'noreply', State}
    end;
handle_cast('stop', _State) ->
    {'stop', 'normal'};
handle_cast(_Data, _State) ->
    lager:debug("unhandled handle_cast event"),
    {'noreply', _State}.

%%--------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}
%%--------------------------------------------------------------------
-spec handle_info(any(), state()) -> {'noreply', state()}.
handle_info(Info, #state{module=Mod}=State) ->
    case Mod:async_response_handler(Info) of
        {'exit', Reason}-> {'stop', Reason, State};
        {'error',Reason}-> {'noreply', handle_error(Reason, State)};
        _Ok -> {'noreply', State}
    end.

%%--------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(Reason, #state{module_state=ModState, module=Mod}) ->
    Mod:stop_backend(ModState, Reason),
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', any()} | {'error', any()}.
code_change(_OldVsn, _State, _Extra) ->
    {'ok', _State}.

%% ------------------------------------------------------------------
-spec handle_error(any(), state()) -> state().
handle_error(Reason, #state{name=N, module=M} = State)->
    lager:debug("error in ~s|~s|~p: ~s", [N, M, node(), Reason]),
    State.

%%------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------
%% Always push
should_push(_, _)-> 'true'.
