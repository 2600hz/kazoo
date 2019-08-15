%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Behaviour of backend modules
%%% @author SIPLABS, LLC (Vorontsov Nikita) <info@siplabs.ru>
%%% @author Conversant Ltd (Max Lay)
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(gen_edr_backend).

-behaviour(gen_server).

-include("edr.hrl").

-export([push/2
        ,stop/1
        ]).

%% gen_server callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ,start_link/2
        ]).

-record(state, {module_state :: any()
               ,module :: module()
               ,name :: kz_term:ne_binary()
               }).
-type state() :: #state{}.

%%%-----------------------------------------------------------------------------
%%% Callbacks
%%%-----------------------------------------------------------------------------
-callback(init(backend())-> init_ret(ModuleState :: any())).
-callback(push(ModuleState :: any(), Event :: edr_event()) -> work_result()).
-callback(stop(ModuleState :: any(), Reason :: any()) -> 'ok').
-callback(async_response_handler(Response :: any()) -> work_result()).
%%------------------------------------------------------------------------------
%%
%%------------------------------------------------------------------------------
-spec start_link(module(), backend()) -> kz_types:startlink_ret().
start_link(Module, #backend{}=Backend)->
    gen_server:start_link(?MODULE, {Module, Backend}, []);
start_link(_Module, _Other) ->
    lager:error("not started: bad backend format: ~p", [_Other]),
    'ignore'.

-spec init({module(), backend()})-> init_ret(state()).
init({Mod, #backend{name=Name, bindings=Bindings}=Backend})->
    case Mod:init(Backend) of
        {'ok', ModState} ->
            _ = edr_bindings:bind(Bindings, ?MODULE, 'push', self()),
            {'ok', #state{module_state=ModState, module=Mod, name=Name}};
        _ ->
            'ignore'
    end;
init(_Other) ->
    lager:error("init ignore ~p", [_Other]),
    'ignore'.

-spec push(pid(), edr_event())-> 'ok'.
push(Pid, Event)->
    gen_server:cast(Pid, {'push', Event}).

-spec stop(pid())-> 'ok'.
stop(Pid)->
    gen_server:cast(Pid, 'stop').

%%------------------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}
%%------------------------------------------------------------------------------
-spec handle_call(any(), any(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, _State) ->
    lager:debug("unhandled call ~p", [_Request]),
    {'stop', {'error', 'not implemented'}, _State}.

%%------------------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> {'noreply', state()}.
handle_cast({'push', Event=#edr_event{}}, State=#state{module_state=ModState, module=Mod}) ->
    case Mod:push(ModState, Event) of
        {'exit', Reason}-> {'stop', Reason, State};
        {'error',Reason}-> {'noreply', handle_error(Reason, State)};
        _Ok -> {'noreply', State}
    end;
handle_cast('stop', _State) ->
    {'stop', 'normal'};
handle_cast(_Data, _State) ->
    lager:debug("unhandled handle_cast event"),
    {'noreply', _State}.

%%------------------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> {'noreply', state()}.
handle_info(Info, #state{module=Mod}=State) ->
    case Mod:async_response_handler(Info) of
        {'exit', Reason}-> {'stop', Reason, State};
        {'error',Reason}-> {'noreply', handle_error(Reason, State)};
        _Ok -> {'noreply', State}
    end.

%%------------------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(Reason, #state{module_state=ModState, module=Mod}) ->
    Mod:stop(ModState, Reason),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', any()} | {'error', any()}.
code_change(_OldVsn, _State, _Extra) ->
    {'ok', _State}.

%%------------------------------------------------------------------------------
-spec handle_error(any(), state()) -> state().
handle_error(Reason, #state{name=N, module=M} = State)->
    lager:debug("error in ~s|~s|~p: ~s", [N, M, node(), Reason]),
    State.
