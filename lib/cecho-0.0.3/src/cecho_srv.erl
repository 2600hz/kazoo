%%==============================================================================
%% Copyright 2010 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-module(cecho_srv).

-author('mazen.harake@erlang-solutions.com').

-behaviour(gen_server).
-include("cecho.hrl").
-include("cecho_commands.hrl").

%% Behaviour Callbacks
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3]).

%% Module API
-export([start_link/0, call/2, getch/0]).

%% Records
-record(state, { port, getch, observer }).

%% =============================================================================
%% Module API
%% =============================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

call(Cmd, Args) ->
    gen_server:call(?MODULE, {call, Cmd, Args}, infinity).

getch() ->
    gen_server:call(?MODULE, getch, infinity).

%% =============================================================================
%% Behaviour Callbacks
%% =============================================================================
init(no_args) ->
    process_flag(trap_exit, true),
    case load_driver() of
	ok ->
	    Port = erlang:open_port({spawn, "cecho"}, [binary]),
	    ok = do_call(Port, ?INITSCR),
	    ok = do_call(Port, ?WERASE, 0),
	    ok = do_call(Port, ?REFRESH),
	    {ok, #state{ port = Port }};
	{error, ErrorCode} ->
	    exit({driver_error, erl_ddll:format_error(ErrorCode)})
    end.

handle_call({call, Cmd, Args}, _From, State) ->
    {reply, do_call(State#state.port, Cmd, Args), State};
handle_call(getch, From, #state{ getch = undefined } = State) ->
    {noreply, State#state{ getch = From }};
handle_call(getch, _From, State) ->
    {reply, -1, State}.

terminate(_Reason, State) ->
    do_call(State#state.port, ?ENDWIN),
    do_call(State#state.port, ?CURS_SET, ?ceCURS_NORMAL),
    erlang:port_close(State#state.port),
    erl_ddll:unload("cecho").

handle_info({_Port, {data, _Binary}}, #state{ getch = undefined } = State) ->
    {noreply, State};
handle_info({_Port, {data, Binary}}, State) ->
    gen_server:reply(State#state.getch, binary_to_term(Binary)),
    {noreply, State#state{ getch = undefined }}.

%% @hidden
handle_cast(_, State) ->
    {noreply, State}.

%% @hidden
code_change(_, State, _) ->
    {noreply, State}.

%% =============================================================================
%% Internal Functions
%% =============================================================================
do_call(Port, Cmd) ->
    do_call(Port, Cmd, undefined).

do_call(Port, Cmd, Args) ->
    binary_to_term(erlang:port_control(Port, Cmd, term_to_binary(Args))).

load_driver() ->
    Dir = case code:priv_dir(cecho) of
              {error, bad_name} ->
                  filename:dirname(code:which(?MODULE)) ++ "/../priv";
              D ->
                  D
          end,
    erl_ddll:load(Dir, "cecho").
