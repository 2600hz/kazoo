%% Copyright (c) 2010 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.

%% @doc Small server to monitor the riak_err custom SASL event handler.

-module(riak_err_monitor).

-behaviour(gen_server).

-define(NAME, ?MODULE).
-define(Timeout, infinity).

%% External exports
-export([start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {
          max_len = 20*1024 :: pos_integer(),
          tref              :: timer:tref()
         }).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
%% @doc Starts the monitor
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?NAME}, ?MODULE, [], []).

%% @doc Stops the monitor
-spec stop() -> ok.
stop() ->
    gen_event:call(?NAME, stop, infinity).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%% @hidden
-spec init([]) -> {ok, #state{}}.
init([]) ->
    %% Add our custom handler.
    ok = riak_err_handler:add_sup_handler(),

    %% Disable the default error logger handlers and SASL handlers.
    ok = lists:foreach(
           fun(Handler) ->
                   gen_event:delete_handler(error_logger, Handler, {stop_please, ?MODULE})
           end,
           [error_logger, error_logger_tty_h, sasl_report_tty_h, sasl_report_file_h]),
    
    {ok, TRef} = timer:send_interval(1000, reopen_log_file),
    {ok, #state{tref = TRef}}.

%% @hidden
-spec handle_call(stop | term(), reference(), #state{}) -> {stop, normal, ok, #state{}} | {reply, not_implemented, #state{}}.
handle_call(stop, _From, State) ->
  {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
  {reply, not_implemented, State}.

%% @hidden
-spec handle_cast(tuple(), #state{}) -> {noreply, #state{}}.
handle_cast(Msg, State) ->
    {Str, _} = trunc_io:print(Msg, State#state.max_len),
    error_logger:error_msg("~w: ~s:handle_cast got ~s\n",
                           [self(), ?MODULE, Str]),
    {noreply, State}.

%% @hidden
-spec handle_info(reopen_log_file | {gen_event_EXIT, ?MODULE, term()} | tuple(), #state{}) -> {noreply, #state{}} | {stop, gen_event_EXIT, #state{}}.
handle_info(reopen_log_file, State) ->
    ok = riak_err_handler:reopen_log_file(),
    {noreply, State};
handle_info({gen_event_EXIT, Handler, Reason}, State) ->
    %% Our handler ought to be bullet-proof ... but it wasn't, bummer.
    %% Double bummer, we cannot use the handler to log this event.
    %%
    %% We will stop now, and our supervisor will restart us and thus
    %% reinstate the custom event handler.  If all goes well, we will
    %% be restarted after only a few milliseconds.

    {Str, _} = trunc_io:print(Reason, State#state.max_len),
    io:format("~w: ~s: handler ~w exited for reason ~s\n",
              [self(), ?MODULE, Handler, Str]),
    {stop, gen_event_EXIT, State};
handle_info(Info, State) ->
    {Str, _} = trunc_io:print(Info, State#state.max_len),
    error_logger:error_msg("~w: ~s:handle_info got ~s\n", 
                           [self(), ?MODULE, Str]),
    {noreply, State}.

%% @hidden
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @hidden
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.