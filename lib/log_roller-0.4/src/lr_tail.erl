%% Copyright (c) 2009 Jacob Vorreuter <jacob.vorreuter@gmail.com>
%% 
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%% 
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
-module(lr_tail).
-author('jacob.vorreuter@gmail.com').
-behaviour(gen_server).

-include("log_roller.hrl").

-record(buffer, {log_name, logs}).
-record(state, {buffers=[], responses=[]}).

-define(TIMER_VALUE, 2000).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, 
		 handle_info/2, terminate/2, code_change/3]).

%% API exports
-export([add_response/3, get_responses/0, send_log/1]).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_response(LogName, Opts, Response) ->
	gen_server:call(?MODULE, {add_response, LogName, Opts, Response}).
	
get_responses() ->
    gen_server:call(?MODULE, get_responses).
    
send_log(Log) ->
    gen_server:abcast(?MODULE, Log).
    
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%% @hidden
%%--------------------------------------------------------------------
init(_) -> 
    lr_hooks:add_hook(?MODULE, send_log, []),
    erlang:start_timer(?TIMER_VALUE, ?MODULE, flush),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%% @hidden
%%--------------------------------------------------------------------
handle_call({add_response, LogName, Opts, Response}, _From, State) ->
    Responses = State#state.responses,
	{reply, ok, State#state{responses=[{LogName, {Opts, Response}}|Responses]}};
	
handle_call(get_responses, _From, Responses) ->
    {reply, Responses, Responses}.
	
%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%% @hidden
%%--------------------------------------------------------------------
handle_cast({log, LogName, LogEntry}, State) ->
    Buffers = State#state.buffers,
    NewState =
        case lists:keyfind(LogName, 2, Buffers) of
            false ->
                State#state{buffers=[#buffer{log_name=LogName, logs=[LogEntry]}|Buffers]};
            Buffer ->
                NewBuffer = Buffer#buffer{logs=[LogEntry|Buffer#buffer.logs]},
                State#state{
                    buffers=lists:keyreplace(LogName, 2, State#state.buffers, NewBuffer)
                }
        end,
    {noreply, NewState};
    
handle_cast(_Message, State) -> {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%% @hidden
%%--------------------------------------------------------------------	
handle_info({timeout,_,flush}, State) ->
    [begin
        [begin
            Terms = lists:foldl(
                fun(Log, Acc) -> 
                    case lr_filter:filter(Log, Opts) of
                        [_] -> [log_roller_utils:format_log_entry(Log)|Acc];
                        [] -> Acc
                    end
                end, [], Buffer#buffer.logs),
            Content = lr_logs:render({data, Terms}),
            (catch Response:write_chunk(Content))
         end || {LogName, {Opts, Response}} <- State#state.responses, LogName == Buffer#buffer.log_name]
     end || Buffer <- State#state.buffers],
    erlang:start_timer(?TIMER_VALUE, ?MODULE, flush),
    {noreply, State#state{buffers=[]}};

handle_info(_Info, State) -> error_logger:info_msg("info: ~p~n", [_Info]), {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @hidden
%%--------------------------------------------------------------------
terminate(_Reason, _State) -> ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%% @hidden
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> {ok, State}.

