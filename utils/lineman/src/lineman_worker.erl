%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%------------------------------------------------------------------
-module(lineman_worker).

-behaviour(gen_server).

-export([start_link/0]).
-export([run/1]).
-export([print_report/1]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("lineman.hrl").

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec run/1 :: (text()) -> 'ok' | {'error', term()}.
run(File) when not is_list(File) ->
    run(wh_util:to_list(File));
run(File) ->
    gen_server:call(?SERVER, {dispatch, File}).

print_report(Workorder) ->
    io:format("Test Name: ~s~n", [lineman_workorder:name(Workorder)]),
    io:format("Running Sequences: ~b~n", [lineman_workorder:running_sequences(Workorder)]),
    io:format("Completed Sequences: ~b~n", [lineman_workorder:completed_sequences(Workorder)]),
    io:format("Failed Sequences: ~b~n", [lineman_workorder:failed_sequences(Workorder)]),
    ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, lineman_workorder:empty()}.

%%--------------------------------------------------------------------
%% @private
%% @doc

%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({dispatch, File}, _, _) ->
    case lineman_workorder:read(File) of
        {error, _}=E -> E;
        {ok, Workorder} -> 
            gen_server:cast(self(), initialize_toolbag),
            {reply, ok, Workorder}
    end; 
handle_call(_Msg, _From, Workorder) ->
    {reply, {error, not_implemented}, Workorder}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(initialize_toolbag, Workorder) ->
    Toolbag = lineman_workorder:toolbag(Workorder),
    ok = lineman_toolbag_sup:reset_all(),
    initialize_tools(Toolbag),
    erlang:send_after(lineman_workorder:sequence_period(Workorder), self(), {start_sequences}),
    erlang:send_after(lineman_workorder:display_period(Workorder), self(), {print_report}),
    {noreply, Workorder};
handle_cast(_Msg, Workorder) ->
    {noreply, Workorder}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({start_sequences}, Workorder) ->
    Total = lineman_workorder:completed_sequences(Workorder)
        + lineman_workorder:failed_sequences(Workorder)
        + lineman_workorder:running_sequences(Workorder),
    case lineman_workorder:max_sequence_executions(Workorder) > Total of
        false ->
            erlang:send_after(1000, self(), {gracefull_stop}),
            {noreply, Workorder};
        true ->
            erlang:send_after(lineman_workorder:sequence_period(Workorder), self(), {start_sequences}),
            Headroom = lineman_workorder:max_running_sequences(Workorder) - 
                lineman_workorder:running_sequences(Workorder),
            case lineman_workorder:sequence_rate(Workorder) of
                Rate when Rate > Headroom ->
                    {noreply, start_sequences(Headroom, Workorder)};
                Rate ->
                    {noreply, start_sequences(Rate, Workorder)} 
            end
    end;
handle_info({gracefull_stop}, Workorder) ->
    case lineman_workorder:running_sequences(Workorder) =:= 0 of
        true -> 
            print_report(Workorder),
            {stop, normal, Workorder};
        false ->
            erlang:send_after(1000, self(), {gracefull_stop}),
            {noreply, Workorder}
    end;
handle_info({print_report}, Workorder) ->
    erlang:send_after(lineman_workorder:display_period(Workorder), self(), {print_report}),
    spawn(?MODULE, print_report, [Workorder]),
    {noreply, Workorder};
handle_info({'EXIT', Pid, normal}, Workorder) ->
    {noreply, lineman_workorder:remove_running_sequence(Pid, true, Workorder)};
handle_info({'EXIT', Pid, _Reason}, Workorder) ->
    {noreply, lineman_workorder:remove_running_sequence(Pid, false, Workorder)};
handle_info(_Info, Workorder) ->
    lager:info("unhandled ~p", [_Info]),
    {noreply, Workorder}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _Workorder) ->
    lager:debug("listener terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, Workorder, _Extra) ->
    {ok, Workorder}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec initialize_tools/1 :: ([#xmlElement{},...] | []) -> 'ok'.
initialize_tools([Tool|Tools]) ->
    {xmlObj, string, Name} = xmerl_xpath:string("name()", Tool),
    Parameters = xmerl_xpath:string("/*/*", Tool),
    initialize_tool(Name, Parameters),
    initialize_tools(Tools);
initialize_tools([]) -> ok.

-spec initialize_tool/2 :: (string(), [#xmlElement{},...] | []) -> 'ok'.
initialize_tool(Tool, [Parameter|Parameters]) ->
    {xmlObj, string, Name} = xmerl_xpath:string("name()", Parameter),
    lineman_toolbag_sup:set_parameter(Tool, Name, Parameter),
    initialize_tool(Tool, Parameters);
initialize_tool(_, []) -> ok.

-spec start_sequences/2 :: (integer(), lineman_workorder:workorder()) -> lineman_workorder:workorder().
start_sequences(Rate, Workorder) when Rate =< 0 ->
    Workorder;
start_sequences(Rate, Workorder) ->
    Sequences = lineman_workorder:sequences(Workorder),
    Order = lineman_workorder:sequence_order(Workorder),
    {ok, Sequence, NewSequences} = next_sequence(Order, Sequences),
    Pid = lineman_sequence:start_link(Sequence, Workorder),
    start_sequences(Rate - 1, lineman_workorder:add_running_sequence(Pid, NewSequences, Workorder)).

-spec next_sequence/2 :: (ne_binary(), list()) -> {'ok', xml_el() | xml_els(), list()}.
next_sequence(_, []) ->
    throw(<<"no sequences found">>);
next_sequence(<<"random">>, Sequences) ->
    N = crypto:rand_uniform(1, length(Sequences)),
    {ok, lists:nth(N, Sequences), Sequences};
next_sequence(<<"sequential">>, [Sequence|Sequences]) ->
    {ok, Sequence, lists:append(Sequences, [Sequence])}.
