%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%------------------------------------------------------------------
-module(lineman_workorder).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).
-export([sample_workorder/0]).

-include_lib("lineman/src/lineman.hrl").

-define(SERVER, ?MODULE).

-record(state, {max_running_sequences = 10
                ,deadcall_wait = 5000
                ,sequence_order = simultanous
                ,busy = false
                ,workorder
                ,toolbag = []
                ,sequences = []
               }).

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

sample_workorder() ->
    gen_server:call(?SERVER, {dispatch, "/opt/whistle/2600hz-platform/utils/lineman/workorders/registration_test.xml"}).

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
    {ok, #state{}}.

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
handle_call({dispatch, _}, _From, #state{busy=true}=State) ->
    {reply, {error, busy}, State};
handle_call({dispatch, File}, _From, State) ->
    try xmerl_scan:file(File) of
        {error, _}=E ->
            {reply, E, State};
        {Xml, []} ->
            gen_server:cast(self(), initialize_toolbag),
            {reply, ok, set_workorder_parameters(Xml, State)};
        {_, _} ->
            {reply, {error, non_xml_content}, State}
    catch
        _:_ ->
            {reply, {error, parse_failed}, State}
    end; 
handle_call(_Msg, _From, State) ->
    {reply, {error, not_implemented}, State}.

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
handle_cast(initialize_toolbag, State) ->
    ok = lineman_toolbag_sup:reset_all(),
    gen_server:cast(self(), start_sequences),
    {noreply, initialize_toolbag(State)};
handle_cast(start_sequences, #state{sequences=[Seq]}=State) ->
    lineman_sequence:start_link(self(), Seq),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

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
handle_info(_Info, State) ->
    {noreply, State}.

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
terminate(_Reason, _State) ->
    lager:debug("listener terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec set_workorder_parameters/2 :: (#xmlElement{}, #state{}) -> #state{}.
set_workorder_parameters(Xml, State) ->
    State#state{workorder=Xml
               ,toolbag=xmerl_xpath:string("/workorder/toolbag/*", Xml)
               ,sequences=xmerl_xpath:string("/workorder/sequences/*", Xml)}.

-spec initialize_toolbag/1 :: (#state{}) -> #state{}.
initialize_toolbag(#state{toolbag=Toolbag}=State) ->
    initialize_tools(Toolbag),
    State.

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
