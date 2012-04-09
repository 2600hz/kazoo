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
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).
-export([run/0, run/1]).

-include_lib("lineman/src/lineman.hrl").

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

run() ->
    run("/opt/whistle/2600hz-platform/utils/lineman/workorders/registrar_validation.xml").

run(File) ->
    gen_server:call(?SERVER, {dispatch, File}).

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
    gen_server:cast(self(), start_sequences),
    {noreply, Workorder};
handle_cast(start_sequences, Workorder) ->
    [Sequence|_] = lineman_workorder:sequences(Workorder),
    lineman_sequence:start_link(Sequence, Workorder),
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
handle_info(_Info, Workorder) ->
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
