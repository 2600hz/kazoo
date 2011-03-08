%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Handle registrations of Name/CallID combos for media, creating
%%% temp names to store on the local box.
%%% @end
%%% Created : 27 Aug 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(ecallmgr_media_registry).

-behaviour(gen_server).

%% API
-export([start_link/0, register_name/2, is_registered/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(logger, [log/2, format_log/3]).

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

%% Name is the value passed from the CallControl queue in the Record
%% message. Creates a new entry or returns the generated name if
%% {CallID,Name} exists.
register_name(CallID, Name) ->
    gen_server:call(?MODULE, {register_name, CallID, Name}).

is_registered(CallID, Name) ->
    gen_server:call(?MODULE, {is_registered, CallID, Name}).

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
    {ok, dict:new()}.

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
handle_call({register_name, CallID, Name}, {Pid, _Ref}, Dict) ->
    case dict:find({Pid, CallID, Name}, Dict) of
	error ->
	    link(Pid),
	    GenName = generate_name(CallID, Name),
	    {reply, GenName, dict:store({Pid, CallID, Name}, GenName, Dict)};
	{ok, GenName} ->
	    {reply, GenName, Dict}
    end;
handle_call({is_registered, CallID, Name}, {Pid, _Ref}, Dict) ->
    Reply = case dict:find({Pid, CallID, Name}, Dict) of
		error ->
		    false;
		{ok, GenName} ->
		    {true, GenName}
	    end,
    {reply, Reply, Dict};
handle_call(_Request, _From, Dict) ->
    Reply = ok,
    {reply, Reply, Dict}.

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
handle_cast(_Msg, Dict) ->
    {noreply, Dict}.

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
handle_info({'DOWN', _Ref, process, Pid, _Reason}, Dict) ->
    format_log(info, "MEDIA_REG(~p): Pid ~p down, Reason: ~p, cleaning up...~n", [self(), Pid, _Reason]),
    {noreply, dict:filter(fun({Pid1, _CallID, _Name}, _Value) -> Pid =/= Pid1 end, Dict)};
handle_info({'EXIT', Pid, _Reason}, Dict) ->
    format_log(info, "MEDIA_REG(~p): Pid ~p exited, Reason ~p, cleaning up...~n", [self(), Pid, _Reason]),
    {noreply, dict:filter(fun({Pid1, _CallID, _Name}, _Value) ->
				  format_log(info, "MEDIA_REG.filter P: ~p P1: ~p~n", [Pid, Pid1]),
				  Pid =/= Pid1
			  end, Dict)};
handle_info(_Info, Dict) ->
    format_log(info, "MEDIA_REG(~p): Info Msg: ~p~n", [self(), _Info]),
    {noreply, Dict}.

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
    ok.

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
generate_name(CallID, Name) ->
    whistle_util:to_hex(erlang:md5(list_to_binary([CallID,Name,"shh"]))).
