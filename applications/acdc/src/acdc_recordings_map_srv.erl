%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2022, 2600Hz
%%% @doc
%%% @author Daniel Finke
%%% @end
%%%-----------------------------------------------------------------------------
-module(acdc_recordings_map_srv).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([register/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("acdc.hrl").

-define(SERVER, ?MODULE).

-record(state, {table_id :: ets:tid()}).
-type state() :: #state{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server
%%
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_server:start_link({'local', ?SERVER}, ?MODULE, [], []).

-spec register(kapps_call:call(), kz_json:object()) -> pid().
register(Call, RecordingJObj) ->
    gen_server:call(?SERVER, {'register', Call, RecordingJObj}).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server
%%
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    process_flag('trap_exit', 'true'),
    TabId = ets:new('map', []),
    {'ok', #state{table_id=TabId}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages
%%
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call({'register', Call, RecordingJObj}, _From, #state{table_id=TabId}=State) ->
    case ets:lookup(TabId, kapps_call:call_id(Call)) of
        [] ->
            {'ok', Pid} = acdc_recordings_sup:new(Call, RecordingJObj),
            link(Pid),
            ets:insert(TabId, {kapps_call:call_id(Call), Pid}),
            Pid;
        [{_, Pid}] -> Pid
    end,
    {'reply', Pid, State};
handle_call(_Request, _From, State) ->
    Reply = 'ok',
    {'reply', Reply, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages
%%
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages
%%
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'EXIT', Pid, _Reason}, #state{table_id=TabId}=State) ->
    ets:match_delete(TabId, {'$1', Pid}),
    {'noreply', State};
handle_info(_Info, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    'ok'.

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed
%%
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
