%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%
%%%-------------------------------------------------------------------
-module(ci_datastore).

-behaviour(gen_server).

-include("call_inspector.hrl").

%% API
-export([start_link/0]).
-export([store_chunk/1]).
-export([store_analysis/1]).
-export([lookup_callid/1]).
-export([flush/0
        ,flush/1
        ]).

%% gen_server callbacks
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-record(state, {}).
-type state() :: #state{}.

-record(object, {call_id
                 ,timestamp
                 ,type
                 ,value
                 }).

-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).

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
    gen_server:start_link({'local', ?SERVER}, ?MODULE, [], []).

-spec store_chunk(ci_chunk:chunk()) -> 'ok'.
store_chunk(Chunk) ->
    'true' = ci_chunk:is_chunk(Chunk),
    CallId = ci_chunk:call_id(Chunk),
    gen_server:cast(?SERVER, {'store_chunk', CallId, Chunk}).

-spec store_analysis(ci_analysis:analysis()) -> 'ok'.
store_analysis(Analysis) ->
    'true' = ci_analysis:is_analysis(Analysis),
    CallId = ci_analysis:call_id(Analysis),
    gen_server:cast(?SERVER, {'store_analysis', CallId, Analysis}).

-spec lookup_callid(ne_binary()) -> wh_json:object().
lookup_callid(CallId) ->
    Props = lists:foldl(fun(#object{type='chunk', value=Chunk}, P) ->
                                Chunks = props:get_value('chunks', P, []),
                                props:set_value('chunks', [Chunk|Chunks], P);
                           (#object{type='analysis', value=Analysis}, P) ->
                                props:set_value('analysis', Analysis, P)
                        end
                       ,[{'chunks', []}, {'analysis', []}]
                       ,ets:lookup(?TAB, CallId)),
    props:set_value('chunks'
                   ,lists:reverse(props:get_value('chunks', Props))
                   ,Props
                   ).

-spec flush() -> 'ok'.
flush() ->
    gen_server:cast(?SERVER, 'flush').

-spec flush(ne_binary()) -> 'ok'.
flush(CallId) ->
    gen_server:cast(?SERVER, {'flush', CallId}).

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
    _ = ets:new(?TAB, ['named_table'
                      ,'duplicate_bag'
                      ,{'keypos', #object.call_id}
                      ,'protected'
                      ,{'read_concurrency', 'true'}
                      ]),
    {'ok', #state{}}.

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
-spec handle_call(atom(), any(), state()) -> handle_call_ret().
handle_call(_Request, _From, State) ->
    lager:debug("unhandled handle_call executed ~p~p", [_Request, _From]),
    Reply = 'ok',
    {'reply', Reply, State}.

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
handle_cast({'store_chunk', CallId, Chunk}, State) ->
    Object = #object{call_id=CallId
                    ,timestamp=wh_util:current_tstamp()
                    ,type='chunk'
                    ,value=Chunk
                    },
    _ = ets:insert(?TAB, Object),
    _ = ci_analyzers:new_chunk(CallId, Chunk),
    {'noreply', State};
handle_cast({'store_analysis', CallId, Analysis}, State) ->
    Object = #object{call_id=CallId
                    ,timestamp=wh_util:current_tstamp()
                    ,type='analysis'
                    ,value=Analysis
                    },
    _ = ets:insert(?TAB, Object),
    {'noreply', State};
handle_cast('flush', State) ->
    _ = ets:delete_all_objects(?TAB),
    {'noreply', State};
handle_cast({'flush', CallId}, State) ->
    _ = ets:delete(?TAB, CallId),
    {'noreply', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled handle_cast ~p", [_Msg]),
    {'noreply', State}.

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
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminate
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{}=_State) ->
    lager:debug("call inspector datastore terminated: ~p", [_Reason]),
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
