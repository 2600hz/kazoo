%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% If a single stream, just send the BigCouch URL for direct access to
%%% the document; if continuous, setup the server to buffer the media,
%%% accept connections, and stream the media to all connected listeners.
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(media_stream).

-behaviour(gen_server).

%% API
export([
        start_link/5
        ,join_stream/2
        ,stop/1
       ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("media.hrl").

-type stream_type() :: 'single' | 'continuous'.

-record(state, {
          db :: ne_binary()
          ,doc :: ne_binary()
          ,attachment :: ne_binary()
          ,stream_type :: stream_type()
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

-spec start_link/5 :: (wh_json:json_object(), ne_binary(), ne_binary(), ne_binary(), stream_type()) -> {'ok', pid()}.
start_link(JObj, Db, Doc, Attachment, StreamType) ->
    gen_server:start_link(?MODULE, [JObj, Db, Doc, Attachment, StreamType], []).

-spec join_stream/2 :: (pid(), wh_json:json_object()) -> 'ok'.
join_stream(StreamPid, JObj) ->
    gen_server:cast(StreamPid, {join, JObj}).

-spec stop/1 :: (pid()) -> 'ok'.
stop(StreamPid) ->
    gen_server:cast(StreamPid, stop_server).

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
init([JObj, Db, Doc, Attachement, StreamType]) ->
    wh_util:put_callid(JObj),

    {ok, #state{db=Db
                ,doc=Doc
                ,attachment=Attachment
                ,stream_type=StreamType
               }}.



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
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

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
handle_cast(stop, State) ->
    {stop, normal, State}.

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
    lager:debug("stream server terminated: ~p", [_Reason]).

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
