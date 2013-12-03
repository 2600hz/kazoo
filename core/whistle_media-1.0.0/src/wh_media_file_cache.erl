%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wh_media_file_cache).

-behaviour(gen_server).

%% API
-export([start_link/3
         ,single/1
         ,continuous/1
        ]).

%% gen_server callbacks
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("whistle_media.hrl").

-define(TIMEOUT_LIFETIME, 600000).
-define(TIMEOUT_MESSAGE, {'$wh_media_file_cache', 'file_timeout'}).

-record(state, {
          db :: ne_binary()
         ,doc :: ne_binary()
         ,attach :: ne_binary()
         ,meta :: wh_json:object()
         ,contents = <<>> :: binary()
         ,stream_ref :: reference()
         ,status :: 'streaming' | 'ready'
         ,reqs :: [{pid(), reference()},...] | []
         ,timer_ref :: reference()
         }).
-type state() :: #state{}.

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
-spec start_link(ne_binary(), ne_binary(), ne_binary()) -> startlink_ret().
start_link(Db, Id, Attachment) ->
    gen_server:start_link(?MODULE, [Db, Id, Attachment, get('callid')], []).

-spec single(pid()) -> {wh_json:object(), binary()}.
single(Srv) -> gen_server:call(Srv, 'single').

-spec continuous(pid()) -> {wh_json:object(), binary()}.
continuous(Srv) -> gen_server:call(Srv, 'continuous').

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
init([Db, Id, Attachment, CallId]) ->
    case wh_util:is_empty(CallId) of
        'true' -> put('callid', ?LOG_SYSTEM_ID);
        'false' -> put('callid', CallId)
    end,
    maybe_start_file_cache(Db, Id, Attachment).

-spec maybe_start_file_cache(ne_binary(), ne_binary(), ne_binary()) ->
                                    {'stop', _} |
                                    {'ok', state()}.
maybe_start_file_cache(Db, Id, Attachment) ->
    case couch_mgr:open_doc(Db, Id) of
        {'error', 'not_found'} ->
            lager:warning("failed to find '~s' in '~s'", [Id, Db]),
            {'stop', 'not_found'};
        {'error', Reason} ->
            lager:debug("unable get metadata for ~s on ~s in ~s: ~p", [Attachment, Id, Db, Reason]),
            {'stop', Reason};
        {'ok', JObj} ->
            lager:debug("starting cache for ~s on ~s in ~s", [Attachment, Id, Db]),
            Meta = wh_json:get_value([<<"_attachments">>, Attachment], JObj, wh_json:new()),
            {'ok', Ref} = couch_mgr:stream_attachment(Db, Id, Attachment),
            {'ok', #state{db=Db
                          ,doc=Id
                          ,attach=Attachment
                          ,meta=Meta
                          ,stream_ref=Ref
                          ,status='streaming'
                          ,contents = <<>>
                          ,reqs = [] %% buffer requests until file has completed streaming
                          ,timer_ref=start_timer()
                         }}
    end.

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
handle_call('single', _From, #state{meta=Meta
                                    ,contents=Contents
                                    ,status='ready'
                                    ,timer_ref=TRef
                                   }=State) ->
    %% doesn't currently check whether we're still streaming in from the DB
    lager:debug("returning media contents"),
    _ = stop_timer(TRef),
    {'reply', {Meta, Contents}, State#state{timer_ref=start_timer()}};
handle_call('single', From, #state{reqs=Reqs
                                   ,status='streaming'
                                  }=State) ->
    lager:debug("file not ready for ~p, queueing", [From]),
    {'noreply', State#state{reqs=[From | Reqs]}};
handle_call('continuous', From, #state{reqs=Reqs
                                       ,status='streaming'
                                      }=State) ->
    lager:debug("file not ready for ~p, queueing", [From]),
    {'noreply', State#state{reqs=[From | Reqs]}};
handle_call('continuous', _From, #state{meta=Meta
                                        ,contents=Contents
                                        ,status='ready'
                                        ,timer_ref=TRef
                                       }=State) ->
    lager:debug("returning media contents"),
    _ = stop_timer(TRef),
    {'reply', {Meta, Contents}, State#state{timer_ref=start_timer()}}.

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
handle_cast(_Msg, State) ->
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
handle_info({'timeout', TRef, ?TIMEOUT_MESSAGE}, #state{timer_ref=TRef}=State) ->
    lager:debug("timeout expired, going down"),
    {'stop', 'normal', State};
handle_info({Ref, 'done'}, #state{stream_ref=Ref
                                  ,reqs=Reqs
                                  ,contents=Contents
                                  ,meta=Meta
                                  ,timer_ref=TRef
                                 }=State) ->
    _ = stop_timer(TRef),
    Res = {Meta, Contents},
    _ = [gen_server:reply(From, Res) || From <- Reqs],

    lager:debug("finished receiving file contents"),
    {'noreply', State#state{status='ready'
                            ,timer_ref=start_timer()
                            ,reqs = []
                           }
    ,'hibernate'};
handle_info({Ref, {'ok', Bin}}, #state{stream_ref=Ref
                                       ,contents=Contents
                                      }=State) ->
    lager:debug("recv ~b bytes", [byte_size(Bin)]),
    {'noreply', State#state{contents = <<Contents/binary, Bin/binary>>}, 'hibernate'};
handle_info({Ref, {'error', E}}, #state{stream_ref=Ref}=State) ->
    lager:debug("recv stream error: ~p", [E]),
    {'stop', 'normal', State};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State, 'hibernate'}.

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
    lager:debug("media file going down: ~p", [_Reason]).

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
start_timer() ->
    erlang:start_timer(?TIMEOUT_LIFETIME, self(), ?TIMEOUT_MESSAGE).
stop_timer(Ref) when is_reference(Ref) ->
    erlang:cancel_timer(Ref);
stop_timer(_) -> 'ok'.
