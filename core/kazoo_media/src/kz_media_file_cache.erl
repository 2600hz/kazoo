%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_media_file_cache).

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

-include("kazoo_media.hrl").

-define(SERVER, ?MODULE).

-define(TIMEOUT_LIFETIME, 600 * ?MILLISECONDS_IN_SECOND).
-define(TIMEOUT_MESSAGE, {'$kz_media_file_cache', 'file_timeout'}).

-record(state, {db :: kz_term:ne_binary()
               ,doc :: kz_term:ne_binary()
               ,attach :: kz_term:ne_binary()
               ,meta :: kz_json:object()
               ,contents = <<>> :: binary()
               ,stream_ref :: reference()
               ,status :: 'streaming' | 'ready'
               ,reqs :: [{pid(), reference()}]
               ,timer_ref :: reference()
               }).
-type state() :: #state{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_types:startlink_ret().
start_link(Db, Id, Attachment) ->
    gen_server:start_link(?SERVER, [Db, Id, Attachment, get('callid')], []).

-spec single(pid()) -> {kz_json:object(), binary()}.
single(Srv) -> gen_server:call(Srv, 'single').

-spec continuous(pid()) -> {kz_json:object(), binary()}.
continuous(Srv) -> gen_server:call(Srv, 'continuous').

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init(list()) -> {'ok', state()} |
                      {'stop', any()}.
init([Db, Id, Attachment, CallId]) ->
    case kz_term:is_empty(CallId) of
        'true' -> kz_log:put_callid(?DEFAULT_LOG_SYSTEM_ID);
        'false' -> kz_log:put_callid(CallId)
    end,
    maybe_start_file_cache(Db, Id, Attachment).

-spec maybe_start_file_cache(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                                    {'stop', _} |
                                    {'ok', state()}.
maybe_start_file_cache(Db, Id, Attachment) ->
    case kz_datamgr:open_cache_doc(Db, Id) of
        {'error', 'not_found'} ->
            lager:warning("failed to find '~s' in '~s'", [Id, Db]),
            {'stop', 'not_found'};
        {'error', Reason} ->
            lager:debug("unable get metadata for ~s on ~s in ~s: ~p", [Attachment, Id, Db, Reason]),
            {'stop', Reason};
        {'ok', JObj} ->
            lager:debug("starting cache for ~s on ~s in ~s", [Attachment, Id, Db]),
            Meta = kz_doc:attachment(JObj, Attachment, kz_json:new()),
            {'ok', Ref} = kz_datamgr:stream_attachment(Db, Id, Attachment),
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

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
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
    lager:debug("file not ready for ~p, queuing", [From]),
    {'noreply', State#state{reqs=[From | Reqs]}};
handle_call('continuous', From, #state{reqs=Reqs
                                      ,status='streaming'
                                      }=State) ->
    lager:debug("file not ready for ~p, queuing", [From]),
    {'noreply', State#state{reqs=[From | Reqs]}};
handle_call('continuous', _From, #state{meta=Meta
                                       ,contents=Contents
                                       ,status='ready'
                                       ,timer_ref=TRef
                                       }=State) ->
    lager:debug("returning media contents"),
    _ = stop_timer(TRef),
    {'reply', {Meta, Contents}, State#state{timer_ref=start_timer()}}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
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

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("media file going down: ~p", [_Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec start_timer() -> reference().
start_timer() ->
    erlang:start_timer(?TIMEOUT_LIFETIME, self(), ?TIMEOUT_MESSAGE).

-spec stop_timer(reference()) -> integer() | boolean() | 'ok'.
stop_timer(Ref) ->
    erlang:cancel_timer(Ref).
