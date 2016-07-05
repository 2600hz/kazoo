%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2016, 2600Hz INC
%%% @doc
%%% API interface for buckets
%%% ETS writer for table
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kz_buckets).

%% API exports
-export([start_link/0
        ,consume_token/1, consume_token/2
        ,consume_tokens/2, consume_tokens/3

        ,consume_tokens_until/2, consume_tokens_until/3, consume_tokens_until/4

        ,start_bucket/1, start_bucket/2, start_bucket/3, start_bucket/4, start_bucket/5
        ,exists/1, exists/2
        ,tokens/0

        ,get_bucket/2, get_bucket/3
        ]).

%% ETS related
-export([table_id/0
        ,table_options/0
        ,gift_data/0
        ]).

%% gen_server callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include("kz_buckets.hrl").

-define(SERVER, ?MODULE).

-define(MAX_TOKENS, kapps_config:get_integer(?APP_NAME, [?DEFAULT_APP, <<"max_bucket_tokens">>], 100)).
-define(MAX_TOKENS(App)
       ,kapps_config:get_integer(?APP_NAME, [App, <<"max_bucket_tokens">>], ?MAX_TOKENS)
       ).

-define(FILL_RATE, kapps_config:get_integer(?APP_NAME, <<"tokens_fill_rate">>, 10)).
-define(FILL_RATE(App)
       ,kapps_config:get_integer(?APP_NAME, [App, <<"tokens_fill_rate">>], ?FILL_RATE)
       ).

-record(state, {table_id :: ets:tid()
               ,inactivity_timer_ref :: reference()
               }).

-record(bucket, {key :: {ne_binary(), ne_binary()} | '_'
                ,srv :: pid() | '$1' | '$2' | '_'
                ,ref :: reference() | '$2' | '_'
                ,accessed = kz_util:now_s() :: gregorian_seconds() | '$1' | '_'
                }).
-type bucket() :: #bucket{}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    gen_server:start_link({'local', ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% consume_tokens(Name, [Count, [StartIfMissing]])
%% Name :: name of the bucket
%% Count :: how many tokens to try to consume
%% StartIfMissing :: start the token bucket if it doesn't exist yet
%% @end
%%--------------------------------------------------------------------
-spec consume_token(ne_binary()) -> boolean().
-spec consume_token(ne_binary(), ne_binary() | boolean()) -> boolean().

consume_token(Name) ->
    consume_tokens(Name, 1).

consume_token(<<_/binary>> = App, <<_/binary>> = Name) ->
    consume_tokens(App, Name, 1);
consume_token(Name, StartIfMissing) ->
    consume_tokens(?DEFAULT_APP, Name, 1, StartIfMissing).

-spec consume_tokens(ne_binary(), integer()) -> boolean().
-spec consume_tokens(ne_binary(), ne_binary() | integer(), integer() | boolean()) -> boolean().
-spec consume_tokens(ne_binary(), ne_binary(), integer(), boolean()) -> boolean().
consume_tokens(Key, Count) ->
    consume_tokens(Key, Count, 'true').

consume_tokens(<<_/binary>> = App, <<_/binary>> = Key, Count) when is_integer(Count) ->
    consume_tokens(App, Key, Count, 'true');
consume_tokens(<<_/binary>> = Key, Count, StartIfMissing) when is_integer(Count),
                                                               is_boolean(StartIfMissing)
                                                               ->
    consume_tokens(?DEFAULT_APP, Key, Count, StartIfMissing).

consume_tokens(App, Key, Count, StartIfMissing) ->
    consume_tokens(App, Key, Count, StartIfMissing, fun kz_token_bucket:consume/2).

%%--------------------------------------------------------------------
%% @doc
%% consume_tokens_until(Name, Count, [StartIfMissing])
%% Name :: name of the bucket
%% Count :: how many tokens to try to consume
%% StartIfMissing :: start the token bucket if it doesn't exist yet
%%
%% If Bucket is started and has fewer than Count tokens, consume the
%% remaining tokens and return false
%% @end
%%--------------------------------------------------------------------
-spec consume_tokens_until(ne_binary(), pos_integer()) -> boolean().
-spec consume_tokens_until(ne_binary(), ne_binary() | pos_integer(), pos_integer() | boolean()) -> boolean().
consume_tokens_until(Key, Count) ->
    consume_tokens_until(?DEFAULT_APP, Key, Count, 'true').

consume_tokens_until(<<_/binary>> = App, <<_/binary>> = Key, Count) when is_integer(Count) ->
    consume_tokens_until(App, Key, Count, 'true');
consume_tokens_until(<<_/binary>> = Key, Count, StartIfMissing) when is_integer(Count),
                                                                     is_boolean(StartIfMissing)
                                                                     ->
    consume_tokens(?DEFAULT_APP, Key, Count, StartIfMissing).

consume_tokens_until(<<_/binary>> = App, <<_/binary>> = Key, Count, StartIfMissing) when is_integer(Count),
                                                                                         is_boolean(StartIfMissing)
                                                                                         ->
    consume_tokens(App, Key, Count, StartIfMissing, fun kz_token_bucket:consume_until/2).

-spec consume_tokens(ne_binary(), ne_binary(), integer(), boolean(), fun()) -> boolean().
consume_tokens(App, Key, Count, StartIfMissing, BucketFun) ->
    case get_bucket(App, Key) of
        'undefined' ->
            maybe_start_bucket(App, Key, Count, StartIfMissing, BucketFun);
        Srv ->
            case is_process_alive(Srv) of
                'true' -> BucketFun(Srv, Count);
                'false' ->
                    maybe_start_bucket(App, Key, Count, StartIfMissing, BucketFun)
            end
    end.

-spec maybe_start_bucket(ne_binary(), ne_binary(), integer(), boolean(), fun()) -> boolean().
maybe_start_bucket(_App, _Key, _Count, 'false', _BucketFun) -> 'false';
maybe_start_bucket(App, Key, Count, 'true', BucketFun) ->
    lager:debug("bucket (~s ~s) missing, starting", [App, Key]),
    case start_bucket(App, Key) of
        'error' -> 'false';
        _OK -> consume_tokens(App, Key, Count, 'false', BucketFun)
    end.

-spec get_bucket(ne_binary(), ne_binary()) -> api_pid().
-spec get_bucket(ne_binary(), ne_binary(), 'record'|'server') -> api_pid() | bucket().
get_bucket(App, Key) ->
    get_bucket(App, Key, 'server').

get_bucket(App, Key, 'record') ->
    case ets:lookup(table_id(), {App, Key}) of
        [] -> 'undefined';
        [Bucket] -> Bucket
    end;
get_bucket(App, Key, 'server') ->
    T = {App, Key},
    case ets:lookup(table_id(), T) of
        [] -> 'undefined';
        [#bucket{srv=Srv}] ->
            gen_server:cast(?SERVER, {'bucket_accessed', T}),
            Srv
    end.

-spec exists(ne_binary()) -> boolean().
-spec exists(ne_binary(), ne_binary()) -> boolean().
exists(Key) ->
    exists(?DEFAULT_APP, Key).
exists(App, Key) ->
    case ets:lookup(table_id(), {App, Key}) of
        [] -> 'false';
        [#bucket{}] -> 'true';
        _O ->
            lager:error("exists(~s, ~s) failed: ~p", [App, Key, _O]),
            'false'
    end.

-spec start_bucket(ne_binary()) ->
                          'ok' | 'error' | 'exists'.
-spec start_bucket(ne_binary(), ne_binary()) ->
                          'ok' | 'error' | 'exists'.
-spec start_bucket(ne_binary(), ne_binary(), pos_integer()) ->
                          'ok' | 'error' | 'exists'.
-spec start_bucket(ne_binary(), ne_binary(), pos_integer(), pos_integer()) ->
                          'ok' | 'error' | 'exists'.
-spec start_bucket(ne_binary(), ne_binary(), pos_integer(), pos_integer(), kz_token_bucket:fill_rate_time()) ->
                          'ok' | 'error' | 'exists'.
start_bucket(Name) ->
    start_bucket(?DEFAULT_APP, Name).
start_bucket(App, Name) ->
    start_bucket(App, Name, ?MAX_TOKENS(App)).
start_bucket(App, Name, MaxTokens) ->
    start_bucket(App, Name, MaxTokens, ?FILL_RATE(App)).
start_bucket(App, Name, MaxTokens, FillRate) ->
    start_bucket(App, Name, MaxTokens, FillRate, kz_token_bucket:default_fill_time(App)).
start_bucket(App, Name, MaxTokens, FillRate, FillTime) ->
    case exists(App, Name) of
        'true' ->
            lager:debug("bucket exists for (~s, ~s) already", [App, Name]),
            'exists';
        'false' ->
            gen_server:call(?SERVER, {'start', App, Name, MaxTokens, FillRate, FillTime})
    end.

-define(TOKEN_FORMAT_STRING, " ~20s | ~50.50s | ~15.15s | ~6.6s | ~20.20s |~n").
-spec tokens() -> 'ok'.
tokens() ->
    io:format(?TOKEN_FORMAT_STRING
             ,[<<"Application">>, <<"Key">>, <<"Pid">>, <<"Tokens">>, <<"Last Accessed">>]
             ),

    _ = lists:foldl(fun print_bucket_info/2
                   ,'undefined'
                   ,lists:keysort(#bucket.key, ets:tab2list(table_id()))
                   ),
    'ok'.

print_bucket_info(#bucket{key={CurrentApp, Name}
                         ,srv=P
                         ,accessed=Accessed
                         }
                 ,CurrentApp) ->
    io:format(?TOKEN_FORMAT_STRING
             ,[""
              ,Name
              ,pid_to_list(P)
              ,integer_to_list(kz_token_bucket:tokens(P))
              ,kz_util:pretty_print_elapsed_s(kz_util:elapsed_s(Accessed))
              ]
             ),
    CurrentApp;
print_bucket_info(#bucket{key={App, _}}=Bucket, _OldApp) ->
    io:format(?TOKEN_FORMAT_STRING
             ,[App, "", "", "" ,""]
             ),
    print_bucket_info(Bucket, App).

%%%===================================================================
%%% ETS
%%%===================================================================
-spec table_id() -> atom().
table_id() -> ?MODULE.

-spec table_options() -> list().
table_options() ->
    ['named_table' | kazoo_etsmgr_srv:default_table_options()].

-spec gift_data() -> 'ok'.
gift_data() -> 'ok'.

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
    kz_util:put_callid(?MODULE),
    {'ok', #state{inactivity_timer_ref=start_inactivity_timer()}}.

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
handle_call({'start', App, Name, MaxTokens, FillRate, FillTime}, _From, #state{table_id=Tbl}=State) ->
    lager:debug("maybe starting token bucket for ~s, ~s (~b at ~b/~s)"
               ,[App, Name, MaxTokens, FillRate, FillTime]
               ),
    case not exists(App, Name)
        andalso kz_buckets_sup:start_bucket(MaxTokens, FillRate, FillTime)
    of
        {'ok', Pid} when is_pid(Pid) ->
            T = {App, Name},
            case ets:insert_new(Tbl, new_bucket(Pid, T)) of
                'true' -> lager:debug("new bucket for ~s, ~s: ~p", [App, Name, Pid]);
                'false' ->
                    lager:debug("hmm, bucket appears to exist for ~s, ~s, stopping ~p", [App, Name, Pid]),
                    kz_buckets_sup:stop_bucket(Pid)
            end,
            kz_token_bucket:set_name(Pid, T),
            {'reply', 'ok', State};
        'false' ->
            lager:debug("good chance the bucket ~s, ~s already exists", [App, Name]),
            {'reply', 'exists', State};
        _E ->
            lager:debug("error: starting bucket: ~p", [_E]),
            {'reply', 'error', State}
    end;
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

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
handle_cast(_Req, #state{table_id='undefined'}=State) ->
    lager:debug("ignoring req: ~p", [_Req]),
    {'noreply', State};
handle_cast({'bucket_accessed', Key}, State) ->
    ets:update_element(table_id(), Key, {#bucket.accessed, kz_util:now_s()}),
    {'noreply', State};
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
handle_info({'ETS-TRANSFER', Tbl, _From, _Data}, #state{table_id='undefined'}=State) ->
    lager:debug("recv ets transfer from ~p for ~p", [_From, Tbl]),
    {'noreply', State#state{table_id=Tbl}};
handle_info({'DOWN', Ref, 'process', Pid, _Reason}, #state{table_id=Tbl}=State) ->
    Match = [{#bucket{srv='$1'
                     ,ref='$2'
                     ,_='_'
                     }
             ,[{'=:=', '$1', Pid}
              ,{'=:=', '$2', Ref}
              ]
             ,['true']
             }
            ],
    case ets:select_delete(Tbl, Match) of
        N when N > 0 -> lager:debug("bucket ~p down: ~p", [Pid, _Reason]);
        0 -> lager:debug("unknown procress ~p(~p) down: ~p", [Pid, Ref, _Reason])
    end,
    {'noreply', State};
handle_info(?INACTIVITY_MSG, #state{inactivity_timer_ref=_OldRef}=State) ->
    _Pid = kz_util:spawn(fun check_for_inactive_buckets/0),
    {'noreply', State#state{inactivity_timer_ref=start_inactivity_timer()}};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

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
    lager:debug("bucket ets mgr going down: ~p", [_Reason]).

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
-spec new_bucket(pid(), {ne_binary(), ne_binary()}) -> bucket().
new_bucket(Pid, Name) ->
    #bucket{key=Name
           ,srv=Pid
           ,ref=erlang:monitor('process', Pid)
           }.

-spec start_inactivity_timer() -> reference().
start_inactivity_timer() ->
    erlang:send_after(?MILLISECONDS_IN_MINUTE, self(), ?INACTIVITY_MSG).

-spec check_for_inactive_buckets() -> 'ok'.
check_for_inactive_buckets() ->
    kz_util:put_callid(?MODULE),
    Now = kz_util:now_s(),
    InactivityTimeout = ?INACTIVITY_TIMEOUT_S,

    MS = [{#bucket{accessed='$1'
                  ,srv='$2'
                  ,_='_'
                  }
          ,[{'<', '$1', {'const', Now-InactivityTimeout}}]
          ,['$2']
          }],
    case [begin
              kz_token_bucket:stop(Srv),
              kz_util:to_binary(Srv)
          end
          || Srv <- ets:select(?MODULE:table_id(), MS)
         ]
    of
        [] -> 'ok';
        L -> lager:debug("stopped servers ~s", [kz_util:join_binary(L)])
    end.
