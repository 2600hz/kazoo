%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz INC
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

         ,consume_tokens_until/2, consume_tokens_until/3

         ,start_bucket/1, start_bucket/2, start_bucket/3, start_bucket/4
         ,exists/1
         ,tokens/0
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

-define(MAX_TOKENS, whapps_config:get_integer(<<"token_buckets">>, <<"max_bucket_tokens">>, 100)).
-define(FILL_RATE, whapps_config:get_integer(<<"token_buckets">>, <<"tokens_fill_rate">>, 10)).

-record(state, {table_id :: ets:tid()
                ,inactivity_timer_ref :: reference()
               }).

-record(bucket, {key :: api_binary() | '_'
                 ,srv :: pid() | '$1' | '_'
                 ,ref :: reference() | '$2' | '_'
                 ,accessed = os:timestamp() :: wh_now() | '_'
                }).
-type bucket() :: #bucket{}.

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
    gen_server:start_link({'local', ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% consume_tokens(Name, [Count, [StartIfMissing]])
%% Name :: name of the bucket
%% Count :: how many tokens to try to consume
%% StartIfMissing :: start the token bucket if it doesn't exist yet
%% @end
%%--------------------------------------------------------------------
-spec consume_token(api_binary()) -> boolean().
-spec consume_token(api_binary(), boolean()) -> boolean().

consume_token(Name) ->
    consume_tokens(Name, 1).

consume_token(Name, StartIfMissing) ->
    consume_tokens(Name, 1, StartIfMissing).

-spec consume_tokens(api_binary(), pos_integer()) -> boolean().
-spec consume_tokens(api_binary(), pos_integer(), boolean()) -> boolean().
consume_tokens(Key, Count) ->
    consume_tokens(Key, Count, 'true').

consume_tokens(Key, Count, StartIfMissing) ->
    consume_tokens(Key, Count, StartIfMissing, fun kz_token_bucket:consume/2).

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
-spec consume_tokens_until(api_binary(), pos_integer()) -> boolean().
-spec consume_tokens_until(api_binary(), pos_integer(), boolean()) -> boolean().
consume_tokens_until(Key, Count) ->
    consume_tokens_until(Key, Count, 'true').

consume_tokens_until(Key, Count, StartIfMissing) ->
    consume_tokens(Key, Count, StartIfMissing, fun kz_token_bucket:consume_until/2).

consume_tokens(Key, Count, StartIfMissing, BucketFun) ->
    case get_bucket(Key) of
        'undefined' when StartIfMissing ->
            lager:debug("bucket ~s missing, starting", [Key]),
            case start_bucket(Key) of
                'error' -> 'false';
                _OK -> consume_tokens(Key, Count, StartIfMissing, BucketFun)
            end;
        'undefined' -> 'false';
        Srv -> BucketFun(Srv, Count)
    end.

-spec get_bucket(ne_binary()) -> api_pid().
-spec get_bucket(ne_binary(), 'record'|'server') -> api_pid() | bucket().
get_bucket(Key) ->
    get_bucket(Key, 'server').

get_bucket(Key, 'record') ->
    case ets:lookup(table_id(), Key) of
        [] -> 'undefined';
        [Bucket] -> Bucket
    end;
get_bucket(Key, 'server') ->
    case ets:lookup(table_id(), Key) of
        [] -> 'undefined';
        [#bucket{srv=Srv}] ->
            gen_server:cast(?MODULE, {'bucket_accessed', Key}),
            Srv
    end.


-spec exists(api_binary()) -> boolean().
exists(Key) ->
    case ets:lookup(table_id(), Key) of
        [] -> 'false';
        [#bucket{}] -> 'true';
        _O ->
            lager:error("exists(~s) failed: ~p", [Key, _O]),
            'false'
    end.

-spec start_bucket(ne_binary()) ->
                          'ok' | 'error' | 'exists'.
-spec start_bucket(ne_binary(), pos_integer()) ->
                          'ok' | 'error' | 'exists'.
-spec start_bucket(ne_binary(), pos_integer(), pos_integer()) ->
                          'ok' | 'error' | 'exists'.
-spec start_bucket(ne_binary(), pos_integer(), pos_integer(), kz_token_bucket:fill_rate_time()) ->
                          'ok' | 'error' | 'exists'.
start_bucket(Name) ->
    start_bucket(Name, ?MAX_TOKENS).
start_bucket(Name, MaxTokens) ->
    start_bucket(Name, MaxTokens, ?FILL_RATE).
start_bucket(Name, MaxTokens, FillRate) ->
    start_bucket(Name, MaxTokens, FillRate, kz_token_bucket:default_fill_time()).
start_bucket(Name, MaxTokens, FillRate, FillTime) ->
    case exists(Name) of
        'true' ->
            lager:debug("bucket exists for ~s already", [Name]),
            'exists';
        'false' ->
            gen_server:call(?MODULE, {'start', Name, MaxTokens, FillRate, FillTime})
    end.

-spec tokens() -> 'ok'.
tokens() ->
    io:format("~60.60s | ~20.20s | ~10.10s | ~20.20s |~n", [<<"Key">>, <<"Pid">>, <<"Tokens">>, <<"Last Accessed">>]),
    tokens_traverse(ets:first(table_id())).
tokens_traverse('$end_of_table') ->
    io:format("~s~n", [<<"No more token servers">>]);
tokens_traverse(Key) ->
    [#bucket{key=K, srv=P, accessed=Accessed}] = ets:lookup(table_id(), Key),
    io:format("~60.60s | ~20.20s | ~10.10s | ~20.20s |~n"
              ,[K, pid_to_list(P), integer_to_list(kz_token_bucket:tokens(P))
                ,integer_to_list(wh_util:elapsed_s(Accessed))
               ]
             ),
    tokens_traverse(ets:next(table_id(), Key)).

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
    wh_util:put_callid(?MODULE),
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
handle_call({'start', Name, MaxTokens, FillRate, FillTime}, _From, #state{table_id=Tbl}=State) ->
    lager:debug("maybe starting token bucket for ~s (~b at ~b/~s)"
                ,[Name, MaxTokens, FillRate, FillTime]
               ),
    case not exists(Name) andalso kz_buckets_sup:start_bucket(MaxTokens, FillRate, FillTime) of
        {'ok', Pid} when is_pid(Pid) ->
            case ets:insert_new(Tbl, new_bucket(Pid, Name)) of
                'true' -> lager:debug("new bucket for ~s: ~p", [Name, Pid]);
                'false' ->
                    lager:debug("hmm, bucket appears to exist for ~s, stopping ~p", [Name, Pid]),
                    kz_buckets_sup:stop_bucket(Pid)
            end,
            kz_token_bucket:set_name(Pid, Name),
            {'reply', 'ok', State};
        'false' ->
            lager:debug("good chance the bucket ~s already exists", [Name]),
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
    ets:update_element(table_id(), Key, {#bucket.accessed, os:timestamp()}),
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
    _Pid = spawn(fun check_for_inactive_buckets/0),
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
-spec new_bucket(pid(), api_binary()) -> bucket().
new_bucket(Pid, Name) ->
    #bucket{key=Name
            ,srv=Pid
            ,ref=erlang:monitor('process', Pid)
           }.

-spec start_inactivity_timer() -> reference().
start_inactivity_timer() ->
    erlang:send_after(?MILLISECONDS_IN_MINUTE, self(), ?INACTIVITY_MSG).

-spec check_for_inactive_buckets() -> 'ok'.
-spec check_for_inactive_buckets(wh_now(), pos_integer(), api_binary() | '$end_of_table') -> 'ok'.

check_for_inactive_buckets() ->
    wh_util:put_callid(?MODULE),
    Now = os:timestamp(),
    InactivityTimeout = ?INACTIVITY_TIMEOUT_MS,
    check_for_inactive_buckets(Now, InactivityTimeout, ets:first(table_id())).

check_for_inactive_buckets(_Now, _InactivityTimeout, '$end_of_table') -> 'ok';
check_for_inactive_buckets(Now, InactivityTimeout, Key) ->
    case get_bucket(Key, 'record') of
        'undefined' -> 'ok';
        Bucket ->
            maybe_stop_bucket(Now, InactivityTimeout, Bucket)
    end,
    check_for_inactive_buckets(Now, InactivityTimeout, ets:next(table_id(), Key)).

-spec maybe_stop_bucket(wh_now(), pos_integer(), bucket()) -> 'ok'.
maybe_stop_bucket(Now
                  ,InactivityTimeout
                  ,#bucket{accessed=Accessed
                           ,srv=Srv
                           ,key=Key
                          }
                 ) ->
    case wh_util:elapsed_ms(Accessed, Now) > InactivityTimeout of
        'false' -> 'ok';
        'true' ->
            lager:debug("bucket ~s(~p) hasn't been accessed recently, stopping", [Key, Srv]),
            kz_token_bucket:stop(Srv),
            'ok'
    end.
