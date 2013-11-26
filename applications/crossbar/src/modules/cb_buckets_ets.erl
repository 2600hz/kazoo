%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, VoIP INC
%%% @doc
%%% Handles reading/writing to the buckets ETS table
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_buckets_ets).

-behaviour(gen_server).

%% API
-export([start_link/0
         ,has_token/1, has_token/2, has_token/3
         ,has_tokens/2, has_tokens/3, has_tokens/4
         ,start_bucket/1, start_bucket/2, start_bucket/3, start_bucket/4, start_bucket/5
         ,has_bucket/1, has_bucket/2
         ,key_pos/0
         ,table_id/0
         ,tokens/0
        ]).

%% gen_server callbacks
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("../crossbar.hrl").

-define(SERVER, ?MODULE).
-define(TABLE_ID, 'cb_buckets_mgr').

-define(MAX_TOKENS, whapps_config:get_integer(?CONFIG_CAT, <<"max_bucket_tokens">>, 100)).
-define(FILL_RATE, whapps_config:get_integer(?CONFIG_CAT, <<"tokens_fill_rate">>, 10)).

-record(state, {table_id :: ets:tid()}).

-record(bucket, {key :: api_binary() | '_'
                 ,srv :: pid() | '$1' | '_'
                 ,ref :: reference() | '$2' | '_'
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
    gen_server:start_link({'local', ?SERVER}, ?MODULE, [], []).

-spec has_token(cb_context:context()) -> boolean().
-spec has_token(api_binary(), api_binary()) -> boolean().
-spec has_token(api_binary(), api_binary(), boolean()) -> boolean().

-spec has_tokens(cb_context:context(), pos_integer()) -> boolean().
-spec has_tokens(api_binary(), api_binary(), pos_integer()) -> boolean().
-spec has_tokens(api_binary(), api_binary(), pos_integer(), boolean()) -> boolean().
has_token(Context) ->
    has_tokens(Context, 1).
has_token(AccountId, ClientIp) ->
    has_tokens(AccountId, ClientIp, 1).
has_token(AccountId, ClientIp, StartIfMissing) ->
    has_tokens(AccountId, ClientIp, 1, StartIfMissing).

bucket_name(Context) ->
    AccountId = case cb_context:account_id(Context) of
                    'undefined' -> <<"unauthenticated">>;
                    Id -> Id
                end,
    ClientIp = cb_context:client_ip(Context),
    {AccountId, ClientIp}.

has_tokens(Context, Count) ->
    {AccountId, ClientIp} = bucket_name(Context),
    has_tokens(AccountId, ClientIp, Count).
has_tokens(AccountId, ClientIp, Count) ->
    has_tokens(AccountId, ClientIp, Count, 'true').

has_tokens(AccountId, ClientIp, Count, StartBucketIfMissing) ->
    lager:debug("looking for token bucket for ~s/~s", [AccountId, ClientIp]),
    case ets:lookup(?MODULE, key(AccountId, ClientIp)) of
        [] when StartBucketIfMissing -> start_bucket(AccountId, ClientIp), 'true';
        [] -> 'false';
        [#bucket{srv=Srv}] -> kz_token_bucket:consume(Srv, Count)
    end.

tokens() ->
    lager:info("~60.s | ~20.s | ~10.s |", [<<"Key">>, <<"Pid">>, <<"Tokens">>]),
    tokens_traverse(ets:first(?MODULE)).
tokens_traverse('$end_of_table') ->
    lager:debug("~90.s", [<<"No more token servers">>]);
tokens_traverse(Key) ->
    [#bucket{key=K, srv=P}] = ets:lookup(?MODULE, Key),
    lager:info("~60.60s | ~20.20p | ~10.10p |", [K, P, kz_token_bucket:tokens(P)]),
    tokens_traverse(ets:next(?MODULE, Key)).

key_pos() -> #bucket.key.
table_id() -> ?MODULE.

key(AccountId, ClientIp) -> <<AccountId/binary, "/", ClientIp/binary>>.

-spec start_bucket(cb_context:context()) -> 'ok'.
-spec start_bucket(ne_binary(), ne_binary()) -> 'ok'.
-spec start_bucket(ne_binary(), ne_binary(), pos_integer()) -> 'ok'.
-spec start_bucket(ne_binary(), ne_binary(), pos_integer(), pos_integer()) -> 'ok'.
-spec start_bucket(ne_binary(), ne_binary(), pos_integer(), pos_integer(), kz_token_bucket:fill_rate_time()) -> 'ok'.

start_bucket(Context) ->
    {AccountId, ClientIp} = bucket_name(Context),
    start_bucket(AccountId, ClientIp).
start_bucket(AccountId, ClientIp) ->
    start_bucket(AccountId, ClientIp, ?MAX_TOKENS).
start_bucket(AccountId, ClientIp, MaxTokens) ->
    start_bucket(AccountId, ClientIp, MaxTokens, ?FILL_RATE).
start_bucket(AccountId, ClientIp, MaxTokens, FillRate) ->
    start_bucket(AccountId, ClientIp, MaxTokens, FillRate, 'second').
start_bucket(AccountId, ClientIp, MaxTokens, FillRate, FillTime) ->
    case has_bucket(AccountId, ClientIp) of
        'true' -> lager:debug("bucket exists for ~s/~s already", [AccountId, ClientIp]);
        'false' ->
            gen_server:cast(?MODULE, {'start', AccountId, ClientIp, MaxTokens, FillRate, FillTime})
    end.

-spec has_bucket(cb_context:context()) -> boolean().
-spec has_bucket(api_binary(), api_binary()) -> boolean().
has_bucket(Context) ->
    {AccountId, ClientIp} = bucket_name(Context),
    has_bucket(AccountId, ClientIp).
has_bucket(AccountId, ClientIp) ->
    case ets:lookup(?MODULE, key(AccountId, ClientIp)) of
        [] -> 'false';
        [#bucket{}] -> 'true';
        _O ->
            lager:error("has_bucket(~s, ~s) failed: ~p", [AccountId, ClientIp, _O]),
            'false'
    end.

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
    put('callid', ?MODULE),
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
handle_cast({'start', AccountId, ClientIp, MaxTokens, FillRate, FillTime}, #state{table_id=Tbl}=State) ->
    lager:debug("starting token bucket for ~s/~s (~b at ~b/~s)"
                ,[AccountId, ClientIp, MaxTokens, FillRate, FillTime]
               ),
    case cb_kz_buckets_sup:start_bucket(MaxTokens, FillRate, FillTime) of
        {'ok', Pid} when is_pid(Pid) ->
            case ets:insert_new(Tbl, new_bucket(Pid, AccountId, ClientIp)) of
                'true' -> lager:debug("new bucket for ~s/~s: ~p", [AccountId, ClientIp, Pid]);
                'false' ->
                    lager:debug("hmm, bucket appears to exist for ~s/~s, stopping ~p", [AccountId, ClientIp, Pid]),
                    cb_kz_buckets_sup:stop_bucket(Pid)
            end;
        _E -> lager:debug("error: starting bucket: ~p", [_E])
    end,
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
        N when N > 0 -> lager:debug("bucket ~p down: ~p", [_Reason]);
        0 -> lager:debug("unknown procress ~p(~p) down: ~p", [Pid, Ref, _Reason])
    end,
    {'noreply', State};
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
-spec new_bucket(pid(), api_binary(), api_binary()) -> bucket().
new_bucket(Pid, AccountId, ClientIp) ->
    #bucket{key=key(AccountId, ClientIp)
            ,srv=Pid
            ,ref=erlang:monitor('process', Pid)
           }.
