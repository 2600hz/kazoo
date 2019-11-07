%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2019, 2600Hz
%%% @doc API interface for buckets
%%% ETS writer for table
%%%
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_buckets).

%% API exports
-export([start_link/0
        ,consume_token/1, consume_token/2
        ,consume_tokens/2, consume_tokens/3

        ,consume_tokens_until/2, consume_tokens_until/3, consume_tokens_until/4

        ,start_bucket/1, start_bucket/2, start_bucket/3, start_bucket/4, start_bucket/5
        ,exists/1, exists/2
        ,tokens/0
        ,tokens_remaining/2

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

-record(state, {table_id :: ets:tid() | 'undefined'
               ,inactivity_timer_ref :: reference()
               }).
-type state() :: #state{}.

-record(bucket, {key :: {kz_term:ne_binary(), kz_term:ne_binary()} | '_'
                ,srv :: pid() | '$1' | '$2' | '_'
                ,ref :: reference() | '$2' | '_'
                ,accessed = kz_time:now_s() :: kz_time:gregorian_seconds() | '$1' | '_'
                }).
-type bucket() :: #bucket{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_server:start_link({'local', ?SERVER}, ?MODULE, [], []).

%% consume_tokens(Name, [Count, [StartIfMissing]])
%% Name :: name of the bucket
%% Count :: how many tokens to try to consume
%% StartIfMissing :: start the token bucket if it doesn't exist yet

-spec consume_token(kz_term:ne_binary()) -> boolean().
consume_token(Name) ->
    consume_tokens(Name, 1).

-spec consume_token(kz_term:ne_binary(), kz_term:ne_binary() | boolean()) -> boolean().
consume_token(<<_/binary>> = App, <<_/binary>> = Name) ->
    consume_tokens(App, Name, 1);
consume_token(Name, StartIfMissing) ->
    consume_tokens(?DEFAULT_APP, Name, 1, StartIfMissing).

-spec consume_tokens(kz_term:ne_binary(), integer()) -> boolean().
consume_tokens(Key, Count) ->
    consume_tokens(Key, Count, 'true').

-spec consume_tokens(kz_term:ne_binary(), kz_term:ne_binary() | integer(), integer() | boolean()) -> boolean().
consume_tokens(<<_/binary>> = App, <<_/binary>> = Key, Count) when is_integer(Count) ->
    consume_tokens(App, Key, Count, 'true');
consume_tokens(<<_/binary>> = Key, Count, StartIfMissing) when is_integer(Count),
                                                               is_boolean(StartIfMissing)
                                                               ->
    consume_tokens(?DEFAULT_APP, Key, Count, StartIfMissing).

-spec consume_tokens(kz_term:ne_binary(), kz_term:ne_binary(), integer(), boolean()) -> boolean().
consume_tokens(App, Key, Count, StartIfMissing) ->
    consume_tokens(App, Key, Count, StartIfMissing, fun kz_token_bucket:consume/2).

%% consume_tokens_until(Name, Count, [StartIfMissing])
%% Name :: name of the bucket
%% Count :: how many tokens to try to consume
%% StartIfMissing :: start the token bucket if it doesn't exist yet

%%------------------------------------------------------------------------------
%% @doc If Bucket is started and has fewer than Count tokens, consume the
%% remaining tokens and return `false'.
%% @end
%%------------------------------------------------------------------------------

-spec consume_tokens_until(kz_term:ne_binary(), pos_integer()) -> boolean().
consume_tokens_until(Key, Count) ->
    consume_tokens_until(?DEFAULT_APP, Key, Count, 'true').

-spec consume_tokens_until(kz_term:ne_binary(), kz_term:ne_binary() | pos_integer(), pos_integer() | boolean()) -> boolean().
consume_tokens_until(App=?NE_BINARY, Key=?NE_BINARY, Count)
  when is_integer(Count) ->
    consume_tokens_until(App, Key, Count, 'true');
consume_tokens_until(Key=?NE_BINARY, Count, StartIfMissing)
  when is_integer(Count),
       is_boolean(StartIfMissing) ->
    consume_tokens(?DEFAULT_APP, Key, Count, StartIfMissing).

-spec consume_tokens_until(kz_term:ne_binary(), kz_term:ne_binary(), pos_integer(), boolean()) -> boolean().
consume_tokens_until(App=?NE_BINARY, Key=?NE_BINARY, Count, StartIfMissing)
  when is_integer(Count),
       is_boolean(StartIfMissing) ->
    consume_tokens(App, Key, Count, StartIfMissing, fun kz_token_bucket:consume_until/2).

-spec consume_tokens(kz_term:ne_binary(), kz_term:ne_binary(), integer(), boolean(), fun()) -> boolean().
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

-spec maybe_start_bucket(kz_term:ne_binary(), kz_term:ne_binary(), integer(), boolean(), fun()) -> boolean().
maybe_start_bucket(_App, _Key, _Count, 'false', _BucketFun) -> 'false';
maybe_start_bucket(App, Key, Count, 'true', BucketFun) ->
    lager:debug("bucket (~s ~s) missing, starting", [App, Key]),
    case start_bucket(App, Key) of
        'error' -> 'false';
        _OK -> consume_tokens(App, Key, Count, 'false', BucketFun)
    end.

-spec get_bucket(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:api_pid().
get_bucket(App, Key) ->
    get_bucket(App, Key, 'server').

-spec get_bucket(kz_term:ne_binary(), kz_term:ne_binary(), 'record'|'server') -> kz_term:api_pid() | bucket().
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

-spec exists(kz_term:ne_binary()) -> boolean().
exists(Key) ->
    exists(?DEFAULT_APP, Key).

-spec exists(kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
exists(App, Key) ->
    case ets:lookup(table_id(), {App, Key}) of
        [] -> 'false';
        [#bucket{}] -> 'true';
        _O ->
            lager:error("exists(~s, ~s) failed: ~p", [App, Key, _O]),
            'false'
    end.

-spec start_bucket(kz_term:ne_binary()) ->
                          'ok' | 'error' | 'exists'.
start_bucket(Name) ->
    start_bucket(?DEFAULT_APP, Name).

-spec start_bucket(kz_term:ne_binary(), kz_term:ne_binary()) ->
                          'ok' | 'error' | 'exists'.
start_bucket(App, Name) ->
    start_bucket(App, Name, ?MAX_TOKENS(App)).

-spec start_bucket(kz_term:ne_binary(), kz_term:ne_binary(), pos_integer()) ->
                          'ok' | 'error' | 'exists'.
start_bucket(App, Name, MaxTokens) ->
    start_bucket(App, Name, MaxTokens, ?FILL_RATE(App)).

-spec start_bucket(kz_term:ne_binary(), kz_term:ne_binary(), pos_integer(), pos_integer()) ->
                          'ok' | 'error' | 'exists'.
start_bucket(App, Name, MaxTokens, FillRate) ->
    start_bucket(App, Name, MaxTokens, FillRate, kz_token_bucket:default_fill_time(App)).

-spec start_bucket(kz_term:ne_binary(), kz_term:ne_binary(), pos_integer(), pos_integer(), kz_token_bucket:fill_rate_time()) ->
                          'ok' | 'error' | 'exists'.
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
              ,kz_time:pretty_print_elapsed_s(kz_time:elapsed_s(Accessed))
              ]
             ),
    CurrentApp;
print_bucket_info(#bucket{key={App, _}}=Bucket, _OldApp) ->
    io:format(?TOKEN_FORMAT_STRING
             ,[App, "", "", "" ,""]
             ),
    print_bucket_info(Bucket, App).

-spec tokens_remaining(kz_term:ne_binary(), kz_term:ne_binary()) -> non_neg_integer().
tokens_remaining(App, Key) ->
    case ets:lookup(table_id(), {App, Key}) of
        [] ->
            ?MAX_TOKENS(App);
        [#bucket{srv=P}] ->
            kz_token_bucket:tokens(P)
    end.

%%%=============================================================================
%%% ETS
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec table_id() -> atom().
table_id() -> ?MODULE.

-spec table_options() -> list().
table_options() ->
    ['named_table' | kazoo_etsmgr_srv:default_table_options()].

-spec gift_data() -> 'ok'.
gift_data() -> 'ok'.

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    kz_log:put_callid(?MODULE),
    {'ok', #state{inactivity_timer_ref=start_inactivity_timer()}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call({'start', _App, _Name, _MaxTokens, _FillRate, _FillTime}
           ,_From
           ,#state{table_id='undefined'}=State
           ) ->
    lager:debug("not starting token bucket for ~p ~p, table not ready"
               ,[_App, _Name]
               ),
    {'reply', 'error', State};
handle_call({'start', App, Name, MaxTokens, FillRate, FillTime}, _From, #state{table_id=Tbl}=State) ->
    lager:debug("maybe starting token bucket for ~s, ~s (~b at ~b/~s)"
               ,[App, Name, MaxTokens, FillRate, FillTime]
               ),
    case not exists(App, Name)
        andalso kz_buckets_sup:start_bucket(MaxTokens, FillRate, FillTime)
    of
        {'ok', Pid} when is_pid(Pid) ->
            T = {App, Name},
            _ = case ets:insert_new(Tbl, new_bucket(Pid, T)) of
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

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast(_Req, #state{table_id='undefined'}=State) ->
    lager:debug("ignoring req: ~p", [_Req]),
    {'noreply', State};
handle_cast({'bucket_accessed', Key}, State) ->
    ets:update_element(table_id(), Key, {#bucket.accessed, kz_time:now_s()}),
    {'noreply', State};
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
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
        0 -> lager:debug("unknown process ~p(~p) down: ~p", [Pid, Ref, _Reason])
    end,
    {'noreply', State};
handle_info(?INACTIVITY_MSG, #state{inactivity_timer_ref=_OldRef}=State) ->
    _Pid = kz_process:spawn(fun check_for_inactive_buckets/0),
    {'noreply', State#state{inactivity_timer_ref=start_inactivity_timer()}};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

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
    lager:debug("bucket ets mgr going down: ~p", [_Reason]).

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
-spec new_bucket(pid(), {kz_term:ne_binary(), kz_term:ne_binary()}) -> bucket().
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
    kz_log:put_callid(?MODULE),
    Now = kz_time:now_s(),
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
              kz_term:to_binary(Srv)
          end
          || Srv <- ets:select(table_id(), MS)
         ]
    of
        [] -> 'ok';
        L -> lager:debug("stopped servers ~s", [kz_binary:join(L)])
    end.
