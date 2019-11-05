%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(gcm).
-behaviour(gen_server).

-export([start/2, stop/1, start_link/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([push/3, push/4, sync_push/3, sync_push/4]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").

-define(SERVER, ?MODULE).
-define(RETRY, 3).

-record(state, {key :: nonempty_string()
               }).



-spec start(atom(), any()) -> {ok, undefined | pid()} | {error, any()}.
start(Name, Key) when is_binary(Key) ->
    start(Name, kz_term:to_list(Key));
start(Name, Key) ->
    gcm_sup:start_child(Name, Key).

-spec stop(atom()) -> stopped.
stop(Name) ->
    gen_server:call(Name, stop).

-spec push(kz_types:server_ref(), list(), any()) -> ok.
push(Name, RegIds, Message) ->
    push(Name, RegIds, Message, ?RETRY).

-spec push(kz_types:server_ref(), list(), any(), non_neg_integer()) -> ok.
push(Name, RegIds, Message, Retry) ->
    gen_server:cast(Name, {send, RegIds, Message, Retry}).

-spec sync_push(atom(), list(), non_neg_integer()) -> ok | {ok, any()} | {error, any()}.
sync_push(Name, RegIds, Message) ->
    sync_push(Name, RegIds, Message, ?RETRY).

-spec sync_push(atom(), list(), any(), non_neg_integer()) -> ok | {ok, any()} | {error, any()}.
sync_push(Name, RegIds, Message, Retry) ->
    gen_server:call(Name, {send, RegIds, Message, Retry}).

%% OTP

-spec start_link(atom(), any()) -> {ok, pid()} | {error, any()}.
start_link(Name, Key) ->
    gen_server:start_link({local, Name}, ?MODULE, [Key], []).

-spec init(any()) -> {ok, #state{}} | {ok, #state{}, non_neg_integer()} | {ok, #state{}, hibernate} | {stop, any()} | ignore.
init([Key]) ->
    kz_log:put_callid(?MODULE),
    lager:debug("starting with key ~s", [Key]),
    {ok, #state{key=Key}}.

-spec handle_call(any(), {pid(),any()}, #state{}) -> {reply, any(), #state{}} | {reply, any(), #state{}, non_neg_integer()} | {reply, any(), #state{}, hibernate} | {noreply, #state{}} | {noreply, #state{}, non_neg_integer()} | {noreply, #state{}, hibernate} | {stop, any(), any(), #state{}} | {stop, any(), #state{}}.
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({send, RegIds, Message, Retry}, _From, #state{key=Key} = State) ->
    Reply = do_push(RegIds, Message, Key, Retry),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

-spec handle_cast(any(), #state{}) -> {noreply, #state{}} | {noreply, #state{}, non_neg_integer()} | {noreply, #state{}, hibernate} | {stop, any(), #state{}}.
handle_cast({send, RegIds, Message, Retry}, #state{key=Key} = State) ->
    _ = do_push(RegIds, Message, Key, Retry),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(any(), #state{}) -> {noreply, #state{}} | {noreply, #state{}, non_neg_integer()} | {noreply, #state{}, hibernate} | {stop, any(), #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(any(), #state{}) -> any().
terminate(_Reason, _State) ->
    ok.

-spec code_change(any(), #state{}, any()) -> {ok, any()} | {error, any()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal
do_push(_RegIds, _, _, 0) ->
    lager:info("retries exceeded for ~p", [_RegIds]);
do_push(RegIds, Message, Key, Retry) ->
    lager:info("trying to push to ~p(~p): ~p", [RegIds, Key, Message]),
    case gcm_api:push(RegIds, Message, Key) of
        {ok, _}=R ->
            lager:info("pushed: ~p", [R]),
            R;
        {error, {retry, RetryAfter}} ->
            lager:info("retry after: ~p", [RetryAfter]),
            _ = do_backoff(RetryAfter, RegIds, Message, Key, Retry),
            {error, retry};
        {error, Reason} = Error ->
            lager:info("error pushing: ~p", [Reason]),
            Error
    end.

%% handle_result(GCMResult, RegIds) ->
%%     {_MulticastId, _SuccessesNumber, _FailuresNumber, _CanonicalIdsNumber, Results} = GCMResult,
%%     lists:map(fun({Result, RegId}) -> {RegId, parse(Result)} end, lists:zip(Results, RegIds)).

do_backoff(RetryAfter, RegIds, Message, Key, Retry) ->
    case RetryAfter of
        no_retry ->
            ok;
        _ ->
            error_logger:info_msg("Received retry-after. Will retry: ~p times~n", [Retry-1]),
            timer:apply_after(RetryAfter * 1000, ?MODULE, do_push, [RegIds, Message, Key, Retry - 1])
    end.

%% parse(Result) ->
%%     case {
%%       props:get_value(<<"error">>, Result),
%%       props:get_value(<<"message_id">>, Result),
%%       props:get_value(<<"registration_id">>, Result)
%%      } of
%%         {Error, undefined, undefined} ->
%%             Error;
%%         {undefined, _MessageId, undefined}  ->
%%             ok;
%%         {undefined, _MessageId, NewRegId} ->
%%             {<<"NewRegistrationId">>, NewRegId}
%%     end.
