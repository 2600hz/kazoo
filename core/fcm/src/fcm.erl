-module(fcm).
-behaviour(gen_server).

-include("logger.hrl").

-export([start/2, stop/1, start_link/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([push/3, push/4, sync_push/3, sync_push/4]).

-define(SERVER, ?MODULE).
-define(RETRY, 3).

-record(state, {key}).

start(Name, Key) ->
    fcm_sup:start_child(Name, Key).

stop(Name) ->
    gen_server:call(Name, stop).

push(Name, RegIds, Message) ->
    push(Name, RegIds, Message, ?RETRY).

push(Name, RegIds, Message, Retry) ->
    gen_server:cast(Name, {send, RegIds, Message, Retry}).

sync_push(Name, RegIds, Message) ->
    sync_push(Name, RegIds, Message, ?RETRY).

sync_push(Name, RegIds, Message, Retry) ->
    gen_server:call(Name, {send, RegIds, Message, Retry}).

%% OTP
start_link(Name, Key) ->
    gen_server:start_link({local, Name}, ?MODULE, [Key], []).

init([Key]) ->
    GoogleKey = string:concat("key=", Key),
    {ok, #state{key = GoogleKey}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({send, RegIds, Message, Retry}, _From, #state{key=Key} = State) ->
    Reply = do_push(RegIds, Message, Key, Retry),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({send, RegIds, Message, Retry}, #state{key=Key} = State) ->
    do_push(RegIds, Message, Key, Retry),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_push(RegIds, Message, Key, Retry) ->
    ?INFO_MSG("Sending message: ~p to reg ids: ~p retries: ~p.~n", [Message, RegIds, Retry]),
    case fcm_api:push(RegIds, Message, Key) of
        {ok, GCMResult} ->
            handle_result(GCMResult, RegIds);
        {error, {retry, RetryAfter}} ->
            do_backoff(RetryAfter, RegIds, Message, Key, Retry - 1),
            {error, retry};
        {error, Reason} ->
            {error, Reason}
    end.

handle_result(GCMResult, RegId) when is_binary(RegId) ->
    handle_result(GCMResult, [RegId]);
handle_result(GCMResult, RegIds) ->
    {_MulticastId, _SuccessesNumber, _FailuresNumber, _CanonicalIdsNumber, Results} = GCMResult,
    lists:map(fun({Result, RegId}) -> {RegId, parse(Result)} end, lists:zip(Results, RegIds)).

do_backoff(RetryAfter, RegIds, Message, Key, Retry) when (Retry >= 0) ->
    case RetryAfter of
        no_retry -> ok;
        _ ->
            ?INFO_MSG("Received retry-after. Will retry: ~p times~n", [Retry]),
            timer:apply_after(RetryAfter * 1000, ?MODULE, do_push, [RegIds, Message, Key, Retry])
    end;
do_backoff(_, _, _, _, _) -> ok.

parse(Result) ->
    case {
      proplists:get_value(<<"error">>, Result),
      proplists:get_value(<<"message_id">>, Result),
      proplists:get_value(<<"registration_id">>, Result)
     } of
        {Error, undefined, undefined} ->
            Error;
        {undefined, _MessageId, undefined}  ->
            ok;
        {undefined, _MessageId, NewRegId} ->
            {<<"NewRegistrationId">>, NewRegId}
    end.
