-module(gcm).
-behaviour(gen_server).

-export([start/2, stop/1, start_link/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([push/3, push/4, sync_push/3, sync_push/4]).

-include_lib("whistle/include/wh_log.hrl").

-define(SERVER, ?MODULE).
-define(RETRY, 3).

-record(state, {key}).

start(Name, Key) when is_binary(Key) ->
    start(Name, wh_util:to_list(Key));
start(Name, Key) ->
    gcm_sup:start_child(Name, Key).

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
    wh_util:put_callid(?MODULE),
    lager:debug("starting with key ~s", [Key]),
    {ok, #state{key=Key}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({send, RegIds, Message, Retry}, _From, #state{key=Key} = State) ->
    Reply = do_push(RegIds, Message, Key, Retry),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
            do_backoff(RetryAfter, RegIds, Message, Key, Retry),
            {error, retry};
        {error, Reason} ->
            lager:info("error pushing: ~p", [Reason]),
            {error, Reason}
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
%%       proplists:get_value(<<"error">>, Result),
%%       proplists:get_value(<<"message_id">>, Result),
%%       proplists:get_value(<<"registration_id">>, Result)
%%      } of
%%         {Error, undefined, undefined} ->
%%             Error;
%%         {undefined, _MessageId, undefined}  ->
%%             ok;
%%         {undefined, _MessageId, NewRegId} ->
%%             {<<"NewRegistrationId">>, NewRegId}
%%     end.
