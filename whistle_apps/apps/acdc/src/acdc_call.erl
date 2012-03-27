%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Represents the caller as it progresses through the queue
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_call).

%% do somethings
-export([new/2            % create new caller record
         ,next_agent/1    % pull the next agent off the queue
         ,reload_agents/1 % if the list of agents is exhausted, reload with the latest version of agents available
         ,put_callid/1
        ]).

%% accessors
-export([call/1, callid/1, is_call/1
         ,connection_timeout/1
         ,member_timeout/1
         ,account_db/1
         ,queue_id/1
        ]).

-include("acdc.hrl").

-record(caller, {
          call     :: whapps_call:call()
         ,agents   :: acdc_agents:agents()
         ,queue_id :: ne_binary() % the queue id
         ,queue    :: wh_json:json_object() % the queue configuration
         ,acct_db  :: ne_binary()  % the account of the queue
         }).
-opaque caller() :: #caller{}.

-export_type([caller/0]).

%%%-------------------------------------------------------------------
%%% @public
%%% @doc Creates a new caller record for passing around agent processes
%%% @end
%%%-------------------------------------------------------------------
-spec new/2 :: (whapps_call:call(), ne_binary()) -> caller() | {'error', any()}.
new(Call, QueueId) ->
    AccountDb = whapps_call:account_db(Call),

    case acdc_util:find_queue(AccountDb, QueueId) of
        {error, _Reason}=E -> E;
        {ok, Queue} ->
            Agents = acdc_agents:get_agents(),

            #caller{call = Call
                    ,agents = Agents
                    ,queue_id = QueueId
                    ,queue = Queue
                    ,acct_db = AccountDb
                   }
    end.

%%%-------------------------------------------------------------------
%%% @public
%%% @doc Load the agents (with any updates) for interating over
%%% @end
%%%-------------------------------------------------------------------
-spec reload_agents/1 :: (caller()) -> caller().
reload_agents(#caller{}=Caller) ->
    Agents = acdc_agents:get_agents(),
    Caller#caller{agents=Agents}.

%%%-------------------------------------------------------------------
%%% @public
%%% @doc Return the next agent process, according to the queueing
%%%      strategy being employed.
%%% @end
%%%-------------------------------------------------------------------
-spec next_agent/1 :: (caller()) -> {pid(), caller()} | 'undefined'.
next_agent(#caller{agents=As}=Caller) ->
    case acdc_agents:next_agent_please(As) of
        undefined -> undefined;
        {A, As1} -> {A, Caller#caller{agents=As1}}
    end.

-spec put_callid/1 :: (caller()) -> any().
put_callid(#caller{call=Call}) ->
    put(callid, whapps_call:call_id(Call)).

%%%-------------------------------------------------------------------
%%% @public
%%% @doc Return how long the caller should remain in the queue before
%%%      progressing to another action
%%% @end
%%%-------------------------------------------------------------------
-spec connection_timeout/1 :: (caller()) -> pos_integer().
connection_timeout(#caller{queue=Queue}) ->
    wh_json:get_integer_value(<<"connection_timeout">>, Queue, 300) * 1000.

%%%-------------------------------------------------------------------
%%% @public
%%% @doc Returns the whapps_call:call() object
%%% @end
%%%-------------------------------------------------------------------
-spec call/1 :: (caller()) -> whapps_call:call().
call(#caller{call=Call}) ->
    Call.

%%%-------------------------------------------------------------------
%%% @public
%%% @doc Returns the callid of the call
%%% @end
%%%-------------------------------------------------------------------
-spec callid/1 :: (caller()) -> ne_binary().
callid(#caller{call=Call}) ->
    whapps_call:call_id(Call).

%%%-------------------------------------------------------------------
%%% @public
%%% @doc Returns if the parameter is a caller()
%%% @end
%%%-------------------------------------------------------------------
-spec is_call/1 :: (caller()) -> boolean().
is_call(#caller{}) -> true;
is_call(_) -> false.

%%%-------------------------------------------------------------------
%%% @public
%%% @doc How long to ring the agent (member) before moving to the next
%%% @end
%%%-------------------------------------------------------------------
-spec member_timeout/1 :: (caller()) -> ne_binary().
member_timeout(#caller{queue=Queue}) ->
    wh_json:get_ne_value(<<"member_timeout">>, Queue, <<"5">>).

%%%-------------------------------------------------------------------
%%% @public
%%% @doc Returns the account db of the call queue
%%% @end
%%%-------------------------------------------------------------------
-spec account_db/1 :: (caller()) -> ne_binary().
account_db(#caller{acct_db=Db}) ->
    Db.

%%%-------------------------------------------------------------------
%%% @public
%%% @doc Returns the queue id of the call queue
%%% @end
%%%-------------------------------------------------------------------
-spec queue_id/1 :: (caller()) -> ne_binary().
queue_id(#caller{queue_id=QID}) ->
    QID.

