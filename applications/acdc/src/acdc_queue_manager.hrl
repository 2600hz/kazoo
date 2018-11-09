-ifndef(ACDC_QUEUE_MANAGER_HRL).

%% rr :: Round Robin
%% mi :: Most Idle
-type queue_strategy() :: 'rr' | 'mi' | 'all'.

-type queue_strategy_state() :: queue:queue() | kz_term:ne_binaries().
-type ss_details() :: {non_neg_integer(), 'busy' | 'undefined'}.
-record(strategy_state, {agents :: queue_strategy_state() | 'undefined'
                                   %% details include # of agent processes and availability
                        ,details = dict:new() :: dict:dict(kz_term:ne_binary(), ss_details())
                        }).
-type strategy_state() :: #strategy_state{}.

-record(state, {ignored_member_calls = dict:new() :: dict:dict()
               ,account_id :: kz_term:api_ne_binary()
               ,queue_id :: kz_term:api_ne_binary()
               ,supervisor :: kz_term:api_pid()
               ,strategy = 'rr' :: queue_strategy() % round-robin | most-idle
               ,strategy_state = #strategy_state{} :: strategy_state() % based on the strategy
               ,known_agents = dict:new() :: dict:dict() % how many agent processes are available {AgentId, Count}
               ,enter_when_empty = 'true' :: boolean() % allow caller into queue if no agents are logged in
               ,moh :: kz_term:api_ne_binary()
               ,current_member_calls = [] :: list() % ordered list of current members waiting
               ,announcements_config = [] :: kz_term:proplist()
               ,announcements_pids = #{} :: announcements_pids()
               }).
-type mgr_state() :: #state{}.

-define(ACDC_QUEUE_MANAGER_HRL, 'true').
-endif.
