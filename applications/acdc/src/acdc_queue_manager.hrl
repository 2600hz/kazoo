-ifndef(ACDC_QUEUE_MANAGER_HRL).

%% rr :: Round Robin
%% mi :: Most Idle
-type queue_strategy() :: 'rr' | 'mi' | 'sbrr' | 'all'.

-type sbrr_skill_map() :: #{kz_term:ne_binaries() := sets:set()}. % skill keys must be alphabetically sorted
-type sbrr_id_map() :: #{kz_term:ne_binary() := kz_term:ne_binary()}.
-type sbrr_strategy_state() :: #{agent_id_map := sbrr_id_map() % maps agent IDs to assigned call IDs
                                ,call_id_map := sbrr_id_map() % maps assigned call IDs to agent IDs
                                ,rr_queue := pqueue4:queue()
                                ,skill_map := sbrr_skill_map()
                                }.

-type queue_strategy_state() :: pqueue4:queue() | kz_term:ne_binaries() | sbrr_strategy_state().
-type ss_details() :: {non_neg_integer(), 'ringing' | 'busy' | 'undefined'}.
-record(strategy_state, {agents :: queue_strategy_state()
                                   %% details include # of agent processes and availability
                        ,details = dict:new() :: dict:dict()
                        ,ringing_agents = [] :: kz_term:ne_binaries()
                        ,busy_agents = [] :: kz_term:ne_binaries()
                        }).
-type strategy_state() :: #strategy_state{}.

-record(state, {ignored_member_calls = dict:new() :: dict:dict()
               ,account_id :: api_kz_term:ne_binary()
               ,queue_id :: api_kz_term:ne_binary()
               ,supervisor :: kz_term:api_pid()
               ,strategy = 'rr' :: queue_strategy() % round-robin | most-idle
               ,strategy_state = #strategy_state{} :: strategy_state() % based on the strategy
               ,enter_when_empty = 'true' :: boolean() % allow caller into queue if no agents are logged in
               ,moh :: api_kz_term:ne_binary()
               ,current_member_calls = [] :: list() % ordered list of current members waiting
               ,announcements_config = [] :: kz_term:proplist()
               ,announcements_pids = #{} :: announcements_kz_term:pids()
               ,registered_callbacks = [] :: list()
               }).
-type mgr_state() :: #state{}.

-define(ACDC_REQUIRED_SKILLS_KEY, 'acdc_required_skills').

-define(ACDC_QUEUE_MANAGER_HRL, 'true').
-endif.
