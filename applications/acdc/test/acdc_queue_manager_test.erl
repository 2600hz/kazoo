%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, Voxter Communication Inc.
%%% @doc
%%% @end
%%% @contributors
%%%   Daniel Finke
%%%-------------------------------------------------------------------
-module(acdc_queue_manager_test).

-export([init/1, init/4
        ,create_strategy_state/4
        ,most_recent_status/2
        ]).

-include_lib("eunit/include/eunit.hrl").

-include("../src/acdc.hrl").
-include("../src/acdc_queue_manager.hrl").

-define(AGENT_ID, <<"agent_id">>).
-define(AGENTS, [<<"95101aa694278465cdea6c6c097778b1">>]).
-define(QUEUE_JOBJ, kz_json:from_list([{<<"_id">>, <<"0e279b77f708747e91f644df7beaa679">>}
                                                %,{<<"_rev">>, <<"20-e2bbc5d2f91d51318f3dee45bb1c9fe1">>}
                                      ,{<<"connection_timeout">>, <<"0">>}
                                      ,{<<"member_timeout">>, <<"5">>}
                                      ,{<<"agent_wrapup_time">>, <<"30">>}
                                      ,{<<"record_caller">>, true}
                                      ,{<<"moh">>, <<"">>}
                                      ,{<<"notifications">>, []}
                                      ,{<<"max_queue_size">>, <<"0">>}
                                      ,{<<"position_announcements_enabled">>, false}
                                      ,{<<"holdtime_announcements_enabled">>, false}
                                      ,{<<"announcements_timer">>, <<"30">>}
                                      ,{<<"name">>, <<"PL English">>}
                                      ,{<<"strategy">>, <<"most_idle">>}
                                      ,{<<"preserve_metadata">>, true}
                                      ,{<<"enter_when_empty">>, true}
                                      ,{<<"ui_metadata">>, [{<<"ui">>, <<"kazoo-ui">>}]}
                                      ,{<<"agent_ring_timeout">>, 15}
                                      ,{<<"ring_simultaneously">>, 1}
                                      ,{<<"caller_exit_key">>, <<"#">>}
                                                % ,{<<"_id">>, <<"0e279b77f708747e91f644df7beaa679">>}
                                                % ,{<<"pvt_is_authenticated">>, true}
                                                % ,{<<"pvt_auth_account_id">>, <<"8ffdf424a9f2a32e09e042ddc603d9b5">>}
                                                % ,{<<"pvt_auth_user_id">>, <<"1041fb18ff1cda7f7db9c6554fc40202">>}
                                                % ,{<<"pvt_type">>, <<"queue">>}
                                                % ,{<<"pvt_vsn">>, <<"1">>}
                                      ,{<<"pvt_account_id">>, <<"af3e2f0f5d546e9d9fcf5f96ea69f6d5">>}
                                                % ,{<<"pvt_account_db">>, <<"account%2Faf%2F3e%2F2f0f5d546e9d9fcf5f96ea69f6d5">>}
                                                % ,{<<"pvt_created">>, 63606198815}
                                                % ,{<<"pvt_modified">>, 63627009716}
                                                % ,{<<"pvt_request_id">>, <<"fc7fa83d411e3b7a6e051c51019c8a70">>}
                                      ])).

%%% =====
%%% TESTS
%%% =====

%%% Fixtures
init_test_() ->
    {"Test that acdc_queue_manager can create strategy state"
    ,{'setup'
     ,fun setup_init_session/0
     ,fun teardown_init_session/1
     ,fun(Pid) ->
              {'timeout', 120
              ,[t_init(Pid)]
              }
      end
     }
    }.

ss_size_test_() ->
    {"Make sure that ss_size returns the right values"
    ,{'setup'
     ,fun setup_ss_size_session/0
     ,fun teardown_ss_size_session/1
     ,fun(_) ->
              [t_ss_size_empty()
              ,t_ss_size_one_busy()
              ]
      end
     }
    }.

%%% Fixtures' setup/teardown
setup_init_session() ->
    process_flag('trap_exit', 'true'),
    start_deps(),

    meck:new('acdc_queue_manager', ['passthrough']),
    meck:expect('acdc_queue_manager', 'init'
               ,fun init/1),
    meck:expect('acdc_queue_manager', 'init'
               ,fun init/4),
    meck:expect('acdc_queue_manager', 'handle_cast'
               ,fun handle_cast/2),
    meck:expect('acdc_queue_manager', 'create_strategy_state'
               ,fun create_strategy_state/4),

    {'ok', Pid} = acdc_queue_manager:start_link(self(), ?QUEUE_JOBJ),
    Pid.

setup_ss_size_session() ->
    meck:new('acdc_queue_manager', ['passthrough']),
    meck:expect('acdc_queue_manager', 'create_strategy_state'
               ,fun create_strategy_state/4),
    'ok'.

teardown_init_session(_Pid) ->
    meck:unload('acdc_queue_manager').

teardown_ss_size_session(_) ->
    meck:unload('acdc_queue_manager').

start_deps() ->
    application:load('acdc'),
    {'ok', Deps} = application:get_key('acdc', 'applications'),
    lists:foreach(fun(Dep) ->
                          case application:ensure_all_started(Dep) of
                              {'error', {'already_started', Dep}} -> 'ok';
                              {'error', _}=E -> throw(E);
                              _ -> 'ok'
                          end
                  end
                 ,Deps).

%%% Actual test functions
t_init(Pid) ->
    ?_assertEqual(?AGENTS, acdc_queue_manager:current_agents(Pid)).

t_ss_size_empty() ->
    SS = #strategy_state{agents=[]},
    [?_assertEqual(0, acdc_queue_manager:ss_size(SS, 'free'))
    ,?_assertEqual(0, acdc_queue_manager:ss_size(SS, 'logged_in'))].

t_ss_size_one_busy() ->
    SS = #strategy_state{agents=[]},
    SS1 = acdc_queue_manager:update_strategy_with_agent('mi', SS, ?AGENT_ID, 'add', 'undefined'),
    SS2 = acdc_queue_manager:update_strategy_with_agent('mi', SS1, ?AGENT_ID, 'remove', 'busy'),
    [?_assertEqual(0, acdc_queue_manager:ss_size(SS2, 'free'))
    ,?_assertEqual(1, acdc_queue_manager:ss_size(SS2, 'logged_in'))].

%%% =====
%%% MOCKS
%%% =====

init([Super, QueueJObj]) ->
    AccountId = kz_doc:account_id(QueueJObj),
    QueueId = kz_doc:id(QueueJObj),

    kz_util:put_callid(<<"mgr_", QueueId/binary>>),

    acdc_queue_manager:init(Super, AccountId, QueueId, QueueJObj).

init(Super, AccountId, QueueId, QueueJObj) ->
    process_flag('trap_exit', 'false'),

    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
                                                % kz_datamgr:add_to_doc_cache(AccountDb, QueueId, QueueJObj),

                                                % _ = start_secondary_queue(AccountId, QueueId),

                                                % gen_listener:cast(self(), {'start_workers'}),
    Strategy = acdc_queue_manager:get_strategy(kz_json:get_value(<<"strategy">>, QueueJObj)),
    StrategyState = acdc_queue_manager:create_strategy_state(Strategy, #strategy_state{}, AccountDb, QueueId),

    _ = acdc_queue_manager:update_strategy_state(self(), Strategy, StrategyState),

    meck:new('acdc_agent_util'),
    meck:expect('acdc_agent_util', 'most_recent_status'
               ,fun most_recent_status/2),

    lager:debug("queue mgr started for ~s", [QueueId]),
    {'ok', acdc_queue_manager:update_properties(QueueJObj, #state{account_id=AccountId
                                                                 ,queue_id=QueueId
                                                                 ,supervisor=Super
                                                                 ,strategy=Strategy
                                                                 ,strategy_state=StrategyState
                                                                 })}.

handle_cast({'agent_available', AgentId}, #state{strategy=Strategy
                                                ,strategy_state=StrategyState
                                                % ,supervisor=QueueSup
                                                }=State) when is_binary(AgentId) ->
    lager:info("adding agent ~s to strategy ~s", [AgentId, Strategy]),
    StrategyState1 = acdc_queue_manager:update_strategy_with_agent(Strategy, StrategyState, AgentId, 'add', 'undefined'),
                                                % maybe_start_queue_workers(QueueSup, ss_size(StrategyState1, 'logged_in')),
    {'noreply', State#state{strategy_state=StrategyState1}
    ,'hibernate'};
handle_cast(Req, State) -> meck:passthrough([Req, State]).

create_strategy_state('mi', #strategy_state{agents='undefined'}=SS, AcctDb, QueueId) ->
    create_strategy_state('mi', SS#strategy_state{agents=[]}, AcctDb, QueueId);
create_strategy_state('mi', SS, _, _) ->
    SS#strategy_state{agents=?AGENTS}.

most_recent_status(_, _) -> {'ok', <<"ready">>}.
