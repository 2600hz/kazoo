%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_cb_comments).

%% Manual testing
-export([seq/0
        ]).

%% API Shims

-include_lib("proper/include/proper.hrl").
-include("kazoo_proper.hrl").


-spec seq() -> 'ok'.
seq() ->
    API = init_api(),
    ?INFO("API ~p~n~n", [API]).

init_api() ->
    Model = initial_state(),
    pqc_kazoo_model:api(Model).

-spec initial_state() -> pqc_kazoo_model:model().
initial_state() ->
    _ = init_system(),
    API = pqc_cb_api:authenticate(),
    pqc_kazoo_model:new(API).

init_system() ->
    TestId = kz_binary:rand_hex(5),
    kz_util:put_callid(TestId),

    _ = kz_data_tracing:clear_all_traces(),
    _ = [kapps_controller:start_app(App) ||
            App <- ['crossbar']
        ],
    _ = [crossbar_maintenance:start_module(Mod) ||
            Mod <- ['cb_cdrs']
        ],

    ?INFO("INIT FINISHED").
