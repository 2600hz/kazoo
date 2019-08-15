%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_cb_system_configs).

-export([seq/0
        ,cleanup/0, cleanup/1
        ,default/0, init_db/0, cleanup_db/0
        ]
       ).

%% API
-export([list_configs/1
        ,set_default_config/2
        ,patch_default_config/3
        ,get_config/2
        ,get_default_config/2
        ,get_node_config/3
        ,delete_config/2
        ]).

-include("kazoo_proper.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").

-define(CATEGORY_ID, <<"kazoo_proper">>).
-define(SCHEMA_ID, <<"system_config.kazoo_proper">>).
-define(NODE_ID, <<"foo@bar.com">>).

-define(KEY_SCHEMA, kz_json:from_list([{<<"type">>, <<"string">>}
                                      ,{<<"default">>, <<"default_key">>}
                                      ])
       ).
-define(KNEE_SCHEMA, kz_json:from_list([{<<"type">>, <<"string">>}
                                       ,{<<"default">>, <<"default_knee">>}
                                       ])
       ).

-define(NESTED_SCHEMA, kz_json:from_list([{<<"type">>, <<"object">>}
                                         ,{<<"properties">>, kz_json:from_list([{<<"knee">>, ?KNEE_SCHEMA}])}
                                         ,{<<"default">>, kz_json:from_list([{<<"knee">>, <<"default_knee">>}])}
                                         ])
       ).

-define(CONFIG_SCHEMA
       ,kz_json:from_list([{<<"_id">>, ?SCHEMA_ID}
                          ,{<<"$schema">>, <<"http://json-schema.org/draft-04/schema#">>}
                          ,{<<"properties">>, kz_json:from_list([{<<"key">>, ?KEY_SCHEMA}
                                                                ,{<<"nested">>, ?NESTED_SCHEMA}
                                                                ])}
                          ,{<<"type">>, <<"object">>}
                          ])
       ).

-spec configs_url() -> string().
configs_url() ->
    string:join([pqc_cb_api:v2_base_url(), "system_configs"], "/").

-spec config_url(kz_term:ne_binary()) -> string().
config_url(Id) ->
    string:join([pqc_cb_api:v2_base_url(), "system_configs", kz_term:to_list(Id)], "/").

-spec config_url(kz_term:ne_binary(), kz_term:ne_binary()) -> string().
config_url(Id, NodeId) ->
    string:join([pqc_cb_api:v2_base_url()
                ,"system_configs"
                ,kz_term:to_list(Id)
                ,kz_term:to_list(NodeId)
                ]
               ,"/"
               ).

-spec list_configs(pqc_cb_api:api()) -> kz_term:ne_binaries().
list_configs(API) ->
    URL = configs_url(),
    Expectations = [#expectation{response_codes = [200]}],
    Resp = pqc_cb_api:make_request(Expectations
                                  ,fun kz_http:get/2
                                  ,URL
                                  ,pqc_cb_api:request_headers(API)
                                  ),
    pqc_cb_response:data(Resp).

-spec get_config(pqc_cb_api:api(), kz_term:ne_binary()) -> kzd_system_configs:doc().
get_config(API, Id) ->
    URL = config_url(Id),
    Expectations = [#expectation{response_codes = [200, 404]}],
    Resp = pqc_cb_api:make_request(Expectations
                                  ,fun kz_http:get/2
                                  ,URL
                                  ,pqc_cb_api:request_headers(API)
                                  ),
    pqc_cb_response:data(Resp).

-spec get_default_config(pqc_cb_api:api(), kz_term:ne_binary()) -> kzd_system_configs:doc().
get_default_config(API, Id) ->
    URL = config_url(Id) ++ "?with_defaults=true",
    Expectations = [#expectation{response_codes = [200, 404]}],
    Resp = pqc_cb_api:make_request(Expectations
                                  ,fun kz_http:get/2
                                  ,URL
                                  ,pqc_cb_api:request_headers(API)
                                  ),
    pqc_cb_response:data(Resp).

-spec get_node_config(pqc_cb_api:api(), kz_term:ne_binary(), kz_term:ne_binary()) -> kzd_system_configs:doc().
get_node_config(API, Id, NodeId) ->
    URL = config_url(Id, NodeId) ++ "?with_defaults=true",
    Expectations = [#expectation{response_codes = [200, 404]}],
    Resp = pqc_cb_api:make_request(Expectations
                                  ,fun kz_http:get/2
                                  ,URL
                                  ,pqc_cb_api:request_headers(API)
                                  ),
    pqc_cb_response:data(Resp).


-spec set_default_config(pqc_cb_api:api(), kz_json:object()) -> kzd_system_configs:doc().
set_default_config(API, Config) ->
    ?INFO("setting default config for ~p", [Config]),
    URL = config_url(kz_doc:id(Config)),
    Data = pqc_cb_api:create_envelope(Config),
    Expectations = [#expectation{response_codes = [200]}],

    Resp = pqc_cb_api:make_request(Expectations
                                  ,fun kz_http:post/3
                                  ,URL
                                  ,pqc_cb_api:request_headers(API)
                                  ,kz_json:encode(Data)
                                  ),
    pqc_cb_response:data(Resp).

-spec patch_default_config(pqc_cb_api:api(), kz_term:ne_binary(), kz_json:object()) -> kzd_system_configs:doc().
patch_default_config(API, Id, Config) ->
    ?INFO("patching default config for ~p", [Config]),
    URL = config_url(Id),
    Data = pqc_cb_api:create_envelope(Config),
    Expectations = [#expectation{response_codes = [200]}],

    Resp = pqc_cb_api:make_request(Expectations
                                  ,fun kz_http:patch/3
                                  ,URL
                                  ,pqc_cb_api:request_headers(API)
                                  ,kz_json:encode(Data)
                                  ),
    pqc_cb_response:data(Resp).

-spec delete_config(pqc_cb_api:api(), kz_term:ne_binary()) -> kz_json:object().
delete_config(API, Id) ->
    URL = config_url(Id),
    Expectations = [#expectation{response_codes = [200, 404]}],
    Resp = pqc_cb_api:make_request(Expectations
                                  ,fun kz_http:delete/2
                                  ,URL
                                  ,pqc_cb_api:request_headers(API)
                                  ),
    pqc_cb_response:data(Resp).

init() ->
    _ = kz_data_tracing:clear_all_traces(),
    _ = [kapps_controller:start_app(App) ||
            App <- ['crossbar', 'conference']
        ],
    _ = [crossbar_maintenance:start_module(Mod) ||
            Mod <- ['cb_system_configs']
        ],
    _ = init_db(),
    ?INFO("INIT FINISHED").

-spec init_db() -> {'ok', kz_json:object()}.
init_db() ->
    {'ok', _} = kz_datamgr:save_doc(?KZ_SCHEMA_DB, ?CONFIG_SCHEMA).

-spec cleanup_db() -> 'ok'.
cleanup_db() ->
    _ = kz_datamgr:del_doc(?KZ_CONFIG_DB, ?CATEGORY_ID),
    _ = kz_datamgr:del_doc(?KZ_SCHEMA_DB, ?SCHEMA_ID),
    'ok'.

-spec initial_state() -> pqc_kazoo_model:model().
initial_state() ->
    _ = init(),
    API = pqc_cb_api:authenticate(),
    ?INFO("state initialized to ~p", [API]),
    pqc_kazoo_model:new(API).

-spec default() -> 'ok'.
default() ->
    _ = init_db(),
    Stored = kapps_config_doc:stored_node(?CATEGORY_ID, <<"default">>),
    io:format("stored: ~p~n", [Stored]),
    cleanup_db().

-spec seq() -> any().
seq() ->
    Model = initial_state(),
    API = pqc_kazoo_model:api(Model),

    Listing = list_configs(API),
    'false' = lists:member(?CATEGORY_ID, Listing),

    GetSchemaDefaults = get_default_config(API, ?CATEGORY_ID),
    io:format("get with schema defaults: ~p~n", [GetSchemaDefaults]),
    ?CATEGORY_ID = kz_json:get_value(<<"id">>, GetSchemaDefaults),
    <<"default_key">> = kz_json:get_value([<<"default">>, <<"key">>], GetSchemaDefaults),
    <<"default_knee">> = kz_json:get_value([<<"default">>, <<"nested">>, <<"knee">>], GetSchemaDefaults),

    Section = kz_json:from_list([{<<"key">>, <<"value">>}
                                ,{<<"nested">>, kz_json:from_list([{<<"knee">>, <<"nalue">>}])}
                                ]),
    Defaults = kz_json:from_list([{<<"default">>, Section}
                                 ,{<<"id">>, ?CATEGORY_ID}
                                 ]),

    Set = set_default_config(API, Defaults),
    io:format("set: ~p~n", [Set]),
    'true' = kz_json:are_equal(Set, Defaults),

    Get = get_node_config(API, ?CATEGORY_ID, <<"default">>),
    io:format("get: ~p~n", [Get]),
    <<"nalue">> = kz_json:get_value([<<"nested">>, <<"knee">>], Get),
    [?CATEGORY_ID, <<"default">>] = binary:split(kz_doc:id(Get), <<"/">>),
    <<"value">> = kz_json:get_value(<<"key">>, Get),

    <<"nalue">> = kapps_config:get(?CATEGORY_ID, [<<"nested">>, <<"knee">>], 'undefined', <<"default">>),
    <<"value">> = kapps_config:get(?CATEGORY_ID, <<"key">>, 'undefined', <<"default">>),

    NodeSection = kz_json:from_list([{<<"key">>, <<"node">>}
                                    ,{<<"nested">>, kz_json:from_list([{<<"ankle">>, <<"alue">>}])}
                                    ]),
    NodeSettings = kz_json:from_list([{?NODE_ID, NodeSection}]),

    Patch = patch_default_config(API, ?CATEGORY_ID, NodeSettings),
    io:format("patch: ~p~n", [Patch]),
    <<"node">> = kz_json:get_value([?NODE_ID, <<"key">>], Patch),
    <<"alue">> = kz_json:get_value([?NODE_ID, <<"nested">>, <<"ankle">>], Patch),

    <<"node">> = kapps_config:get(?CATEGORY_ID, <<"key">>, 'undefined', ?NODE_ID),
    <<"alue">> = kapps_config:get(?CATEGORY_ID, [<<"nested">>, <<"ankle">>], 'undefined', ?NODE_ID),
    <<"nalue">> = kapps_config:get(?CATEGORY_ID, [<<"nested">>, <<"knee">>], 'undefined', ?NODE_ID),

    GetAll = get_default_config(API, ?CATEGORY_ID),
    io:format("get all: ~p~n", [GetAll]),
    <<"node">> = kz_json:get_value([?NODE_ID, <<"key">>], Patch),
    <<"alue">> = kz_json:get_value([?NODE_ID, <<"nested">>, <<"ankle">>], Patch),
    <<"nalue">> = kz_json:get_value([<<"default">>, <<"nested">>, <<"knee">>], Patch),

    GetNode = get_node_config(API, ?CATEGORY_ID, ?NODE_ID),
    io:format("get node: ~p~n", [GetNode]),
    [?CATEGORY_ID, ?NODE_ID] = binary:split(kz_doc:id(GetNode), <<"/">>),
    <<"node">> = kz_json:get_value([<<"key">>], GetNode),
    <<"alue">> = kz_json:get_value([<<"nested">>, <<"ankle">>], GetNode),
    <<"nalue">> = kz_json:get_value([<<"nested">>, <<"knee">>], GetNode),

    InListing = list_configs(API),
    'true' = lists:member(?CATEGORY_ID, InListing),

    Delete = delete_config(API, ?CATEGORY_ID),
    io:format("delete: ~p~n", [Delete]),
    'true' = kz_json:is_true([<<"_read_only">>, <<"deleted">>], Delete),

    OutListing = list_configs(API),
    'false' = lists:member(?CATEGORY_ID, OutListing),

    ?INFO("COMPLETED SUCCESSFULLY!"),
    _ = cleanup(API),
    io:format("done: ~p~n", [API]).

-spec cleanup() -> any().
cleanup() ->
    ?INFO("CLEANUP ALL THE THINGS"),
    kz_data_tracing:clear_all_traces(),
    cleanup(pqc_cb_api:authenticate()).

-spec cleanup(pqc_cb_api:state()) -> any().
cleanup(API) ->
    ?INFO("CLEANUP TIME, EVERYBODY HELPS"),
    cleanup_db(),
    pqc_cb_api:cleanup(API).
