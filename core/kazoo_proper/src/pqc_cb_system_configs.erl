-module(pqc_cb_system_configs).

-export([seq/0
        ,cleanup/0, cleanup/1
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

-define(SYSTEM_CONFIG_ID, <<"kazoo_proper">>).
-define(NODE_ID, <<"foo@bar.com">>).

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
    Resp = pqc_cb_api:make_request([200]
                                  ,fun kz_http:get/2
                                  ,URL
                                  ,pqc_cb_api:request_headers(API)
                                  ),
    pqc_cb_response:data(Resp).

-spec get_config(pqc_cb_api:api(), kz_term:ne_binary()) -> kzd_system_configs:doc().
get_config(API, Id) ->
    URL = config_url(Id),
    Resp = pqc_cb_api:make_request([200, 404]
                                  ,fun kz_http:get/2
                                  ,URL
                                  ,pqc_cb_api:request_headers(API)
                                  ),
    pqc_cb_response:data(Resp).

-spec get_default_config(pqc_cb_api:api(), kz_term:ne_binary()) -> kzd_system_configs:doc().
get_default_config(API, Id) ->
    URL = config_url(Id) ++ "?with_defaults=true",
    Resp = pqc_cb_api:make_request([200, 404]
                                  ,fun kz_http:get/2
                                  ,URL
                                  ,pqc_cb_api:request_headers(API)
                                  ),
    pqc_cb_response:data(Resp).

-spec get_node_config(pqc_cb_api:api(), kz_term:ne_binary(), kz_term:ne_binary()) -> kzd_system_configs:doc().
get_node_config(API, Id, NodeId) ->
    URL = config_url(Id, NodeId) ++ "?with_defaults=true",
    Resp = pqc_cb_api:make_request([200, 404]
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

    Resp = pqc_cb_api:make_request([200]
                                  ,fun kz_http:post/3
                                  ,URL
                                  ,pqc_cb_api:request_headers(API)
                                  ,kz_json:encode(Data)
                                  ),
    pqc_cb_response:data(Resp).

-spec patch_default_config(pqc_cb_api:api(), kz_tern:ne_binary(), kz_json:object()) -> kzd_system_configs:doc().
patch_default_config(API, Id, Config) ->
    ?INFO("patching default config for ~p", [Config]),
    URL = config_url(Id),
    Data = pqc_cb_api:create_envelope(Config),

    Resp = pqc_cb_api:make_request([200]
                                  ,fun kz_http:patch/3
                                  ,URL
                                  ,pqc_cb_api:request_headers(API)
                                  ,kz_json:encode(Data)
                                  ),
    pqc_cb_response:data(Resp).

-spec delete_config(pqc_cb_api:api(), kz_term:ne_binary()) -> binary().
delete_config(API, Id) ->
    URL = config_url(Id),
    Resp = pqc_cb_api:make_request([200, 404]
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
    ?INFO("INIT FINISHED").


-spec initial_state() -> pqc_kazoo_model:model().
initial_state() ->
    init(),
    API = pqc_cb_api:authenticate(),
    ?INFO("state initialized to ~p", [API]),
    pqc_kazoo_model:new(API).

-spec seq() -> any().
seq() ->
    init(),
    Model = initial_state(),
    API = pqc_kazoo_model:api(Model),

    Listing = list_configs(API),
    'false' = lists:member(?SYSTEM_CONFIG_ID, Listing),

    Section = kz_json:from_list([{<<"key">>, <<"value">>}
                                ,{<<"nested">>, kz_json:from_list([{<<"knee">>, <<"nalue">>}])}
                                ]),
    Defaults = kz_json:from_list([{<<"default">>, Section}
                                 ,{<<"id">>, ?SYSTEM_CONFIG_ID}
                                 ]),

    Set = set_default_config(API, Defaults),
    io:format("set: ~p~n", [Set]),
    'true' = kz_json:are_equal(Set, Defaults),

    Get = get_config(API, ?SYSTEM_CONFIG_ID),
    io:format("get: ~p~n", [Get]),
    'true' = kz_json:are_equal(Get, Defaults),

    NodeSection = kz_json:from_list([{<<"key">>, <<"node">>}
                                    ,{<<"nested">>, kz_json:from_list([{<<"ankle">>, <<"alue">>}])}
                                    ]),
    NodeSettings = kz_json:from_list([{?NODE_ID, NodeSection}]),

    Patch = patch_default_config(API, ?SYSTEM_CONFIG_ID, NodeSettings),
    io:format("patch: ~p~n", [Patch]),
    <<"node">> = kz_json:get_value([?NODE_ID, <<"key">>], Patch),
    <<"alue">> = kz_json:get_value([?NODE_ID, <<"nested">>, <<"ankle">>], Patch),

    GetAll = get_default_config(API, ?SYSTEM_CONFIG_ID),
    io:format("get all: ~p~n", [GetAll]),
    <<"node">> = kz_json:get_value([?NODE_ID, <<"key">>], Patch),
    <<"alue">> = kz_json:get_value([?NODE_ID, <<"nested">>, <<"ankle">>], Patch),
    <<"nalue">> = kz_json:get_value([<<"default">>, <<"nested">>, <<"knee">>], Patch),

    GetNode = get_node_config(API, ?SYSTEM_CONFIG_ID, ?NODE_ID),
    io:format("get node: ~p~n", [GetNode]),
    [?SYSTEM_CONFIG_ID, ?NODE_ID] = binary:split(kz_doc:id(GetNode), <<"/">>),
    <<"node">> = kz_json:get_value([<<"key">>], GetNode),
    <<"alue">> = kz_json:get_value([<<"nested">>, <<"ankle">>], GetNode),
    <<"nalue">> = kz_json:get_value([<<"nested">>, <<"knee">>], GetNode),

    InListing = list_configs(API),
    'true' = lists:member(?SYSTEM_CONFIG_ID, InListing),

    Delete = delete_config(API, ?SYSTEM_CONFIG_ID),
    io:format("delete: ~p~n", [Delete]),
    'true' = kz_json:is_true([<<"_read_only">>, <<"deleted">>], Delete),

    OutListing = list_configs(API),
    'false' = lists:member(?SYSTEM_CONFIG_ID, OutListing),

    ?INFO("COMPLETED SUCCESSFULLY!"),
    cleanup(API),
    io:format("done: ~p~n", [API]).

-spec cleanup() -> any().
cleanup() ->
    ?INFO("CLEANUP ALL THE THINGS"),
    kz_data_tracing:clear_all_traces(),
    cleanup(pqc_cb_api:authenticate()).

-spec cleanup(pqc_cb_api:state()) -> any().
cleanup(API) ->
    ?INFO("CLEANUP TIME, EVERYBODY HELPS"),
    kz_datamgr:del_doc(?KZ_CONFIG_DB, ?SYSTEM_CONFIG_ID),
    pqc_cb_api:cleanup(API).
