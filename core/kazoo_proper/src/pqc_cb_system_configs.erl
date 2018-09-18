-module(pqc_cb_system_configs).

-export([seq/0
        ,cleanup/0, cleanup/1
        ]
       ).

%% API
-export([list_configs/1
        ,set_default_config/3
        ,get_config/2
        ,get_default_config/2
        ,delete_config/2
        ]).

-include("kazoo_proper.hrl").

-spec configs_url() -> string().
configs_url() ->
    string:join([pqc_cb_api:v2_base_url(), "system_configs"], "/").

-spec config_url(kz_term:ne_binary()) -> string().
config_url(Id) ->
    string:join([pqc_cb_api:v2_base_url(), "system_configs", kz_term:to_list(Id)], "/").

-spec list_configs(pqc_cb_api:api()) -> kz_term:ne_binaries().
list_configs(API) ->
    URL = configs_url(),
    io:format("GET ~s~n", [URL]),
    Resp = pqc_cb_api:make_request([200]
                                  ,fun kz_http:get/2
                                  ,URL
                                  ,pqc_cb_api:request_headers(API)
                                  ),
    pqc_cb_response:data(Resp).

-spec get_config(pqc_cb_api:api(), kz_term:ne_binary()) -> kz_json:object().
get_config(API, Id) ->
    URL = config_url(Id),
    io:format("GET ~p:~n", [URL]),
    Resp = pqc_cb_api:make_request([200, 404]
                                  ,fun kz_http:get/2
                                  ,URL
                                  ,pqc_cb_api:request_headers(API)
                                  ),
    pqc_cb_response:data(Resp).

-spec get_default_config(pqc_cb_api:api(), kz_term:ne_binary()) -> binary().
get_default_config(API, Id) ->
    URL = config_url(Id) ++ "?with_defaults=true",
    io:format("GET ~p:~n", [URL]),
    pqc_cb_api:make_request([200, 404]
                           ,fun kz_http:get/2
                           ,URL
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec set_default_config(pqc_cb_api:api(), kz_term:ne_binary(), kz_json:object()) -> binary().
set_default_config(API, Id, Config) ->
    ?INFO("settings config for ~s"),
    URL = config_url(Id),
    Data = pqc_cb_api:create_envelope(Config),

    io:format("POST ~p:~n~p~n", [URL, Data]),
    pqc_cb_api:make_request([200]
                           ,fun kz_http:post/3
                           ,URL
                           ,pqc_cb_api:request_headers(API)
                           ,kz_json:encode(Data)
                           ).

-spec delete_config(pqc_cb_api:api(), kz_term:ne_binary()) -> binary().
delete_config(API, Id) ->
    URL = config_url(Id),
    io:format("DELETE ~p:~n", [URL]),
    pqc_cb_api:make_request([200, 404]
                           ,fun kz_http:delete/2
                           ,URL
                           ,pqc_cb_api:request_headers(API)
                           ).

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

    Id = <<"kazoo_proper">>,

    Listing = list_configs(API),
    io:format("listing: ~p~n", [Listing]),
    'false' = lists:member(Id, Listing),

    404 =

        ?INFO("COMPLETED SUCCESSFULLY!"),
    cleanup(API),
    io:format("done: ~p~n", [API]).

-spec cleanup() -> any().
cleanup() ->
    ?INFO("CLEANUP ALL THE THINGS"),
    kz_data_tracing:clear_all_traces(),
    pqc_cb_services:cleanup(),
    cleanup(pqc_cb_api:authenticate()).

-spec cleanup(pqc_cb_api:state()) -> any().
cleanup(API) ->
    ?INFO("CLEANUP TIME, EVERYBODY HELPS"),
    pqc_cb_api:cleanup(API).
