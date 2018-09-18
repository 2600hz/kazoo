-module(pqc_cb_system_configs).

-export([seq/0
        ,cleanup/0, cleanup/1
        ]
       ).

%% API
-export([set_default_config/3
        ,get_config/2
        ,get_default_config/2
        ,delete_config/2
        ]).

-include("kazoo_proper.hrl").

-spec get_config(pqc_cb_api:api(), kz_term:ne_binary()) -> binary().
get_config(API, Id) ->
    URL = string:join([pqc_cb_api:v2_base_url(), "system_configs", kz_term:to_list(Id)], "/"),
    io:format("GET ~p:~n", [URL]),
    pqc_cb_api:make_request([200, 404]
                           ,fun kz_http:get/2
                           ,URL
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec get_default_config(pqc_cb_api:api(), kz_term:ne_binary()) -> binary().
get_default_config(API, Id) ->
    URL = string:join([pqc_cb_api:v2_base_url(), "system_configs", kz_term:to_list(Id)], "/")
        ++ "?with_defaults=true",
    io:format("GET ~p:~n", [URL]),
    pqc_cb_api:make_request([200, 404]
                           ,fun kz_http:get/2
                           ,URL
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec set_default_config(pqc_cb_api:api(), kz_term:ne_binary(), kz_json:object()) -> binary().
set_default_config(API, Id, Config) ->
    ?INFO("settings config for ~s"),
    URL = string:join([pqc_cb_api:v2_base_url(), "system_configs", kz_term:to_list(Id)], "/"),
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
    URL = string:join([pqc_cb_api:v2_base_url(), "system_configs", kz_term:to_list(Id)], "/"),
    io:format("DELETE ~p:~n", [URL]),
    pqc_cb_api:make_request([200, 404]
                           ,fun kz_http:delete/2
                           ,URL
                           ,pqc_cb_api:request_headers(API)
                           ).

-define(DEFAULT_CONFIG
       ,<<"{\"default\":{\"entry_tone\":\"tone_stream:\/\/v=-7;>=2;+=.1;%(300,0,523,659);v=-7;>=3;+=.1;%(800,0,659,783)\",\"exit_tone\":\"tone_stream:\/\/v=-7;>=2;+=.1;%(300,0,523,440);v=-7;>=3;+=.1;%(800,0,349,440)\",\"moderator_entry_tone\":\"tone_stream:\/\/v=-7;>=2;+=.1;%(300,0,523,659);v=-7;>=3;+=.1;%(800,0,659,783)\",\"moderator_exit_tone\":\"tone_stream:\/\/v=-7;>=2;+=.1;%(300,0,523,440);v=-7;>=3;+=.1;%(800,0,349,440)\",\"number_timeout\":5000,\"participant_sanity_check_ms\":60000,\"pin_timeout\":5000,\"profiles\":{\"default\":{\"caller-controls\":\"default\",\"comfort-noise\":1000,\"energy-level\":20,\"enter-sound\":\"tone_stream:\/\/v=-7;>=2;+=.1;%(300,0,523,659);v=-7;>=3;+=.1;%(800,0,659,783)\",\"interval\":20,\"moh-sound\":\"$${hold_music}\",\"rate\":8000},\"page\":{\"caller-controls\":\"default\",\"comfort-noise\":1000,\"energy-level\":20,\"enter-sound\":\"\",\"interval\":20,\"moh-sound\":\"\",\"rate\":8000}},\"review_name\":false,\"route_win_timeout\":3000,\"support_name_announcement\":true},\"id\":\"conferences\/default\",\"profiles\":{\"default\":{\"enter-sound\":\"tone_stream:\/\/%(1000,0,1750)\",\"exit-sound\":\"tone_stream:\/\/%(1000,0,1800)\",\"deaf-sound\":\"tone_stream:\/\/%(1000,0,1000)\",\"undeaf-sound\":\"tone_stream:\/\/%(1000,0,1550)\",\"muted-sound\":\"tone_stream:\/\/%(1000,0,1250)\",\"unmuted-sound\":\"tone_stream:\/\/%(1000,0,1600)\",\"alone-sound\":\"tone_stream:\/\/%(1000,0,800)\",\"member-enter-sound\":\"tone_stream:\/\/%(1000,0,1150)\"}}}">>
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

    Id = <<"conferences">>,
    Config = kz_json:decode(?DEFAULT_CONFIG),

    'true' = lists:all(fun(Test) -> run_test(Test, API, Id, Config) end
                      ,[fun test_get/3
                       ,fun test_get_default/3
                       ]
                      ),

    ?INFO("COMPLETED SUCCESSFULLY!"),
    cleanup(API),
    io:format("done: ~p~n", [API]).

run_test(Test, API, Id, Config) ->
    try Test(API, Id, Config)
    catch
        _E:_R ->
            ST = erlang:get_stacktrace(),
            io:format("test ~p failed: ~s: ~p~n~p~n", [Test, _E, _R, ST]),
            'false'
    end.

test_get(API, Id, Config) ->
    Set = set_default_config(API, Id, Config),
    SetDefault = kz_json:get_json_value(<<"data">>, kz_json:decode(Set)),
    io:format("set default: ~p", [SetDefault]),

    Get = get_config(API, Id),
    Id =:= kz_json:get_value([<<"data">>, <<"id">>], kz_json:decode(Get)).

test_get_default(API, Id, _Config) ->
    Delete = delete_config(API, Id),
    io:format("delete: ~p~n", [Delete]),

    Get = get_default_config(API, Id),
    GetDefault = kz_json:get_json_value(<<"data">>, kz_json:decode(Get)),
    io:format("get default: ~p", [GetDefault]),

    'true' = kz_json:is_defined(<<"default">>, GetDefault),

    5000 = kz_json:get_integer_value([<<"default">>, <<"number_timeout">>], GetDefault),

    1000 = kz_json:get_integer_value([<<"default">>, <<"profiles">>, <<"default">>, <<"comfort-noise">>], GetDefault).

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
