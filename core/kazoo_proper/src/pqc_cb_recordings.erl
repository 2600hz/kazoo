%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2018, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_cb_recordings).

-export([list_recordings/2
        ,create_recording/2
         %% ,fetch_recording/3
         %% ,fetch_recording_binary/3
         %% ,delete_recording/3
        ]).

%% -export([command/2
%%          ,next_state/3
%%          ,postcondition/3
%%         ]).

-export([seq/0]).

-include("kazoo_proper.hrl").

-define(ACCOUNT_NAMES, [<<"account_for_recordings">>]).

-spec list_recordings(pqc_cb_api:state(), pqc_cb_accounts:account_id()) ->
                             {'error', 'not_found'} |
                             {'ok', kz_json:objects()}.
list_recordings(API, AccountId) ->
    case pqc_cb_api:make_request([200]
                                ,fun kz_http:get/2
                                ,recordings_url(AccountId)
                                ,pqc_cb_api:request_headers(API)
                                )
    of
        {'error', _E} ->
            ?DEBUG("listing recordings errored: ~p", [_E]),
            {'error', 'not_found'};
        Response ->
            ?DEBUG("listing recordings: ~s", [Response]),
            {'ok', kz_json:get_list_value(<<"data">>, kz_json:decode(Response))}
    end.

-spec create_recording(pqc_cb_api:state(), pqc_cb_accounts:account_id()) ->
                              {'ok', kzd_call_recordings:doc()}.
create_recording(_API, AccountId) ->
    MODB = kz_util:format_account_mod_id(AccountId),

    BaseMediaDoc = create_recording_doc(),
    MediaDoc = kz_doc:update_pvt_parameters(BaseMediaDoc, MODB, [{'type', kzd_call_recordings:type()}]),
    ?INFO("saving to ~s: ~p", [MODB, MediaDoc]),
    {'ok', Doc} = kazoo_modb:save_doc(MODB, MediaDoc, [{'ensure_saved', 'true'}]),
    create_attachment(MODB, kz_doc:id(Doc)).

-define(RECORDING_ID, <<"bf8a6522730f93248d41f2521cfe2b95">>).
create_recording_doc() ->
    lists:foldl(fun({F, V}, Doc) -> F(Doc, V) end
               ,kzd_call_recordings:new()
               ,[{fun kzd_call_recordings:set_id/2, ?RECORDING_ID}
                ,{fun kzd_call_recordings:set_description/2, <<"pqc_cb_recordings test">>}
                ,{fun kzd_call_recordings:set_source_type/2, kz_term:to_binary(?MODULE)}
                ]
               ).

create_attachment(MODB, DocId) ->
    File = filename:join([code:priv_dir('kazoo_proper'), <<"mp3.mp3">>]),
    AName = filename:basename(File, <<".mp3">>),
    {'ok', Contents} = file:read_file(File),
    ?INFO("adding attachment to ~s/~s: ~s", [MODB, DocId, AName]),
    kz_datamgr:put_attachment(MODB, DocId, AName, Contents).

recordings_url(AccountId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "recordings"], "/").

-spec seq() -> 'ok'.
seq() ->
    init(),
    Model = initial_state(),
    API = pqc_kazoo_model:api(Model),

    try
        AccountResp = pqc_cb_accounts:create_account(API, hd(?ACCOUNT_NAMES)),
        AccountId = kz_json:get_value([<<"data">>, <<"id">>], kz_json:decode(AccountResp)),
        ?INFO("created account ~s", [AccountId]),

        {'ok', RecordingDoc} = create_recording(API, AccountId),
        ?INFO("created recording ~p", [RecordingDoc]),

        {'ok', Recordings} = list_recordings(API, AccountId),
        ?INFO("recordings available: ~p", [Recordings])
    catch
        _E:_R ->
            ST = erlang:get_stacktrace(),
            ?INFO("failed ~s: ~p", [_E, _R]),
            [?INFO("st: ~p", [S]) || S <- ST]
    after
        pqc_cb_accounts:cleanup_accounts(API, ?ACCOUNT_NAMES),
        pqc_cb_api:cleanup(API)
    end,
    ?INFO("seq finished running: ~p", [API]),
    io:format('user', "logs in /tmp/~s.log~n", [maps:get('request_id', API)]).

init() ->
    _ = kz_data_tracing:clear_all_traces(),
    _ = [kapps_controller:start_app(App) ||
            App <- ['crossbar']
        ],
    _ = [crossbar_maintenance:start_module(Mod) ||
            Mod <- ['cb_recordings', 'cb_accounts']
        ],
    ?INFO("INIT FINISHED").

-spec initial_state() -> pqc_kazoo_model:model().
initial_state() ->
    API = pqc_cb_api:authenticate(),
    ?INFO("state initialized to ~p", [API]),
    pqc_kazoo_model:new(API).
