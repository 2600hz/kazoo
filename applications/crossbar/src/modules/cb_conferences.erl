%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Conferences module
%%% Handle client requests for conference documents
%%%
%%% URI schema:
%%% /v2/accounts/{AccountId}/conferences
%%% /v2/accounts/{AccountId}/conferences/{ConferenceID}
%%% /v2/accounts/{AccountId}/conferences/{ConferenceID}/participants
%%% /v2/accounts/{AccountId}/conferences/{ConferenceID}/participants/{ParticipantId}
%%%
%%%
%%% @author Karl Anderson
%%% @author James Aimonetti
%%% @author Roman Galeev
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_conferences).

-export([init/0
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2, allowed_methods/3
        ,resource_exists/0, resource_exists/1, resource_exists/2, resource_exists/3
        ,validate/1, validate/2, validate/3, validate/4
        ,post/2
        ,put/1, put/2, put/3, put/4
        ,patch/2
        ,delete/2
        ]).

-ifdef(TEST).
-export([build_valid_endpoints/3]).
-endif.

-include("crossbar.hrl").
-include_lib("nklib/include/nklib.hrl").

-define(CB_LIST, <<"conferences/crossbar_listing">>).
-define(CB_LIST_BY_NUMBER, <<"conference/listing_by_number">>).

-define(PARTICIPANTS, <<"participants">>).
-define(MUTE, <<"mute">>).
-define(UNMUTE, <<"unmute">>).
-define(DEAF, <<"deaf">>).
-define(UNDEAF, <<"undeaf">>).
-define(KICK, <<"kick">>).
-define(RELATE, <<"relate">>).
-define(PLAY, <<"play">>).

-define(PUT_ACTION, <<"action">>).

-define(MIN_DIGITS_FOR_DID, 5).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.conferences">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.conferences">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.conferences">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.conferences">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.conferences">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.conferences">>, ?MODULE, 'patch'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.conferences">>, ?MODULE, 'delete'),
    ok.

%%%=============================================================================
%%% REST API Callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() -> [?HTTP_GET, ?HTTP_PUT].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_ConferenceId) -> [?HTTP_GET, ?HTTP_PATCH, ?HTTP_DELETE, ?HTTP_POST, ?HTTP_PUT].

-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(_ConferenceId, ?PARTICIPANTS) -> [?HTTP_GET, ?HTTP_PUT].

-spec allowed_methods(path_token(), path_token(), path_token()) -> http_methods().
allowed_methods(_ConferenceId, ?PARTICIPANTS, _ParticipantId) -> [?HTTP_GET, ?HTTP_PUT].

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_) -> 'true'.

-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists(_, _) -> 'true'.

-spec resource_exists(path_token(), path_token(), path_token()) -> 'true'.
resource_exists(_, _, _) -> 'true'.

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_conferences(cb_context:req_verb(Context), Context).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, ConferenceId) ->
    validate_conference(cb_context:req_verb(Context), Context, ConferenceId).

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, ConferenceId, ?PARTICIPANTS) ->
    validate_participants(cb_context:req_verb(Context), Context, ConferenceId).

-spec validate(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
validate(Context, ConferenceId, ?PARTICIPANTS, ParticipantId) ->
    validate_participant(cb_context:req_verb(Context), Context, ConferenceId, ParticipantId).

%%%=============================================================================
%%% Request object validators
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_conferences(http_method(), cb_context:context()) -> cb_context:context().
validate_conferences(?HTTP_GET, Context) ->
    LoadedContext = crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2),
    case cb_context:resp_status(LoadedContext) of
        'success' ->
            Context1 = search_conferences(LoadedContext),
            RunningConferences = cb_context:fetch(Context1, 'conferences', kz_json:new()),
            add_realtime(LoadedContext, RunningConferences);
        _ ->
            LoadedContext
    end;
validate_conferences(?HTTP_PUT, Context) ->
    create_conference(Context).

-spec validate_conference(http_method(), cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
validate_conference(?HTTP_GET, Context0, ConferenceId) ->
    Context1 = maybe_load_conference(ConferenceId, Context0),
    case cb_context:resp_status(Context1) of
        'success' -> enrich_conference(ConferenceId, Context1);
        _Else -> Context1
    end;
validate_conference(?HTTP_POST, Context, ConferenceId) ->
    update_conference(ConferenceId, Context);
validate_conference(?HTTP_PUT, Context, ConferenceId) ->
    maybe_load_conference(ConferenceId, Context);
validate_conference(?HTTP_PATCH, Context, ConferenceId) ->
    patch_conference(ConferenceId, Context);
validate_conference(?HTTP_DELETE, Context, ConferenceId) ->
    load_conference(ConferenceId, Context).

-spec validate_participants(http_method(), cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
validate_participants(?HTTP_GET, Context0, ConferenceId) ->
    Context1 = maybe_load_conference(ConferenceId, Context0),
    case cb_context:resp_status(Context1) of
        'success' -> enrich_participants(ConferenceId, Context1);
        _Else -> Context1
    end;
validate_participants(?HTTP_PUT, Context, ConferenceId) ->
    maybe_load_conference(ConferenceId, Context).

-spec validate_participant(http_method(), cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary()) -> cb_context:context().
validate_participant(?HTTP_GET, Context0, ConferenceId, ParticipantId) ->
    Context1 = maybe_load_conference(ConferenceId, Context0),
    case cb_context:resp_status(Context1) of
        'success' -> enrich_participant(ParticipantId, ConferenceId, Context1);
        _Else -> Context1
    end;
validate_participant(?HTTP_PUT, Context, ConferenceId, _ParticipantId) ->
    maybe_load_conference(ConferenceId, Context).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _ConferenceId) ->
    crossbar_doc:save(Context).

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(Context).

-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context, ConferenceId) ->
    handle_conference_action(Context, ConferenceId, cb_modules_util:get_request_action(Context)).

-spec put(cb_context:context(), path_token(), path_token()) -> cb_context:context().
put(Context, ConferenceId, ?PARTICIPANTS) ->
    Action = cb_context:req_value(Context, ?PUT_ACTION),
    handle_participants_action(Context, ConferenceId, Action).

-spec put(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
put(Context, ConferenceId, ?PARTICIPANTS, ParticipantId) ->
    Action = cb_context:req_value(Context, ?PUT_ACTION),
    participant_action(Context, ConferenceId, ParticipantId, Action).

participant_action(Context, ConferenceId, ParticipantId, ?PLAY) ->
    play(Context, ConferenceId, ParticipantId, cb_context:req_value(Context, <<"data">>));

participant_action(Context, ConferenceId, ParticipantId, Action) ->
    perform_participant_action(conference(ConferenceId), Action, kz_term:to_integer(ParticipantId)),
    crossbar_util:response_202(<<"ok">>, Context).

-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, _) ->
    crossbar_doc:save(Context).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _) ->
    crossbar_doc:delete(Context).

%%%=============================================================================
%%% Conference validation helpers
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_load_conference(path_token(), cb_context:context()) -> cb_context:context().
maybe_load_conference(ConferenceId, Context) ->
    maybe_build_conference(ConferenceId, load_conference(ConferenceId, Context)).

-spec maybe_build_conference(path_token(), cb_context:context()) -> cb_context:context().
maybe_build_conference(ConferenceId, Context) ->
    case cb_context:resp_status(Context) of
        'success' -> Context; % loaded an existing conference
        _ ->
            lager:info("building an ad-hoc conference for ~s", [ConferenceId]),
            build_conference(ConferenceId, Context)
    end.

-spec build_conference(path_token(), cb_context:context()) -> cb_context:context().
build_conference(ConferenceId, Context) ->
    Conference = kz_doc:set_id(kzd_conference:new(), ConferenceId),
    Merged = kz_json:merge(Conference, cb_context:req_data(Context)),
    crossbar_doc:handle_datamgr_success(Merged, Context).

-spec load_conference(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
load_conference(ConferenceId, Context) ->
    crossbar_doc:load(ConferenceId, Context, ?TYPE_CHECK_OPTION(kzd_conference:type())).

-spec create_conference(cb_context:context()) -> cb_context:context().
create_conference(Context) ->
    OnSuccess = fun(C) -> validate_numbers('undefined', C) end,
    cb_context:validate_request_data(<<"conferences">>, Context, OnSuccess).

-spec update_conference(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
update_conference(ConferenceId, Context) ->
    OnSuccess = fun(C) -> validate_numbers(ConferenceId, C) end,
    cb_context:validate_request_data(<<"conferences">>, Context, OnSuccess).

-spec patch_conference(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
patch_conference(ConferenceId, Context) ->
    crossbar_doc:patch_and_validate(ConferenceId, Context, fun update_conference/2).

-spec on_successful_validation(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    cb_context:update_doc(Context, {fun kz_doc:set_type/2, <<"conference">>});
on_successful_validation(ConferenceId, Context) ->
    crossbar_doc:load_merge(ConferenceId, Context, ?TYPE_CHECK_OPTION(<<"conference">>)).

-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(ViewResult, Acc) ->
    [kz_json:get_json_value(<<"value">>, ViewResult) | Acc].

empty_realtime_data() ->
    kz_json:from_list_recursive(
      [{<<"_read_only">>
       ,[{<<"members">>, 0}
        ,{<<"moderators">>, 0}
        ,{<<"duration">>, 0}
        ,{<<"is_locked">>, 'false'}
        ]
       }]).

-spec move_to_read_only(kz_json:key(), kz_json:object()) ->
                               {kz_json:key(), kz_json:object()}.
move_to_read_only(Id, Realtime) ->
    {Id, kz_json:from_list([{<<"id">>, Id}
                           ,{<<"_read_only">>, kz_json:normalize(Realtime)}
                           ])
    }.

-spec add_realtime(cb_context:context(), kz_json:object()) -> cb_context:context().
add_realtime(Context, RunningConferences) ->
    ReadOnly = kz_json:map(fun move_to_read_only/2, RunningConferences),

    Conferences = [add_realtime_fold(JObj, ReadOnly) || JObj <-  cb_context:doc(Context)],

    cb_context:setters(Context
                      ,[{fun cb_context:set_doc/2, Conferences}
                       ,{fun cb_context:set_resp_status/2, 'success'}
                       ,{fun cb_context:set_resp_data/2, Conferences}
                       ,{fun cb_context:set_resp_envelope/2
                        ,kz_json:set_value(<<"page_size">>, length(Conferences), cb_context:resp_envelope(Context))
                        }
                       ]).

-spec add_realtime_fold(kzd_conference:doc(), kz_json:object()) -> kz_json:object().
add_realtime_fold(Conference, ReadOnly) ->
    Realtime = kz_json:get_value(kz_doc:id(Conference), ReadOnly, empty_realtime_data()),
    kz_json:merge(Conference, Realtime).

%%------------------------------------------------------------------------------
%% @doc Create a new conference document with the data provided, if it is valid
%% @end
%%------------------------------------------------------------------------------
-spec validate_numbers(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
validate_numbers(Id, Context) ->
    Doc = cb_context:doc(Context),
    Conf = kz_json:get_value(<<"conference_numbers">>, Doc, []),
    Member = kz_json:get_value([<<"member">>, <<"numbers">>], Doc, []),
    Moderator = kz_json:get_value([<<"moderator">>, <<"numbers">>], Doc, []),
    Keys = Conf ++ Member ++ Moderator,
    AccountDb = cb_context:account_db(Context),
    case kz_datamgr:get_results(AccountDb, ?CB_LIST_BY_NUMBER, [{'keys', Keys}]) of
        {'error', _R} -> cb_context:add_system_error('datastore_fault', Context);
        {'ok', []} -> on_successful_validation(Id, Context);
        {'ok', JObjs} when Id =:= 'undefined' -> invalid_numbers(Context, JObjs);
        {'ok', JObjs} ->
            case [JObj || JObj <- JObjs, kz_doc:id(JObj) =/= Id] of
                [] -> on_successful_validation(Id, Context);
                OtherJObjs -> invalid_numbers(Context, OtherJObjs)
            end
    end.

invalid_numbers(Context, JObjs) ->
    Numbers = kz_binary:join(kz_datamgr:get_result_keys(JObjs)),
    Error = kz_json:from_list([{<<"message">>, <<"Numbers already in use">>}
                              ,{<<"cause">>, Numbers}
                              ]),
    cb_context:add_validation_error([<<"numbers">>], <<"unique">>, Error, Context).

%%%=============================================================================
%%% Conference Actions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_conference_action(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary()) -> cb_context:context().
handle_conference_action(Context, ConferenceId, <<"lock">>) ->
    kapps_conference_command:lock(conference(ConferenceId)),
    crossbar_util:response_202(<<"ok">>, Context);
handle_conference_action(Context, ConferenceId, <<"unlock">>) ->
    kapps_conference_command:unlock(conference(ConferenceId)),
    crossbar_util:response_202(<<"ok">>, Context);
handle_conference_action(Context, ConferenceId, ?PLAY) ->
    play(Context, ConferenceId, cb_context:req_value(Context, <<"data">>));
handle_conference_action(Context, ConferenceId, <<"dial">>) ->
    dial(Context, ConferenceId, cb_context:req_value(Context, <<"data">>));
handle_conference_action(Context, ConferenceId, <<"record">>) ->
    record_conference(Context, ConferenceId, cb_context:req_value(Context, <<"data">>));
handle_conference_action(Context, ConferenceId, Action) ->
    lager:error("unhandled conference id ~p action: ~p", [ConferenceId, Action]),
    cb_context:add_system_error('faulty_request', Context).

-spec record_conference(cb_context:context(), kz_term:ne_binary(), kz_json:api_object()) ->
                               cb_context:context().
record_conference(Context, ConferenceId, 'undefined') ->
    data_required(Context, <<"record">>);
record_conference(Context, ConferenceId, RecordingData) ->
    toggle_recording(Context, ConferenceId, kz_json:get_ne_binary_value(<<"action">>, RecordingData)).

-spec toggle_recording(cb_context:context(), kz_term:ne_binary(), kz_json:api_ne_binary()) ->
                              cb_context:context().
toggle_recording(Context, ConferenceId, <<"start">>) ->
    lager:info("starting the recording of conference ~s", [ConferenceId]),
    kapps_conference_command:record(conference(ConferenceId)),
    crossbar_util:response_202(<<"starting recording">>, Context);
toggle_recording(Context, ConferenceId, <<"stop">>) ->
    lager:info("stopping the recording of conference ~s", [ConferenceId]),
    kapps_conference_command:recordstop(conference(ConferenceId)),
    crossbar_util:response_202(<<"stopping recording">>, Context);
toggle_recording(Context, ConferenceId, 'undefined') ->
    cb_context:add_validation_error([<<"data">>, <<"action">>]
                                   ,<<"required">>
                                   ,kz_json:from_list([{<<"message">>, <<"recording requires an action">>}])
                                   ,Context
                                   );
toggle_recording(Context, ConferenceId, Action) ->
    lager:debug("invalid action: ~p", [Action]),
    cb_context:add_validation_error([<<"data">>, <<"action">>]
                                   ,<<"enum">>
                                   ,kz_json:from_list(
                                      [{<<"message">>, <<"Value not found in enumerated list of values">>}
                                      ,{<<"target">>, [<<"start">>, <<"stop">>]}
                                      ,{<<"value">>, Action}
                                      ])
                                   ,Context
                                   ).

-spec play(cb_context:context(), path_token(), kz_term:api_object()) ->
                  cb_context:context().
play(Context, _ConferenceId, 'undefined') ->
    data_required(Context, <<"play">>);
play(Context, ConferenceId, Data) ->
    play_media(Context, ConferenceId, kz_json:get_ne_binary_value(<<"media_id">>, Data)).

-spec play(cb_context:context(), path_token(), pos_integer(), kz_term:api_object()) ->
                  cb_context:context().
play(Context, _ConferenceId, _ParticipantId, 'undefined') ->
    data_required(Context, <<"play">>);
play(Context, ConferenceId, ParticipantId, Data) ->
    play_media(Context, ConferenceId, ParticipantId, kz_json:get_ne_binary_value(<<"media_id">>, Data)).

-spec data_required(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
data_required(Context, Action) ->
    cb_context:add_validation_error(<<"data">>
                                   ,<<"required">>
                                   ,kz_json:from_list([{<<"message">>, <<"action '", Action/binary, "' requires a data object">>}])
                                   ,Context
                                   ).

-spec play_media(cb_context:context(), path_token(), kz_term:api_ne_binary()) ->
                        cb_context:context().
play_media(Context, _ConferenceId, 'undefined') ->
    media_id_required(Context);
play_media(Context, ConferenceId, MediaId) ->
    case kz_media_util:media_path(MediaId, cb_context:account_id(Context)) of
        'undefined' ->
            media_id_invalid(Context, MediaId);
        Media ->
            lager:info("playing ~s to conference ~s", [Media, ConferenceId]),
            kapps_conference_command:play(Media, conference(ConferenceId)),
            crossbar_util:response_202(<<"ok">>, Context)
    end.

-spec play_media(cb_context:context(), path_token(), pos_integer(), kz_term:api_ne_binary()) ->
                        cb_context:context().
play_media(Context, _ConferenceId, _ParticipantId, 'undefined') ->
    media_id_required(Context);
play_media(Context, ConferenceId, ParticipantId, MediaId) ->
    case kz_media_util:media_path(MediaId, cb_context:account_id(Context)) of
        'undefined' ->
            media_id_invalid(Context, MediaId);
        Media ->
            lager:info("playing ~s to conference ~s participant ~p", [Media, ConferenceId, ParticipantId]),
            kapps_conference_command:play(Media, ParticipantId, conference(ConferenceId)),
            crossbar_util:response_202(<<"ok">>, Context)
    end.

-spec media_id_invalid(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
media_id_invalid(Context, MediaId) ->
    crossbar_util:response_bad_identifier(MediaId, Context).

-spec media_id_required(cb_context:context()) -> cb_context:context().
media_id_required(Context) ->
    cb_context:add_validation_error([<<"data">>, <<"media_id">>]
                                   ,<<"required">>
                                   ,kz_json:from_list([{<<"message">>, <<"action 'play' requires a media ID or URL">>}])
                                   ,Context
                                   ).

-spec dial(cb_context:context(), path_token(), kz_term:api_object()) -> cb_context:context().
dial(Context, _ConferenceId, 'undefined') ->
    data_required(Context, <<"dial">>);
dial(Context, ConferenceId, Data) ->
    case build_valid_endpoints(Context, ConferenceId, Data) of
        {Context1, []} -> error_no_endpoints(Context1);
        {Context1, Endpoints} ->
            case cb_context:has_errors(Context1) of
                'true' -> Context1;
                'false' ->
                    Resp = exec_dial_endpoints(Context1, ConferenceId, Data, Endpoints),
                    crossbar_util:response_202(<<"attempted dial">>, Resp, Context1)
            end
    end.

-spec build_valid_endpoints(cb_context:context(), kz_term:ne_binary(), kz_json:object()) ->
                                   {cb_context:context(), kz_json:objects()}.
build_valid_endpoints(Context, ConferenceId, Data) ->
    case kz_json_schema:validate(<<"conferences.dial">>, Data) of
        {'ok', ValidData} ->
            build_endpoints_to_dial(Context, ConferenceId, kz_json:get_list_value(<<"endpoints">>, ValidData));
        {'error', Errors} ->
            lager:info("dial data failed to validate"),
            {cb_context:failed(Context, Errors), []}
    end.

-spec exec_dial_endpoints(cb_context:context(), path_token(), kz_json:object(), kz_json:objects()) ->
                                 kz_json:object().
exec_dial_endpoints(Context, ConferenceId, Data, ToDial) ->
    Conference = cb_context:doc(Context),
    CAVs = kz_json:from_list(cb_modules_util:cavs_from_context(Context)),
    Timeout = kz_json:get_integer_value(<<"timeout">>, Data, ?BRIDGE_DEFAULT_SYSTEM_TIMEOUT_S),
    TargetCallId = kz_json:get_ne_binary_value(<<"target_call_id">>, Data),

    Command = [{<<"Account-ID">>, cb_context:account_id(Context)}
              ,{<<"Application-Name">>, <<"dial">>}
              ,{<<"Caller-ID-Name">>, kz_json:get_ne_binary_value(<<"caller_id_name">>, Data, kz_json:get_ne_binary_value(<<"name">>, Conference))}
              ,{<<"Caller-ID-Number">>, kz_json:get_ne_binary_value(<<"caller_id_number">>, Data)}
              ,{<<"Conference-ID">>, ConferenceId}
              ,{<<"Custom-Application-Vars">>, CAVs}
              ,{<<"Endpoints">>, ToDial}
              ,{<<"Participant-Flags">>, kz_json:get_list_value(<<"participant_flags">>, Data)}
              ,{<<"Profile-Name">>, kz_json:get_ne_binary_value(<<"profile_name">>, Data)}
              ,{<<"Msg-ID">>, cb_context:req_id(Context)}
              ,{<<"Outbound-Call-ID">>, kz_json:get_ne_binary_value(<<"outbound_call_id">>, Data)}
              ,{<<"Target-Call-ID">>, TargetCallId}
              ,{<<"Timeout">>, Timeout}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],

    Zone = zone(TargetCallId),
    case kz_amqp_worker:call(Command
                            ,fun(P) -> kapi_conference:publish_dial(Zone, P) end
                            ,fun kapi_conference:dial_resp_v/1
                            ,(Timeout * ?MILLISECONDS_IN_SECOND) * length(ToDial)
                            )
    of
        {'ok', Resp} ->
            kz_json:normalize(kz_api:remove_defaults(Resp));
        {'error', 'timeout'} ->
            kz_json:from_list([{<<"status">>, <<"error">>}
                              ,{<<"message">>, <<"timed out trying to dial endpoints">>}
                              ]);
        {'error', _E} ->
            lager:info("failed to hear back about the dial: ~p", [_E]),
            kz_json:from_list([{<<"status">>, <<"error">>}
                              ,{<<"message">>, <<"conference dial failed to find a media server">>}
                              ])
    end.

-spec zone(kz_term:api_ne_binary()) -> kz_term:ne_binary().
zone('undefined') ->
    kz_config:zone('binary');
zone(TargetCallId) ->
    Req = [{<<"Call-ID">>, TargetCallId}
          ,{<<"Fields">>, <<"all">>}
          ,{<<"Active-Only">>, 'true'}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],

    case kz_amqp_worker:call_collect(Req
                                    ,fun kapi_call:publish_query_channels_req/1
                                    ,{'ecallmgr', fun kapi_call:query_channels_resp_v/1}
                                    )
    of
        {'ok', [Resp|_]} ->
            NodeInfo = kz_nodes:node_to_json(kz_api:node(Resp)),
            Zone = kz_json:get_ne_binary_value(<<"zone">>, NodeInfo, kz_config:zone('binary')),
            lager:info("got back channel resp, using target ~s zone ~s", [TargetCallId, Zone]),
            Zone;
        _E ->
            lager:info("target ~s not found (~p), using local zone: ~p"
                      ,[TargetCallId, _E, kz_config:zone('binary')]
                      ),
            kz_config:zone('binary')
    end.

-record(build_acc, {endpoints = [] :: kz_json:objects()
                   ,call :: kapps_call:call()
                   ,context :: cb_context:context()
                   ,element = 1 :: pos_integer()
                   }).
-type build_acc() :: #build_acc{}.

-define(BUILD_ACC(Es, Call, Context, El)
       ,#build_acc{endpoints=Es
                  ,call=Call
                  ,context=Context
                  ,element=El
                  }
       ).

-spec build_endpoints_to_dial(cb_context:context(), path_token(), kz_term:ne_binaries()) ->
                                     {cb_context:context(), kz_json:objects()}.
build_endpoints_to_dial(Context, ConferenceId, Endpoints) ->
    ?BUILD_ACC(ToDial, _Call, Context1, _Element) =
        lists:foldl(fun build_endpoint/2
                   ,?BUILD_ACC([], create_call(Context, ConferenceId), Context, 1)
                   ,Endpoints
                   ),
    {Context1, ToDial}.

-spec error_no_endpoints(cb_context:context()) -> cb_context:context().
error_no_endpoints(Context) ->
    cb_context:add_validation_error([<<"data">>, <<"endpoints">>]
                                   ,<<"minItems">>
                                   ,kz_json:from_list([{<<"message">>, <<"endpoints failed to resolve to route-able destinations">>}
                                                      ,{<<"target">>, 1}
                                                      ])
                                   ,Context
                                   ).

-spec create_call(cb_context:context(), kz_term:ne_binary()) -> kapps_call:call().
create_call(Context, ConferenceId) ->
    Routines =
        [{F, V}
         || {F, V} <- [{fun kapps_call:set_account_db/2, cb_context:account_db(Context)}
                      ,{fun kapps_call:set_account_id/2, cb_context:account_id(Context)}
                      ,{fun kapps_call:set_resource_type/2, <<"audio">>}
                      ,{fun kapps_call:set_authorizing_id/2, ConferenceId}
                      ,{fun kapps_call:set_authorizing_type/2, <<"conference">>}
                      ],
            'undefined' =/= V
        ],
    kapps_call:exec(Routines, kapps_call:new()).

-spec build_endpoint(kz_term:ne_binary(), build_acc()) -> build_acc().
build_endpoint(<<"sip:", _/binary>>=URI, ?BUILD_ACC(_, _, _, _)=Acc) ->
    lager:info("building SIP endpoint ~s", [URI]),
    build_sip_endpoint(URI, Acc);
build_endpoint(<<_:32/binary>>=EndpointId, ?BUILD_ACC(Endpoints, Call, Context, Element)=Acc) ->
    case kz_datamgr:open_cache_doc(kapps_call:account_db(Call), EndpointId) of
        {'ok', Endpoint} -> build_endpoint_from_doc(Endpoint, Acc);
        {'error', _E} ->
            lager:info("failed to build endpoint ~s: ~p", [EndpointId, _E]),
            ?BUILD_ACC(Endpoints, Call, Context, Element+1)
    end;
build_endpoint(?NE_BINARY=Number, ?BUILD_ACC(Endpoints, Call, Context, Element)=Acc) ->
    case knm_converters:is_reconcilable(Number)
        orelse byte_size(Number) < ?MIN_DIGITS_FOR_DID
    of
        'true' -> build_number_endpoint(Number, Acc);
        'false' ->
            ?BUILD_ACC(Endpoints
                      ,Call
                      ,add_not_endpoint_error(Context, Element)
                      ,Element+1
                      )
    end;
build_endpoint(Device, ?BUILD_ACC(_, _, _, _)=Acc) ->
    DeviceWithId = kz_json:insert_value(<<"id">>, kz_binary:rand_hex(16), Device),
    build_endpoint_from_doc(DeviceWithId, Acc, <<"device">>).

-spec add_not_endpoint_error(cb_context:context(), pos_integer()) -> cb_context:context().
add_not_endpoint_error(Context, Element) when is_integer(Element) ->
    cb_context:add_validation_error([<<"data">>, <<"endpoints">>, Element]
                                   ,<<"enum">>
                                   ,kz_json:from_list([{<<"message">>, <<"Value not a number, device or user ID, or SIP endpoint">>}])
                                   ,Context
                                   ).

-spec build_number_endpoint(kz_term:ne_binary(), build_acc()) -> build_acc().
build_number_endpoint(Number, ?BUILD_ACC(Endpoints, Call, Context, Element)) ->
    AccountRealm = kapps_call:account_realm(Call),
    Endpoint = [{<<"Invite-Format">>, <<"loopback">>}
               ,{<<"Route">>,  Number}
               ,{<<"To-DID">>, Number}
               ,{<<"To-Realm">>, AccountRealm}
               ,{<<"Simplify-Loopback">>, 'true'}
               ,{<<"Custom-Channel-Vars">>
                ,kz_json:from_list([{<<"Account-ID">>, kapps_call:account_id(Call)}
                                   ,{<<"Authorizing-Type">>, <<"conference">>}
                                   ,{<<"Authorizing-ID">>, kapps_call:authorizing_id(Call)}
                                   ,{<<"Loopback-Request-URI">>, <<Number/binary, "@", AccountRealm/binary>>}
                                   ,{<<"Request-URI">>, <<Number/binary, "@", AccountRealm/binary>>}
                                   ])

                }
               ],

    lager:info("adding number ~s endpoint", [Number]),
    ?BUILD_ACC([kz_json:from_list(Endpoint) | Endpoints], Call, Context, Element+1).

-spec build_sip_endpoint(kz_term:ne_binary(), build_acc()) ->
                                build_acc().
build_sip_endpoint(URI, ?BUILD_ACC(Endpoints, Call, Context, Element)) ->
    [#uri{user=SipUsername
         ,domain=SipRealm
         }
    ] = nklib_parse_uri:uris(URI),


    SIPSettings = kz_json:from_list([{<<"invite_format">>, <<"route">>}
                                    ,{<<"route">>, URI}
                                    ,{<<"realm">>, SipRealm}
                                    ,{<<"username">>, SipUsername}
                                    ]),
    Device = kz_json:from_list([{<<"sip">>, SIPSettings}]),
    Properties = kz_json:from_list([{<<"source">>, kz_term:to_binary(?MODULE)}]),
    case kz_endpoint:build(Device, Properties, Call) of
        {'ok', SIPEndpoints} ->
            ?BUILD_ACC(SIPEndpoints ++ Endpoints, Call, Context, Element+1);
        {'error', _E} ->
            lager:info("failed to build SIP URI: ~p", [_E]),
            ?BUILD_ACC(Endpoints, Call, add_not_found_error(Context, URI, Element), Element+1)
    end.

-spec build_endpoint_from_doc(kz_json:object(), build_acc()) -> build_acc().
build_endpoint_from_doc(Endpoint, ?BUILD_ACC(_, _, _, _)=Acc) ->
    case kz_doc:is_soft_deleted(Endpoint)
        orelse kz_doc:is_deleted(Endpoint)
    of
        'true' ->
            lager:info("endpoint ~s is deleted, skipping", [kz_doc:id(Endpoint)]),
            Acc;
        'false' ->
            build_endpoint_from_doc(Endpoint, Acc, kz_doc:type(Endpoint))
    end.

-spec build_endpoint_from_doc(kz_json:object(), build_acc(), kz_term:ne_binary()) -> build_acc().
build_endpoint_from_doc(Device, ?BUILD_ACC(Endpoints, Call, Context, Element), <<"device">>) ->
    Properties = kz_json:from_list([{<<"source">>, kz_term:to_binary(?MODULE)}]),
    case kz_endpoint:build(Device, Properties, Call) of
        {'ok', Legs} -> ?BUILD_ACC(Endpoints ++ Legs, Call, Context, Element+1);
        {'error', _E} ->
            lager:info("failed to build endpoint ~s: ~p", [kz_doc:id(Device), _E]),
            ?BUILD_ACC(Endpoints, Call, add_not_found_error(Context, kz_doc:id(Device), Element), Element+1)
    end;
build_endpoint_from_doc(User, ?BUILD_ACC(Endpoints, Call, Context, Element), <<"user">>) ->
    case maybe_add_user_endpoints(User, Call) of
        [] ->
            lager:info("no endpoints found for user ~s", [kz_doc:id(User)]),
            ?BUILD_ACC(Endpoints, Call, add_no_devices_error(Context, kz_doc:id(User), Element), Element+1);
        Legs ->
            ?BUILD_ACC(Legs++Endpoints, Call, Context, Element+1)
    end;
build_endpoint_from_doc(Endpoint, ?BUILD_ACC(Endpoints, Call, Context, Element), _Type) ->
    lager:info("ignoring endpoint type ~s for ~s", [_Type, kz_doc:id(Endpoint)]),
    ?BUILD_ACC(Endpoints, Call, add_not_found_error(Context, kz_doc:id(Endpoint), Element), Element+1).

-spec maybe_add_user_endpoints(kz_json:ne_binary(), kapps_call:call()) -> kz_json:objects().
maybe_add_user_endpoints(User, Call) ->
    Properties = kz_json:from_list([{<<"source">>, kz_term:to_binary(?MODULE)}]),
    lists:foldr(fun(EndpointId, Acc) ->
                        case kz_endpoint:build(EndpointId, Properties, Call) of
                            {'ok', Endpoint} -> Endpoint ++ Acc;
                            {'error', _E} -> Acc
                        end
                end
               ,[]
               ,kz_attributes:owned_by(kz_doc:id(User), <<"device">>, Call)
               ).

-spec add_no_devices_error(cb_context:context(), kz_term:ne_binary(), pos_integer()) -> cb_context:context().
add_no_devices_error(Context, UserId, Element) when is_integer(Element) ->
    cb_context:add_validation_error([<<"data">>, <<"endpoints">>, Element]
                                   ,<<"minItems">>
                                   ,kz_json:from_list([{<<"message">>, <<"user ", UserId/binary, " failed to resolve to route-able destinations">>}
                                                      ,{<<"target">>, 1}
                                                      ])
                                   ,Context
                                   ).


-spec add_not_found_error(cb_context:context(), kz_term:ne_binary(), pos_integer()) -> cb_context:context().
add_not_found_error(Context, Id, Index) when is_integer(Index) ->
    cb_context:add_validation_error([<<"data">>, <<"endpoints">>, Index]
                                   ,<<"not_found">>
                                   ,kz_json:from_list([{<<"message">>, <<"ID ", Id/binary, " not found">>}])
                                   ,Context
                                   ).

%%%=============================================================================
%%% Participant Actions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_participants_action(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary()) -> cb_context:context().
handle_participants_action(Context, ConferenceId, Action=?MUTE) ->
    handle_participants_action(Context, ConferenceId, Action,
                               fun(P) -> kz_json:is_false(<<"Is-Moderator">>, P)
                                             andalso kz_json:is_true(<<"Speak">>, P)
                               end);
handle_participants_action(Context, ConferenceId, Action=?UNMUTE) ->
    handle_participants_action(Context, ConferenceId, Action,
                               fun(P) -> kz_json:is_false(<<"Is-Moderator">>, P)
                                             andalso kz_json:is_false(<<"Speak">>, P)
                               end);
handle_participants_action(Context, ConferenceId, Action=?DEAF) ->
    handle_participants_action(Context, ConferenceId, Action,
                               fun(P) -> kz_json:is_false(<<"Is-Moderator">>, P)
                                             andalso kz_json:is_true(<<"Hear">>, P)
                               end);
handle_participants_action(Context, ConferenceId, Action=?UNDEAF) ->
    handle_participants_action(Context, ConferenceId, Action,
                               fun(P) -> kz_json:is_false(<<"Is-Moderator">>, P)
                                             andalso kz_json:is_false(<<"Hear">>, P)
                               end);
handle_participants_action(Context, ConferenceId, Action=?KICK) ->
    handle_participants_action(Context, ConferenceId, Action, fun kz_term:always_true/1);
handle_participants_action(Context, ConferenceId, Action=?PLAY) ->
    handle_conference_action(Context, ConferenceId, Action);
handle_participants_action(Context, ConferenceId, ?RELATE) ->
    OnSuccess = fun(C) -> handle_participants_relate(C, ConferenceId) end,
    RelateData = cb_context:req_value(Context, <<"data">>, kz_json:new()),
    WithConference = kz_json:set_value(<<"conference_id">>, ConferenceId, RelateData),

    cb_context:validate_request_data(<<"metaflows.relate">>
                                    ,cb_context:set_req_data(Context, WithConference)
                                    ,OnSuccess
                                    );
handle_participants_action(Context, _ConferenceId, _Action) ->
    lager:error("unhandled conference id ~p participants action: ~p", [_ConferenceId, _Action]),
    cb_context:add_system_error('faulty_request', Context).

%% action applicable to conference participants selected by selector function
-type filter_fun() :: fun((kz_json:object()) -> boolean()).
-spec handle_participants_action(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary(), filter_fun()) ->
                                        cb_context:context().
handle_participants_action(Context, ConferenceId, Action, Selector) ->
    ConfData = request_conference_details(ConferenceId),
    Participants = extract_participants(ConfData),
    Conf = conference(ConferenceId),
    _ = [perform_participant_action(Conf, Action, kz_json:get_ne_binary_value(<<"Participant-ID">>, P))
         || P <- Participants,
            filter_participant(P, Selector)
        ],
    Context.

-spec filter_participant(kz_json:object(), filter_fun()) -> boolean().
filter_participant(JObj, Fun) ->
    ConfVars = kz_json:get_json_value(<<"Conference-Channel-Vars">>, JObj, kz_json:new()),
    Fun(ConfVars).

-spec handle_participants_relate(cb_context:context(), path_token()) ->
                                        cb_context:context().
handle_participants_relate(Context, ConferenceId) ->
    ConfData = request_conference_details(ConferenceId),
    Participants = extract_participants(ConfData),

    ParticipantId = kz_term:to_integer(cb_context:req_value(Context, <<"participant_id">>)),
    OtherParticipantId = kz_term:to_integer(cb_context:req_value(Context, <<"other_participant">>)),

    case find_participants(Participants, ParticipantId, OtherParticipantId) of
        [] ->
            lists:foldl(fun participant_not_found/2
                       ,Context
                       ,[ParticipantId, OtherParticipantId]
                       );
        [ParticipantId] ->
            participant_not_found(OtherParticipantId, Context);
        [OtherParticipantId] ->
            participant_not_found(ParticipantId, Context);
        [_, _] ->
            relate(Context, ConferenceId, ParticipantId, OtherParticipantId)
    end.

-spec relate(cb_context:context(), path_token(), pos_integer(), pos_integer()) ->
                    cb_context:context().
relate(Context, ConferenceId, ParticipantId, OtherParticipantId) ->
    Conference = conference(ConferenceId),
    Relationship = cb_context:req_value(Context, <<"relationship">>, <<"clear">>),

    kapps_conference_command:relate_participants(ParticipantId, OtherParticipantId, Relationship, Conference),
    lager:info("relating ~p to ~p with ~s in conference ~s"
              ,[ParticipantId, OtherParticipantId, Relationship, ConferenceId]
              ),
    crossbar_util:response_202(<<"relating participants">>, Context).

-spec find_participants(kz_json:objects(), pos_integer(), pos_integer()) ->
                               [pos_integer()].
find_participants(Participants, ParticipantId, OtherParticipantId) ->
    [PID || P <- Participants,
            PID <- [kz_json:get_integer_value(<<"Participant-ID">>, P)],
            ParticipantId =:= PID
                orelse OtherParticipantId =:= PID
    ].

-spec participant_not_found(pos_integer(), cb_context:context()) -> cb_context:context().
participant_not_found(ParticipantId, Context) ->
    cb_context:add_system_error('bad_identifier'
                               ,kz_json:from_list([{<<"id">>, ParticipantId}])
                               ,Context
                               ).

-spec perform_participant_action(kapps_conference:conference(), kz_term:ne_binary(), kz_term:api_integer()) -> 'ok'.
perform_participant_action(Conference, ?MUTE, ParticipantId) ->
    kapps_conference_command:mute_participant(ParticipantId, Conference);
perform_participant_action(Conference, ?UNMUTE, ParticipantId) ->
    kapps_conference_command:unmute_participant(ParticipantId, Conference);
perform_participant_action(Conference, ?DEAF, ParticipantId) ->
    kapps_conference_command:deaf_participant(ParticipantId, Conference);
perform_participant_action(Conference, ?UNDEAF, ParticipantId) ->
    kapps_conference_command:undeaf_participant(ParticipantId, Conference);
perform_participant_action(Conference, ?KICK, ParticipantId) ->
    kapps_conference_command:kick(ParticipantId, Conference).

%% add real-time call-info to participants
-spec enrich_participant(kz_term:ne_binary(), kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
enrich_participant(ParticipantId, ConferenceId, Context) ->
    Participants = extract_participants(
                     request_conference_details(ConferenceId)
                    ),
    [Normalized|_] = [kz_json:normalize_jobj(JObj)
                      || JObj <- Participants,
                         kz_json:get_binary_value(<<"Participant-ID">>, JObj) == ParticipantId
                     ] ++ [kz_json:new()],
    cb_context:set_resp_data(Context, Normalized).

-spec enrich_participants(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
enrich_participants(ConferenceId, Context) ->
    Participants = extract_participants(
                     request_conference_details(ConferenceId)
                    ),
    Normalized = [kz_json:normalize_jobj(JObj) || JObj <- Participants],
    cb_context:set_resp_data(Context, Normalized).

-spec enrich_conference(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
enrich_conference(ConferenceId, Context) ->
    RealtimeData = conference_realtime_data(ConferenceId),
    Response = kz_json:set_value(<<"_read_only">>, RealtimeData, cb_context:resp_data(Context)),
    cb_context:set_resp_data(Context, Response).

-spec conference_realtime_data(kz_term:ne_binary()) -> kz_json:object().
conference_realtime_data(ConferenceId) ->
    ConferenceDetails = request_conference_details(ConferenceId),
    Participants = extract_participants(ConferenceDetails),
    {Moderators, Members} = partition_participants_count(Participants),
    kz_json:from_list(
      [{<<"members">>, Members}
      ,{<<"moderators">>, Moderators}
      ,{<<"duration">>, run_time(ConferenceDetails)}
      ,{<<"is_locked">>, kz_json:get_value(<<"Locked">>, ConferenceDetails, 'false')}
      ,{<<"participants">>, [kz_json:normalize_jobj(Participant) || Participant <- Participants]}
      ]).

-spec request_conference_details(kz_term:ne_binary()) -> kz_json:object().
request_conference_details(ConferenceId) ->
    Req = [{<<"Conference-ID">>, ConferenceId}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case kz_amqp_worker:call_collect(Req, fun kapi_conference:publish_search_req/1, {'ecallmgr', 'true'}) of
        {'error', _E} ->
            lager:debug("unable to lookup conference details: ~p", [_E]),
            kz_json:new();
        {_, JObjs} -> find_conference_details(JObjs)
    end.

-spec find_conference_details(kz_json:objects()) -> kz_json:object().
find_conference_details(JObjs) ->
    ValidRespones = [JObj || JObj <- JObjs, kapi_conference:search_resp_v(JObj)],
    case lists:sort(fun(A, B) ->
                            run_time(A) > run_time(B)
                    end
                   ,ValidRespones
                   )
    of
        [Latest|_] -> Latest;
        _Else -> kz_json:new()
    end.

%%%=============================================================================
%%% Utility functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec conference(kz_term:ne_binary()) -> kapps_conference:conference().
conference(ConferenceId) ->
    kapps_conference:set_id(ConferenceId, kapps_conference:new()).

-spec run_time(kz_json:object()) -> integer().
run_time(Conf) -> kz_json:get_value(<<"Run-Time">>, Conf, 0).

-spec extract_participants(kz_json:object()) -> kz_json:objects().
extract_participants(JObj) ->
    add_duration_to_participants(kz_json:get_value(<<"Participants">>, JObj, [])).

-spec calc_duration(kz_json:object()) -> integer().
calc_duration(Participant) ->
    Stamp = kz_time:now_s(),
    JoinTime = kz_json:get_value(<<"Join-Time">>, Participant),
    Stamp - JoinTime.

-spec add_duration_to_participants(kz_json:objects()) -> kz_json:objects().
add_duration_to_participants(Participants) ->
    [kz_json:set_value(<<"Duration">>, calc_duration(Participant), Participant)
     || Participant <- Participants
    ].

-spec partition_participants_count(kz_json:objects()) -> {integer(), integer()}.
partition_participants_count(Participants) ->
    partition_participants_count(Participants, fun(P) -> kz_json:is_true([<<"Conference-Channel-Vars">>, <<"Is-Moderator">>], P) end).

-spec partition_participants_count(kz_json:objects(), fun((kz_json:object()) -> boolean())) -> {integer(), integer()}.
partition_participants_count(Participants, Fun) ->
    {A, B} = partition_participants(Participants, Fun),
    {erlang:length(A), erlang:length(B)}.

-spec partition_participants(kz_json:objects(), fun()) -> {kz_json:objects(), kz_json:objects()}.
partition_participants(Participants, Fun) ->
    lists:partition(Fun, Participants).

-spec search_conferences(cb_context:context()) -> cb_context:context().
search_conferences(Context) ->
    AccountId = cb_context:account_id(Context),
    Req = [{<<"Account-ID">>, AccountId}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case kz_amqp_worker:call_collect(Req, fun kapi_conference:publish_search_req/1, {'ecallmgr', 'true'}) of
        {'error', _E} ->
            lager:debug("error searching conferences for account ~s: ~p", [AccountId, _E]),
            cb_context:store(Context, 'conferences', kz_json:new());
        {'ok', JObjs} ->
            Res = lists:foldl(fun search_conferences_fold/2, kz_json:new(), JObjs),
            cb_context:store(Context, 'conferences', Res)
    end.

-spec search_conferences_fold(kz_json:object(), kz_json:object()) ->
                                     kz_json:object().
search_conferences_fold(JObj, Acc) ->
    V = kz_json:get_json_value(<<"Conferences">>, JObj, kz_json:new()),
    kz_json:merge_jobjs(V, Acc).
