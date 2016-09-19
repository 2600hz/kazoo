%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%% Conferences module
%%%
%%% Handle client requests for conference documents
%%%
%%% URI schema:
%%% /v2/accounts/{AccountId}/conferences
%%% /v2/accounts/{AccountId}/conferences/{ConferenceID}
%%% /v2/accounts/{AccountId}/conferences/{ConferenceID}/participants
%%% /v2/accounts/{AccountId}/conferences/{ConferenceID}/participants/{ParticipantId}
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%   Roman Galeev

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

-include("crossbar.hrl").

-define(CB_LIST, <<"conferences/crossbar_listing">>).
-define(CB_LIST_BY_NUMBER, <<"conference/listing_by_number">>).

-define(PARTICIPANTS, <<"participants">>).
-define(MUTE, <<"mute">>).
-define(UNMUTE, <<"unmute">>).
-define(DEAF, <<"deaf">>).
-define(UNDEAF, <<"undeaf">>).
-define(KICK, <<"kick">>).

-define(PUT_ACTION, <<"action">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.conferences">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.conferences">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.conferences">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.conferences">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.conferences">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.conferences">>, ?MODULE, 'patch'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.conferences">>, ?MODULE, 'delete').

%%%===================================================================
%%% REST API Callbacks
%%%===================================================================

-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token(), path_token()) -> http_methods().
allowed_methods() -> [?HTTP_GET, ?HTTP_PUT].
allowed_methods(_ConferenceId) -> [?HTTP_GET, ?HTTP_PATCH, ?HTTP_DELETE, ?HTTP_POST, ?HTTP_PUT].
allowed_methods(_ConferenceId, ?PARTICIPANTS) -> [?HTTP_GET, ?HTTP_PUT].
allowed_methods(_ConferenceId, ?PARTICIPANTS, _ParticipantId) -> [?HTTP_GET, ?HTTP_PUT].

-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token(), path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.
resource_exists(_, _) -> 'true'.
resource_exists(_, _, _) -> 'true'.

-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
validate(Context) ->
    validate_conferences(cb_context:req_verb(Context), Context).
validate(Context, ConferenceId) ->
    validate_conference(cb_context:req_verb(Context), Context, ConferenceId).
validate(Context, ConferenceId, ?PARTICIPANTS) ->
    validate_participants(cb_context:req_verb(Context), Context, ConferenceId).
validate(Context, ConferenceId, ?PARTICIPANTS, ParticipantId) ->
    validate_participant(cb_context:req_verb(Context), Context, ConferenceId, ParticipantId).

%%%===================================================================
%%% Request object validators
%%%===================================================================
-spec validate_conferences(http_method(), cb_context:context()) -> cb_context:context().
validate_conferences(?HTTP_GET, Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2);
validate_conferences(?HTTP_PUT, Context) ->
    check_numbers(Context, fun() -> create_conference(Context) end).

-spec validate_conference(http_method(), cb_context:context(), ne_binary()) -> cb_context:context().
validate_conference(?HTTP_GET, Context0, ConferenceId) ->
    Context1 = load_conference(ConferenceId, Context0),
    case cb_context:resp_status(Context1) of
        'success' -> enrich_conference(ConferenceId, Context1);
        _Else -> Context1
    end;
validate_conference(?HTTP_POST, Context, ConferenceId) ->
    check_numbers(Context, fun() ->
                                   update_conference(ConferenceId, Context)
                           end);
validate_conference(?HTTP_PUT, Context, ConferenceId) ->
    check_numbers(Context, fun() ->
                                   load_conference(ConferenceId, Context)
                           end);
validate_conference(?HTTP_PATCH, Context, ConferenceId) ->
    check_numbers(Context, fun() ->
                                   patch_conference(ConferenceId, Context)
                           end);
validate_conference(?HTTP_DELETE, Context, ConferenceId) ->
    load_conference(ConferenceId, Context).

-spec validate_participants(http_method(), cb_context:context(), ne_binary()) -> cb_context:context().
validate_participants(?HTTP_GET, Context0, ConferenceId) ->
    Context1 = load_conference(ConferenceId, Context0),
    case cb_context:resp_status(Context1) of
        'success' -> enrich_participants(ConferenceId, Context1);
        _Else -> Context1
    end;
validate_participants(?HTTP_PUT, Context, ConferenceId) ->
    load_conference(ConferenceId, Context).

-spec validate_participant(http_method(), cb_context:context(), ne_binary(), ne_binary()) -> cb_context:context().
validate_participant(?HTTP_GET, Context0, ConferenceId, ParticipantId) ->
    Context1 = load_conference(ConferenceId, Context0),
    case cb_context:resp_status(Context1) of
        'success' -> enrich_participant(ParticipantId, ConferenceId, Context1);
        _Else -> Context1
    end;
validate_participant(?HTTP_PUT, Context, ConferenceId, _ParticipantId) ->
    load_conference(ConferenceId, Context).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _ConferenceId) ->
    crossbar_doc:save(Context).

-spec put(cb_context:context()) -> cb_context:context().
-spec put(cb_context:context(), path_token()) -> cb_context:context().
-spec put(cb_context:context(), path_token(), path_token()) -> cb_context:context().
-spec put(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(Context).

put(Context, ConferenceId) ->
    Action = cb_context:req_value(Context, ?PUT_ACTION),
    handle_conference_action(Context, ConferenceId, Action).

put(Context, ConferenceId, ?PARTICIPANTS) ->
    Action = cb_context:req_value(Context, ?PUT_ACTION),
    handle_participants_action(Context, ConferenceId, Action).

put(Context, ConferenceId, ?PARTICIPANTS, ParticipantId) ->
    Action = cb_context:req_value(Context, ?PUT_ACTION),
    perform_participant_action(conference(ConferenceId), Action, kz_util:to_integer(ParticipantId)),
    crossbar_util:response_202(<<"ok">>, Context).

-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, _) ->
    crossbar_doc:save(Context).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _) ->
    crossbar_doc:delete(Context).

%%%===================================================================
%%% Conference validation helpers
%%%===================================================================
-spec load_conference(ne_binary(), cb_context:context()) -> kz_json:object().
load_conference(ConferenceId, Context) ->
    crossbar_doc:load(ConferenceId, Context).

-spec create_conference(cb_context:context()) -> cb_context:context().
create_conference(Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"conferences">>, Context, OnSuccess).

-spec update_conference(ne_binary(), cb_context:context()) -> cb_context:context().
update_conference(ConferenceId, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(ConferenceId, C) end,
    cb_context:validate_request_data(<<"conferences">>, Context, OnSuccess).

-spec patch_conference(ne_binary(), cb_context:context()) -> cb_context:context().
patch_conference(ConferenceId, Context) ->
    crossbar_doc:patch_and_validate(ConferenceId, Context, fun update_conference/2).

-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    cb_context:set_doc(Context
                      ,kz_doc:set_type(cb_context:doc(Context), <<"conference">>)
                      );
on_successful_validation(ConferenceId, Context) ->
    crossbar_doc:load_merge(ConferenceId, Context, ?TYPE_CHECK_OPTION(<<"conference">>)).

-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [enrich_conference(kz_json:get_value(<<"value">>, JObj)) | Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new conference document with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec check_numbers(cb_context:context(), fun()) -> cb_context:context().
check_numbers(Context, Fun) ->
    AccountDb = cb_context:account_db(Context),
    case kz_datamgr:get_all_results(AccountDb, ?CB_LIST_BY_NUMBER) of
        {'error', _R} ->
            cb_context:add_system_error('datastore_fault', Context);
        {'ok', JObjs} ->
            JConf = cb_context:req_data(Context),
            Numbers = kz_datamgr:get_result_keys(JObjs),
            Conf = kz_json:get_value(<<"conference_numbers">>, JConf, []),
            Member = kz_json:get_value([<<"member">>, <<"numbers">>], JConf, []),
            Moderator = kz_json:get_value([<<"moderator">>, <<"numbers">>], JConf, []),
            case is_number_already_used(Numbers, Conf ++ Member ++ Moderator) of
                'false' -> Fun();
                {'true', Number} ->
                    lager:error("number ~s is already used", [Number]),
                    Error = kz_json:from_list([{<<"message">>, <<"Number already in use">>}
                                              ,{<<"cause">>, Number}]),
                    cb_context:add_validation_error([<<"numbers">>], <<"unique">>, Error, Context)
            end
    end.

-spec is_number_already_used(ne_binaries(), ne_binaries()) -> 'false' | {'true', ne_binary()}.
is_number_already_used(Numbers, NewNumbers) ->
    is_number_already_used(Numbers, NewNumbers, 'false').

-spec is_number_already_used(ne_binaries(), ne_binaries(), 'false') -> 'false' | {'true', ne_binary()}.
is_number_already_used(_, [], Acc) -> Acc;
is_number_already_used(Numbers, [Number|NewNumbers], Acc) ->
    case lists:member(Number, Numbers) of
        'true' -> {'true', Number};
        'false' ->
            is_number_already_used(Numbers, NewNumbers, Acc)
    end.

%%%===================================================================
%%% Conterence Actions
%%%===================================================================
-spec handle_conference_action(cb_context:context(), ne_binary(), ne_binary()) -> cb_context:context().
handle_conference_action(Context, ConferenceId, <<"lock">>) ->
    kapps_conference_command:lock(conference(ConferenceId)),
    crossbar_util:response_202(<<"ok">>, Context);
handle_conference_action(Context, ConferenceId, <<"unlock">>) ->
    kapps_conference_command:unlock(conference(ConferenceId)),
    crossbar_util:response_202(<<"ok">>, Context);
handle_conference_action(Context, ConferenceId, Action) ->
    lager:error("unhandled conference id ~p action: ~p", [ConferenceId, Action]),
    cb_context:add_system_error('faulty_request', Context).

%%%===================================================================
%%% Participant Actions
%%%===================================================================
-spec handle_participants_action(cb_context:context(), ne_binary(), ne_binary()) -> cb_context:context().
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
    handle_participants_action(Context, ConferenceId, Action, fun(_) -> 'true' end);
handle_participants_action(Context, _ConferenceId, _Action) ->
    lager:error("unhandled conference id ~p participants action: ~p", [_ConferenceId, _Action]),
    cb_context:add_system_error('faulty_request', Context).

%% action applicable to conference participants selected by selector function
-spec handle_participants_action(cb_context:context(), ne_binary(), ne_binary(), function()) -> cb_context:context().
handle_participants_action(Context, ConferenceId, Action, Selector) ->
    ConfData = request_conference_details(ConferenceId),
    Participants = extract_participants(ConfData),
    Conf = conference(ConferenceId),
    _ = [perform_participant_action(Conf, Action, kz_json:get_value(<<"Participant-ID">>, P))
         || P <- Participants, filter_participant(P, Selector)
        ],
    Context.

filter_participant(JObj, Fun) ->
    ConfVars = kz_json:get_value(<<"Conference-Channel-Vars">>, JObj, kz_json:new()),
    Fun(ConfVars).

-spec perform_participant_action(kapps_conference:conference(), ne_binary(), api_integer()) -> 'ok'.
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
-spec enrich_participant(ne_binary(), ne_binary(), cb_context:context()) -> cb_context:context().
enrich_participant(ParticipantId, ConferenceId, Context) ->
    Participants = extract_participants(
                     request_conference_details(ConferenceId)
                    ),
    [Normalized|_] = [kz_json:normalize_jobj(JObj)
                      || JObj <- Participants,
                         kz_json:get_binary_value(<<"Participant-ID">>, JObj) == ParticipantId
                     ] ++ [kz_json:new()],
    cb_context:set_resp_data(Context, Normalized).

-spec enrich_participants(ne_binary(), cb_context:context()) -> cb_context:context().
enrich_participants(ConferenceId, Context) ->
    Participants = extract_participants(
                     request_conference_details(ConferenceId)
                    ),
    Normalized = [kz_json:normalize_jobj(JObj) || JObj <- Participants],
    cb_context:set_resp_data(Context, Normalized).

-spec enrich_conference(ne_binary(), cb_context:context()) -> cb_context:context().
enrich_conference(ConferenceId, Context) ->
    RealtimeData = conference_realtime_data(ConferenceId),
    Response = kz_json:set_value(<<"_read_only">>, RealtimeData, cb_context:resp_data(Context)),
    cb_context:set_resp_data(Context, Response).

-spec enrich_conference(kz_json:object()) -> kz_json:object().
enrich_conference(JObj) ->
    ConferenceId = kz_doc:id(JObj),
    RealtimeData = conference_realtime_data(ConferenceId),
    kz_json:merge_jobjs(kz_json:delete_key(<<"participants">>, RealtimeData), JObj).

-spec conference_realtime_data(ne_binary()) -> kz_json:object().
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

-spec request_conference_details(ne_binary()) -> kz_json:object().
request_conference_details(ConferenceId) ->
    Req = [{<<"Conference-ID">>, ConferenceId}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case kapps_util:amqp_pool_collect(Req, fun kapi_conference:publish_search_req/1, {'ecallmgr', 'true'}) of
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

%%%===================================================================
%%% Utility functions
%%%===================================================================
-spec conference(ne_binary()) -> kz_json:object().
conference(ConferenceId) ->
    kapps_conference:set_id(ConferenceId, kapps_conference:new()).

-spec run_time(kz_json:object()) -> integer().
run_time(Conf) -> kz_json:get_value(<<"Run-Time">>, Conf, 0).

-spec extract_participants(kz_json:object()) -> kz_json:objects().
extract_participants(JObj) ->
    add_duration_to_participants(kz_json:get_value(<<"Participants">>, JObj, [])).

-spec calc_duration(kz_json:object()) -> integer().
calc_duration(Participant) ->
    Stamp = kz_util:current_tstamp(),
    JoinTime = kz_json:get_value(<<"Join-Time">>, Participant),
    Stamp - JoinTime.

-spec add_duration_to_participants(kz_json:objects()) -> kz_json:objects().
add_duration_to_participants(Participants) ->
    [kz_json:set_value(<<"Duration">>, calc_duration(Participant), Participant)
     || Participant <- Participants
    ].

-spec partition_participants_count(kz_json:objects()) -> {integer(), integer()}.
partition_participants_count(Participants) ->
    partition_participants_count(Participants, fun(P) -> kz_json:is_true(<<"Is-Moderator">>, P) end).

-spec partition_participants_count(kz_json:objects(), fun((kz_json:object()) -> boolean())) -> {integer(), integer()}.
partition_participants_count(Participants, Fun) ->
    {A, B} = partition_participants(Participants, Fun),
    {erlang:length(A), erlang:length(B)}.

%% -spec partition_participants(kz_json:objects()) -> {kz_json:objects(), kz_json:objects()}.
%% partition_participants(Participants) ->
%%     partition_participants(Participants, fun(P) -> kz_json:is_true(<<"Is-Moderator">>, P) end).

-spec partition_participants(kz_json:objects(), fun()) -> {kz_json:objects(), kz_json:objects()}.
partition_participants(Participants, Fun) ->
    lists:partition(Fun, Participants).
