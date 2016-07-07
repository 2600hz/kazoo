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

-define(PARTICIPANT_INFO_FIELDS, [<<"Is-Moderator">>
                                 ,<<"Video">>
                                 ,<<"Current-Energy">>
                                 ,<<"Energy-Level">>
                                 ,<<"Participant-ID">>
                                 ,<<"Mute-Detect">>
                                 ,<<"Talking">>
                                 ,<<"Speak">>
                                 ,<<"Hear">>
                                 ,<<"Floor">>
                                 ,<<"Join-Time">>
                                 ,<<"Duration">>
                                 ]).

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
allowed_methods(_ConferenceId, ?PARTICIPANTS, _ParticipantId) -> [?HTTP_PUT].

-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token(), path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.
resource_exists(_, _) -> 'true'.
resource_exists(_, _, _) -> 'true'.

-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), ConferenceId::path_token()) -> cb_context:context().
-spec validate(cb_context:context(), ConferenceId::path_token(), Section::path_token()) -> cb_context:context().
-spec validate(cb_context:context(), ConferenceId::path_token(), Section::path_token(), SectionId::path_token()) -> cb_context:context().
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
    Context1 = crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2),
    EnrichedDocs = [ enrich_conference(JObj) || JObj <- cb_context:doc(Context1)],
    cb_context:set_resp_data(Context1, EnrichedDocs);
validate_conferences(?HTTP_PUT, Context) ->
    maybe_create_conference(Context).

-spec validate_conference(http_method(), cb_context:context(), ne_binary()) -> cb_context:context().
validate_conference(?HTTP_GET, Context0, ConferenceId) ->
    Context1 = load_conference(ConferenceId, Context0),
    case cb_context:resp_status(Context1) of
        'success' -> enrich_conference(ConferenceId, Context1);
        _Else -> Context1
    end;
validate_conference(?HTTP_POST, Context, ConferenceId) ->
    update_conference(ConferenceId, Context);
validate_conference(?HTTP_PUT, Context, ConferenceId) ->
    load_conference(ConferenceId, Context);
validate_conference(?HTTP_PATCH, Context, ConferenceId) ->
    patch_conference(ConferenceId, Context);
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
    Conference = kapps_conference:set_id(ConferenceId, kapps_conference:new()),
    perform_participant_action(Conference, Action, kz_util:to_integer(ParticipantId)),
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
    [kz_json:get_value(<<"value">>, JObj) | Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new conference document with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec maybe_create_conference(cb_context:context()) -> cb_context:context().
maybe_create_conference(Context) ->
    AccountDb = cb_context:account_db(Context),
    case kz_datamgr:get_all_results(AccountDb, ?CB_LIST_BY_NUMBER) of
        {'error', _R} ->
            cb_context:add_system_error('datastore_fault', Context);
        {'ok', JObjs} ->
            Numbers = kz_datamgr:get_result_keys(JObjs),
            ReqData = cb_context:req_data(Context),
            MemberNumbers = kz_json:get_value([<<"member">>, <<"numbers">>], ReqData, []),
            ModNumbers = kz_json:get_value([<<"moderator">>, <<"numbers">>], ReqData, []),
            case is_number_already_used(Numbers, MemberNumbers ++ ModNumbers) of
                'false' ->
                    create_conference(Context);
                {'true', Number} ->
                    lager:error("number ~s is already used", [Number]),
                    Error = kz_json:from_list([{<<"message">>, <<"Number already in use">>},{<<"cause">>, Number}]),
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
    Conference = kapps_conference:set_id(ConferenceId, kapps_conference:new()),
    kapps_conference_command:lock(Conference),
    crossbar_util:response_202(<<"ok">>, Context);
handle_conference_action(Context, ConferenceId, <<"unlock">>) ->
    Conference = kapps_conference:set_id(ConferenceId, kapps_conference:new()),
    kapps_conference_command:unlock(Conference),
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
                               fun(P) -> kz_json:is_false(<<"Is-Moderator">>, P) andalso kz_json:is_true(<<"Speak">>, P) end);
handle_participants_action(Context, ConferenceId, Action=?UNMUTE) ->
    handle_participants_action(Context, ConferenceId, Action,
                               fun(P) -> kz_json:is_false(<<"Is-Moderator">>, P) andalso kz_json:is_false(<<"Speak">>, P) end);
handle_participants_action(Context, ConferenceId, Action=?DEAF) ->
    handle_participants_action(Context, ConferenceId, Action,
                               fun(P) -> kz_json:is_false(<<"Is-Moderator">>, P) andalso kz_json:is_true(<<"Hear">>, P) end);
handle_participants_action(Context, ConferenceId, Action=?UNDEAF) ->
    handle_participants_action(Context, ConferenceId, Action,
                               fun(P) -> kz_json:is_false(<<"Is-Moderator">>, P) andalso kz_json:is_false(<<"Hear">>, P) end);
handle_participants_action(Context, ConferenceId, Action=?KICK) ->
    handle_participants_action(Context, ConferenceId, Action, fun(_) -> true end);
handle_participants_action(Context, _ConferenceId, _Action) ->
    lager:error("unhandled conference id ~p participants action: ~p", [_ConferenceId, _Action]),
    cb_context:add_system_error('faulty_request', Context).

%% action applicable to conference participants selected by selector function
-spec handle_participants_action(cb_context:context(), ne_binary(), ne_binary(), function()) -> cb_context:context().
handle_participants_action(Context, ConferenceId, Action, Selector) ->
    Participants = extract_participants(
                     request_conference_details(ConferenceId)
                    ),
    Conference = kapps_conference:set_id(ConferenceId, kapps_conference:new()),
    _ = [perform_participant_action(Conference, Action, kz_json:get_value(<<"Participant-ID">>, P))
         || P <- Participants, Selector(P)
        ],
    enrich_participants(ConferenceId, Context).

-spec perform_participant_action(kapps_conference:conference(), ne_binary(), api_integer()) -> 'ok'.
perform_participant_action(Conference, ?MUTE, ParticipantId) ->
    kapps_conference_command:mute_participant(ParticipantId, Conference),
    kapps_conference_command:prompt(<<"conf-muted">>, ParticipantId, Conference);
perform_participant_action(Conference, ?UNMUTE, ParticipantId) ->
    kapps_conference_command:unmute_participant(ParticipantId, Conference),
    kapps_conference_command:prompt(<<"conf-unmuted">>, ParticipantId, Conference);
perform_participant_action(Conference, ?DEAF, ParticipantId) ->
    kapps_conference_command:deaf_participant(ParticipantId, Conference),
    kapps_conference_command:prompt(<<"conf-deaf">>, ParticipantId, Conference);
perform_participant_action(Conference, ?UNDEAF, ParticipantId) ->
    kapps_conference_command:undeaf_participant(ParticipantId, Conference),
    kapps_conference_command:prompt(<<"conf-undeaf">>, ParticipantId, Conference);
perform_participant_action(Conference, ?KICK, ParticipantId) ->
    kapps_conference_command:kick(ParticipantId, Conference).

%% add real-time call-info to participants
-spec enrich_participants(ne_binary(), cb_context:context()) -> cb_context:context().
enrich_participants(ConferenceId, Context) ->
    Participants = extract_participants(
                     request_conference_details(ConferenceId)
                    ),
    Normalized = [kz_json:normalize_jobj(JObj) || JObj <- request_call_details(Participants)],
    crossbar_util:response(Normalized, Context).

-spec enrich_conference(ne_binary(), cb_context:context()) -> cb_context:context().
enrich_conference(ConferenceId, Context) ->
    JObj = request_conference_details(ConferenceId),
    Participants = extract_participants(JObj),
    Enriched = kz_json:from_list(
                 [{<<"members">>, count_members(Participants)}
                 ,{<<"admins">>, count_admins(Participants)}
                 ,{<<"duration">>, run_time(JObj)}
                 ,{<<"is_locked">>, kz_json:get_value(<<"Locked">>, JObj, 'false')}
                 ,{<<"participants">>, [kz_json:normalize_jobj(Participant) || Participant <- Participants]}
                 ]
                ),
    Response = kz_json:set_value(<<"_read_only">>, Enriched, cb_context:resp_data(Context)),
    cb_context:set_resp_data(Context, Response).

-spec enrich_conference(kz_json:object()) -> kz_json:object().
enrich_conference(JObj) ->
    ConferenceId = kz_doc:id(JObj),
    ConferenceDetails = request_conference_details(ConferenceId),
    Participants = extract_participants(ConferenceDetails),
    Enriched = kz_json:from_list(
                 [{<<"members">>, count_members(Participants)}
                 ,{<<"admins">>, count_admins(Participants)}
                 ,{<<"duration">>, run_time(ConferenceDetails)}
                 ,{<<"is_locked">>, kz_json:get_value(<<"Locked">>, ConferenceDetails, 'false')}
                 ]),
    kz_json:set_value(<<"_read_only">>, Enriched, JObj).


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

-spec request_call_details(kz_json:objects()) -> kz_json:objects().
-spec request_call_details(kz_json:objects(), kz_json:obejcts()) -> kz_json:objects().
request_call_details(Participants) -> request_call_details(Participants, []).
request_call_details([], JObjs) -> JObjs;
request_call_details([Participant | Participants], JObjs) ->
    CallId = kz_json:get_value(<<"Call-ID">>, Participant),
    Req = [{<<"Call-ID">>, CallId}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case kapps_util:amqp_pool_request(Req
                                     ,fun kapi_call:publish_channel_status_req/1
                                     ,fun kapi_call:channel_status_resp_v/1
                                     ) of
        {'error', _E} ->
            lager:debug("error fetching channel status for ~s (~p)", [CallId, _E]),
            request_call_details(Participants, [filter_fields(Participant) | JObjs]);
        {'ok', Resp} ->
            JObj = kz_json:set_value(<<"channel">>, Resp, filter_fields(Participant)),
            request_call_details(Participants, [JObj | JObjs])
    end.

%%%===================================================================
%%% Utility functions
%%%===================================================================
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

-spec filter_fields(kz_json:object()) -> kz_json:object().
filter_fields(Participant) ->
    kz_json:filter(fun({Key, _}) ->
                           lists:member(Key, ?PARTICIPANT_INFO_FIELDS)
                   end, Participant).

-spec count_admins(kz_json:objects()) -> integer().
count_admins(Participants) ->
    erlang:length([ P || P <- Participants, kz_json:is_true(<<"Is-Moderator">>, P) ]).

-spec count_members(kz_json:objects()) -> integer().
count_members(Participants) ->
    erlang:length([ P || P <- Participants, kz_json:is_false(<<"Is-Moderator">>, P) ]).
