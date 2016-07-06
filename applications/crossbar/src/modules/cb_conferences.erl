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
        ,post/2, post/3, post/4
        ,put/1
        ,patch/2
        ,delete/2
        ]).

-include("crossbar.hrl").

-define(CB_LIST, <<"conferences/crossbar_listing">>).
-define(CB_LIST_BY_NUMBER, <<"conference/listing_by_number">>).

-define(MUTE, <<"mute">>).
-define(UNMUTE, <<"unmute">>).
-define(DEAF, <<"deaf">>).
-define(UNDEAF, <<"undeaf">>).
-define(KICK, <<"kick">>).

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
allowed_methods(_ConferenceId) -> [?HTTP_GET, ?HTTP_PATCH, ?HTTP_DELETE, ?HTTP_POST].
allowed_methods(_ConferenceId, <<"participants">>) -> [?HTTP_GET, ?HTTP_POST].
allowed_methods(_ConferenceId, <<"participants">>, _ParticipantId) -> [?HTTP_POST].

-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token(), path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.
resource_exists(_, _) -> 'true'.
resource_exists(_, _, _) -> 'true'.

-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), ConfId::path_token()) -> cb_context:context().
-spec validate(cb_context:context(), ConfId::path_token(), Section::path_token()) -> cb_context:context().
-spec validate(cb_context:context(), ConfId::path_token(), Section::path_token(), SectionId::path_token()) -> cb_context:context().
validate(Context) ->
    validate_conferences_uri(cb_context:req_verb(Context), Context).
validate(Context, ConfId) ->
    validate_conference_uri(cb_context:req_verb(Context), Context, ConfId).
validate(Context, ConfId, <<"participants">>) ->
    validate_participants_uri(cb_context:req_verb(Context), Context, ConfId).
validate(Context, ConfId, <<"participants">>, ParticipantId) ->
    validate_participant_uri(cb_context:req_verb(Context), Context, ConfId, ParticipantId).

%%%===================================================================
%%% URI/Method validators
%%%===================================================================

-spec validate_conferences_uri(http_method(), cb_context:context()) -> cb_context:context().
validate_conferences_uri(?HTTP_GET, Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2);
validate_conferences_uri(?HTTP_PUT, Context) ->
    maybe_create_conference(Context).

-spec validate_conference_uri(http_method(), cb_context:context(), ne_binary()) -> cb_context:context().
validate_conference_uri(?HTTP_GET, Context, ConfId) ->
    AccountDb = cb_context:account_db(Context),
    {ok, Conference} = load_conference(AccountDb, ConfId),
    handle_response(Context, enrich_conference(Conference));
validate_conference_uri(?HTTP_POST, Context, ConfId) ->
    load_conference_into_context(Context, ConfId);
validate_conference_uri(?HTTP_PATCH, Context, ConfId) ->
    patch_conference(ConfId, Context);
validate_conference_uri(?HTTP_DELETE, Context, ConfId) ->
    load_conference_into_context(Context, ConfId).

-spec validate_participants_uri(http_method(), cb_context:context(), ne_binary()) -> cb_context:context().
validate_participants_uri(?HTTP_GET, Context, ConfId) ->
    AccountDb = cb_context:account_db(Context),
    AccountId = cb_context:account_id(Context),
    {ok, _Conference} = load_conference(AccountDb, ConfId), % check ownership
    handle_response(Context, load_enriched_participants(AccountId, ConfId));
validate_participants_uri(?HTTP_POST, Context, ConfId) ->
    load_conference_into_context(Context, ConfId).

-spec validate_participant_uri(http_method(), cb_context:context(), ne_binary(), ne_binary()) -> cb_context:context().
validate_participant_uri(?HTTP_POST, Context, ConfId, _ParticipantId) ->
    load_conference_into_context(Context, ConfId).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
-spec post(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
post(Context, ConfId) ->
    Action = cb_context:req_value(Context, <<"Action">>),
    handle_response(Context, handle_conference_action(ConfId, Action)).
post(Context, ConfId, <<"participants">>) ->
    AccountId = cb_context:account_id(Context),
    Action = cb_context:req_value(Context, <<"Action">>),
    handle_response(Context, handle_participants_action(AccountId, ConfId, Action)).
post(Context, ConfId, <<"participants">>, ParticipantId) ->
    Conference = kapps_conference:set_id(ConfId, kapps_conference:new()),
    Action = cb_context:req_value(Context, <<"Action">>),
    perform_conference_action(Conference, Action, erlang:binary_to_integer(ParticipantId)),
    handle_response(Context, {response, ok}).

-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, _) ->
    crossbar_doc:save(Context).

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(Context).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _) ->
    crossbar_doc:delete(Context).

%%%===================================================================
%%% REST API Helpers
%%%===================================================================

-spec load_conference_into_context(cb_context:context(), ne_binary()) -> cb_context:context().
load_conference_into_context(Context, ConfId) ->
    AccountDb = cb_context:account_db(Context),
    handle_response(Context, load_conference(AccountDb, ConfId)).

-spec handle_response(cb_context:context(), any()) -> cb_context:context().
handle_response(Context, {response, Response}) ->
    cb_context:setters(Context, [
                                 {fun cb_context:set_resp_status/2, 'success'}
                                ,{fun cb_context:set_resp_data/2, Response}
                                ]);
handle_response(Context, {ok, Response}) ->
    crossbar_doc:handle_json_success(Response, Context);
handle_response(Context, {error, Error}) ->
    cb_context:add_system_error(Error, Context).

%%%===================================================================
%%% REST API Actions
%%%===================================================================

-spec handle_participants_action(ne_binary(), ne_binary(), ne_binary()) -> {response, [integer()]}.
handle_participants_action(AccountId, ConfId, Action=?MUTE) ->
    handle_participants_action(AccountId, ConfId, Action,
                               fun(P) -> kz_json:is_false(<<"Is-Moderator">>, P) andalso kz_json:is_true(<<"Speak">>, P) end);
handle_participants_action(AccountId, ConfId, Action=?UNMUTE) ->
    handle_participants_action(AccountId, ConfId, Action,
                               fun(P) -> kz_json:is_false(<<"Is-Moderator">>, P) andalso kz_json:is_false(<<"Speak">>, P) end);
handle_participants_action(AccountId, ConfId, Action=?DEAF) ->
    handle_participants_action(AccountId, ConfId, Action,
                               fun(P) -> kz_json:is_false(<<"Is-Moderator">>, P) andalso kz_json:is_true(<<"Hear">>, P) end);
handle_participants_action(AccountId, ConfId, Action=?UNDEAF) ->
    handle_participants_action(AccountId, ConfId, Action,
                               fun(P) -> kz_json:is_false(<<"Is-Moderator">>, P) andalso kz_json:is_false(<<"Hear">>, P) end);
handle_participants_action(AccountId, ConfId, Action=?KICK) ->
    handle_participants_action(AccountId, ConfId, Action, fun(_) -> true end);
handle_participants_action(_AccountId, ConfId, Action) ->
    lager:error("Unhandled conference id:~p participants action:~p", [ConfId, Action]),
    {'error', 'unhandled_conference_participants_action'}.

-spec handle_conference_action(ne_binary(), ne_binary()) -> {'response', 'ok'} | {'error', 'unhandled_conference_action'}.
handle_conference_action(ConfId, <<"lock">>) ->
    lock_conference(ConfId),
    {response, ok};
handle_conference_action(ConfId, <<"unlock">>) ->
    unlock_conference(ConfId),
    {response, ok};
handle_conference_action(ConfId, Action) ->
    lager:error("Unhandled conference id:~p action:~p", [ConfId, Action]),
    {'error', 'unhandled_conference_action'}.

                                                % action applicable to conference participants selected by selector function
-spec handle_participants_action(ne_binary(), ne_binary(), ne_binary(), function()) -> {response, [integer()]}.
handle_participants_action(AccountId, ConfId, Action, Selector) ->
    {ok, JObjs} = request_conference_details(AccountId, ConfId),
    JObj = latest_details(JObjs),
    Participants = extract_participants(JObj),
    Conference = kapps_conference:set_id(ConfId, kapps_conference:new()),
    Commanded = [
                 perform_conference_action(Conference, Action, kz_json:get_value(<<"Participant-ID">>, P))
                 || P <- Participants, Selector(P)
                ],
    {response, Commanded}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec load_conference(ne_binary(), ne_binary()) -> kz_json:object().
load_conference(AccountDb, ConfId) ->
    load_typed_document(AccountDb, ConfId, <<"conference">>).

-spec load_participants(ne_binary(), ne_binary()) -> {ok, kz_json:objects()}.
load_participants(AccountId, ConfId) ->
    {ok, JObjs} = request_conference_details(AccountId, ConfId),
    JObj = latest_details(JObjs),
    {ok, extract_participants(JObj)}.

-spec load_enriched_participants(ne_binary(), ne_binary()) -> {ok, kz_json:objects()}.
load_enriched_participants(AccountId, ConfId) ->
    {ok, Participants} = load_participants(AccountId, ConfId),
    {ok, request_participants_details(Participants)}.

%%%===================================================================
%%% Utility functions
%%%===================================================================

-spec conf_id(kz_json:object()) -> ne_binary().
conf_id(Conf) -> kz_json:get_value(<<"id">>, Conf).
-spec owner_id(kz_json:object()) -> ne_binary().
owner_id(Conf) -> kz_json:get_value(<<"owner_id">>, Conf).
-spec run_time(kz_json:object()) -> integer().
run_time(Conf) -> kz_json:get_value(<<"Run-Time">>, Conf, 0).

-spec ensure_equal(any(), any(), atom()) -> ok.
ensure_equal(Type, Type, _) -> ok;
ensure_equal(_, _, Error) -> erlang:error(Error).

-spec load_typed_document(ne_binary(), ne_binary(), ne_binary()) -> {ok, kz_json:object()}.
load_typed_document(AccountDb, DocId, Type) ->
    {ok, JObj} = kz_datamgr:open_doc(AccountDb, DocId, [{'doc_type', Type}]),
    ensure_equal(kz_doc:type(JObj), Type, 'wrong_document_type'),
    {ok, JObj}.

%%%===================================================================
%%% Other functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new conference document with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create_conference(cb_context:context()) -> cb_context:context().
create_conference(Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"conferences">>, Context, OnSuccess).

-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    cb_context:set_doc(Context
                      ,kz_doc:set_type(cb_context:doc(Context), <<"conference">>)
                      );
on_successful_validation(DocId, Context) ->
    crossbar_doc:load_merge(DocId, Context, ?TYPE_CHECK_OPTION(<<"conference">>)).

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
                    cb_context:add_validation_error(
                      [<<"numbers">>]
                                                   ,<<"unique">>
                                                   ,kz_json:from_list([
                                                                       {<<"message">>, <<"Number already in use">>}
                                                                      ,{<<"cause">>, Number}
                                                                      ])
                                                   ,Context
                     )
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing conference document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update_conference(ne_binary(), cb_context:context()) -> cb_context:context().
update_conference(DocId, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(DocId, C) end,
    cb_context:validate_request_data(<<"conferences">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update-merge an existing conference document partially with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec patch_conference(ne_binary(), cb_context:context()) -> cb_context:context().
patch_conference(DocId, Context) ->
    crossbar_doc:patch_and_validate(DocId, Context, fun update_conference/2).

-spec lock_conference(ne_binary()) -> ok.
lock_conference(ConfId) ->
    Conference = kapps_conference:set_id(ConfId, kapps_conference:new()),
    kapps_conference_command:lock(Conference),
    ok.

-spec unlock_conference(ne_binary()) -> ok.
unlock_conference(ConfId) ->
    Conference = kapps_conference:set_id(ConfId, kapps_conference:new()),
    kapps_conference_command:unlock(Conference),
    ok.

-spec request_conference_details(kz_json:object()) -> kz_json:objects().
-spec request_conference_details(ne_binary(), ne_binary()) -> kz_json:objects().
request_conference_details(Conf) ->
    request_conference_details(owner_id(Conf), conf_id(Conf)).
request_conference_details(AccountId, ConfId) ->
    Realm = kz_util:get_account_realm(AccountId),
    Req = [{<<"Realm">>, Realm}
          ,{<<"Fields">>, []}
          ,{<<"Conference-ID">>, ConfId}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    kapps_util:amqp_pool_collect(Req, fun kapi_conference:publish_search_req/1, {'ecallmgr', 'true'}).

-spec request_participants_details(kz_json:objects()) -> kz_json:objects().
-spec request_participants_details(kz_json:objects(), kz_json:obejcts()) -> kz_json:objects().
request_participants_details(Participants) -> request_participants_details(Participants, []).
request_participants_details([], Acc) -> Acc;
request_participants_details([{H}|T], Acc) ->
    CallId = kz_json:get_value(<<"Call-ID">>, {H}),
    Req = [{<<"Call-ID">>, CallId}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case kapps_util:amqp_pool_request(Req
                                     ,fun kapi_call:publish_channel_status_req/1
                                     ,fun kapi_call:channel_status_resp_v/1
                                     ) of
        {'error', E} ->
            lager:debug("error fetching channel status for ~s (~p)", [CallId, E]),
            request_participants_details(T, Acc);
        {'ok', Resp} ->
            Participant = kz_json:set_values(filter_fields(H), Resp),
            request_participants_details(T, [Participant | Acc])
    end.

-spec extract_participants(kz_json:object()) -> kz_json:objects().
extract_participants(JObj) ->
    add_duration_to_participants(kz_json:get_value(<<"Participants">>, JObj, [])).

-spec calc_duration(kz_json:object()) -> integer().
calc_duration(Participant) ->
    Stamp = kz_util:current_tstamp(),
    JoinTime = props:get_value(<<"Join-Time">>, Participant),
    Stamp - JoinTime.

-spec add_duration_to_participants(kz_json:objects()) -> kz_json:objects().
add_duration_to_participants(Participants) ->
    [ {[{<<"Duration">>, calc_duration(P)} | P]} || {P} <- Participants ].

-spec latest_details(kz_json:obejcts()) -> kz_json:object().
latest_details(JObjs) ->
    [Latest|_] = lists:sort(fun(A, B) -> run_time(A) > run_time(B) end, JObjs),
    Latest.

-spec filter_fields(kz_json:object()) -> kz_json:object().
filter_fields(ParticipantInfo) ->
    [ {K, V} || {K, V} <- ParticipantInfo, lists:member(K, ?PARTICIPANT_INFO_FIELDS) ].

-spec perform_conference_action(kapps_conference:conference(), ne_binary(), api_integer()) -> integer().
perform_conference_action(Conference, ?MUTE, ParticipantId) ->
    kapps_conference_command:mute_participant(ParticipantId, Conference),
    kapps_conference_command:prompt(<<"conf-muted">>, ParticipantId, Conference),
    ParticipantId;
perform_conference_action(Conference, ?UNMUTE, ParticipantId) ->
    kapps_conference_command:unmute_participant(ParticipantId, Conference),
    kapps_conference_command:prompt(<<"conf-unmuted">>, ParticipantId, Conference),
    ParticipantId;
perform_conference_action(Conference, ?DEAF, ParticipantId) ->
    kapps_conference_command:deaf_participant(ParticipantId, Conference),
    kapps_conference_command:prompt(<<"conf-deaf">>, ParticipantId, Conference),
    ParticipantId;
perform_conference_action(Conference, ?UNDEAF, ParticipantId) ->
    kapps_conference_command:undeaf_participant(ParticipantId, Conference),
    kapps_conference_command:prompt(<<"conf-undeaf">>, ParticipantId, Conference),
    ParticipantId;
perform_conference_action(Conference, ?KICK, ParticipantId) ->
    kapps_conference_command:kick(ParticipantId, Conference),
    ParticipantId.

-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    Conf = kz_json:get_value(<<"value">>, JObj),
    [Conf | Acc].

-spec enrich_conference(kz_json:object()) -> kz_json:object().
enrich_conference(Conf) ->
    {ok, JObjs} = request_conference_details(Conf),
    JObj = latest_details(JObjs),
    Participants = extract_participants(JObj),
    Enriched = kz_json:insert_values([{<<"members">>, count_members(Participants)}
                                     ,{<<"admins">>, count_admins(Participants)}
                                     ,{<<"duration">>, run_time(JObj)}
                                     ,{<<"is_locked">>, kz_json:get_value(<<"Locked">>, JObj, false)}
                                     ], Conf),
    {ok, Enriched}.

-spec count_admins(kz_json:objects()) -> integer().
count_admins(Participants) ->
    erlang:length([ P || P <- Participants, kz_json:is_true(<<"Is-Moderator">>, P) ]).

-spec count_members(kz_json:objects()) -> integer().
count_members(Participants) ->
    erlang:length([ P || P <- Participants, kz_json:is_false(<<"Is-Moderator">>, P) ]).
