%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%% Conferences module
%%%
%%% Handle client requests for conference documents
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_conferences).

-export([init/0
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2
         ,resource_exists/0, resource_exists/1, resource_exists/2
         ,validate/1, validate/2, validate/3
         ,put/1
         ,post/2
         ,patch/2
         ,delete/2
        ]).

-include("crossbar.hrl").

-define(CB_LIST, <<"conferences/crossbar_listing">>).
-define(CB_LIST_BY_NUMBER, <<"conference/listing_by_number">>).

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
    crossbar_bindings:bind(<<"*.execute.delete.conferences">>, ?MODULE, 'delete').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].
allowed_methods(_, <<"details">>) ->
    [?HTTP_GET].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.
resource_exists(_, <<"details">>) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context) ->
    validate_conferences(Context, cb_context:req_verb(Context)).

validate_conferences(Context, ?HTTP_GET) ->
    load_conference_summary(Context);
validate_conferences(Context, ?HTTP_PUT) ->
    maybe_create_conference(Context).

validate(Context, Id) ->
    validate_conference(Context, Id, cb_context:req_verb(Context)).

validate_conference(Context, Id, ?HTTP_GET) ->
    load_conference(Id, Context);
validate_conference(Context, Id, ?HTTP_POST) ->
    case cb_context:req_value(Context, <<"action">>) of
        'undefined' -> update_conference(Id, Context);
        Action -> do_conference_action(Context, Id, Action, cb_context:req_value(Context, <<"participant">>))
    end;
validate_conference(Context, Id, ?HTTP_PATCH) ->
    patch_conference(Id, Context);
validate_conference(Context, Id, ?HTTP_DELETE) ->
    load_conference(Id, Context).

validate(Context, Id, <<"details">>) ->
    load_conference_details(Context, Id).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _) ->
    crossbar_doc:save(Context).

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
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of accounts, each summarized.  Or a specific
%% account summary.
%% @end
%%--------------------------------------------------------------------
-spec load_conference_summary(cb_context:context()) -> cb_context:context().
load_conference_summary(Context) ->
    case lists:nth(2, cb_context:req_nouns(Context)) of
        {<<"users">>, [UserId]} ->
            Filter = fun(J, A) ->
                             normalize_users_results(J, A, UserId)
                     end,
            crossbar_doc:load_view(?CB_LIST, [], Context, Filter);
        {?KZ_ACCOUNTS_DB, _} ->
            crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2);
        _ ->
            cb_context:add_system_error('faulty_request', Context)
    end.

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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_number_already_used(ne_binaries(), ne_binaries()) -> 'false' | {'true', ne_binary()}.
-spec is_number_already_used(ne_binaries(), ne_binaries(), 'false') -> 'false' | {'true', ne_binary()}.
is_number_already_used(Numbers, NewNumbers) ->
    is_number_already_used(Numbers, NewNumbers, 'false').

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
%% Create a new conference document with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create_conference(cb_context:context()) -> cb_context:context().
create_conference(Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"conferences">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a conference document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_conference(ne_binary(), cb_context:context()) -> cb_context:context().
load_conference(ConfId, Context) ->
    Realm = kz_util:get_account_realm(cb_context:account_id(Context)),
    Confs = get_conferences(Realm, ConfId),
    case participants_info(Confs) of
        {error, Error} -> cb_context:add_system_error(Error, Context);
        Participants ->
            ConfCtx = crossbar_doc:load(ConfId, Context, ?TYPE_CHECK_OPTION(<<"conference">>)),
            ConfInfo = kz_json:from_list([
                {<<"participants">>, Participants}
                , {<<"created">>, conference_starttime(Confs)}
                , {<<"duration">>, conference_runtime(Confs)}
            ]),
            cb_context:set_resp_data(ConfCtx, kz_json:set_value(<<"_read_only">>, ConfInfo, cb_context:resp_data(ConfCtx)))
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load details of the participants of a conference
%% @end
%%--------------------------------------------------------------------
-spec load_conference_details(cb_context:context(), path_token()) -> cb_context:context().
load_conference_details(Context, ConfId) ->
    Realm = kz_util:get_account_realm(cb_context:account_id(Context)),
    case participants_info(get_conferences(Realm, ConfId)) of
        {error, Error} -> cb_context:add_system_error(Error, Context);
        Participants ->
            crossbar_doc:handle_json_success(Participants, Context)
    end.

-spec get_conferences(ne_binary(), ne_binary()) -> kz_json:objects().
get_conferences(Realm, ConfId) ->
    Req = [{<<"Realm">>, Realm}
           ,{<<"Fields">>, []}
           ,{<<"Conference-ID">>, ConfId}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    kapps_util:amqp_pool_collect(Req, fun kapi_conference:publish_search_req/1, {'ecallmgr', 'true'}).

-spec conference_participants(kz_json:objects()) -> kz_json:objects().
conference_participants(JObjs) ->
    lists:flatten([ kz_json:get_value(<<"Participants">>, JObj, []) || JObj <- JObjs ]).

-spec conference_runtime({ok, kz_json:objects()}) -> integer().
conference_runtime({'ok', JObjs}) when is_list(JObjs) ->
    lists:min([kz_json:get_value(<<"Run-Time">>, JObj, 0) || JObj <- JObjs ]);
conference_runtime(_) -> 0.

-spec conference_starttime({ok, kz_json:objects()}) -> integer().
conference_starttime({'ok', JObjs}) when is_list(JObjs) ->
    lists:min([kz_json:get_value(<<"Start-Time">>, JObj, 0) || JObj <- JObjs ]);
conference_starttime(_) -> 0.

-spec participants_info(kz_json:objects()) -> cb_context:context().
-spec participants_info(kz_json:objects(), kz_json:objects()) -> cb_context:context().

participants_info({'error', []}) -> {'error', 'conference_not_active'};
participants_info({'error', _}) -> {'error', 'not_found'};
participants_info({'ok', JObjs}) -> participants_info(conference_participants(JObjs));

participants_info(Participants) ->
    participants_info(Participants, []).

participants_info([], Acc) -> Acc;
participants_info([{H}|T], Acc) ->
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
            participants_info(T, Acc);
        {'ok', Resp} ->
            Participant = kz_json:set_values(filter_fields(H), Resp),
            participants_info(T, [Participant | Acc])
    end.

-spec filter_fields(kz_json:object()) -> kz_json:object().
filter_fields(ParticipantInfo) ->
    [ {K, V} || {K, V} <- ParticipantInfo, lists:member(K, ?PARTICIPANT_INFO_FIELDS) ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Perform conference kick/mute/unmute via API
%% @end
%%--------------------------------------------------------------------
-spec do_conference_action(cb_context:context(), path_token(), kz_json:json_term(), kz_json:json_term()) ->
                             cb_context:context().
do_conference_action(Context, _, _, 'undefined') ->
    cb_context:add_system_error('participant_missing', Context);
do_conference_action(Context, Id, Action, ParticipantId) ->
    AuthDoc = cb_context:auth_doc(Context),
    ConfDoc = cb_context:doc(load_conference(Id, Context)),
    ConfOwnerId = kz_json:get_value(<<"owner_id">>, ConfDoc),
    AuthOwnerId = kz_json:get_value(<<"owner_id">>, AuthDoc),
    % Abort if the user is not room owner
    case ConfOwnerId of
        AuthOwnerId ->
            Conference = kapps_conference:set_id(Id, kapps_conference:new()),
            Resp = kz_json:set_value(<<"resp">>
                                     ,perform_conference_action(Conference, Action, ParticipantId)
                                     ,kz_json:new()
                                    ),
            crossbar_doc:handle_json_success(Resp, Context);
        _ -> cb_context:add_system_error('forbidden', Context)
    end.

-spec perform_conference_action(kapps_conference:conference(), ne_binary(), api_integer()) -> 'ok'.
perform_conference_action(Conference, <<"mute">>, ParticipantId) ->
    kapps_conference_command:mute_participant(ParticipantId, Conference),
    kapps_conference_command:prompt(<<"conf-muted">>, ParticipantId, Conference);
perform_conference_action(Conference, <<"unmute">>, ParticipantId) ->
    kapps_conference_command:unmute_participant(ParticipantId, Conference),
    kapps_conference_command:prompt(<<"conf-unmuted">>, ParticipantId, Conference);
perform_conference_action(Conference, <<"kick">>, ParticipantId) ->
    kapps_conference_command:kick(ParticipantId, Conference).

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

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
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
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    Conf = kz_json:get_value(<<"value">>, JObj),
    Stats = conference_stats(JObj, Conf),
    ExtConf = lists:foldr(fun({K,V}, ExtConf) -> kz_json:set_value(K, V, ExtConf) end, Conf, Stats),
    [ExtConf | Acc].

-spec conference_stats(kz_json:object(), kz_json:object()) -> kz_json:object().
conference_stats(JObj, Conf) ->
    ConfId = kz_json:get_value(<<"id">>, JObj),
    Realm = kz_util:get_account_realm(kz_json:get_value(<<"owner_id">>, Conf)),
    {ok, Confs} = get_conferences(Realm, ConfId),
    Participants = conference_participants(Confs),
    [
        {<<"members">>, count_members(Participants)}
        ,{<<"admins">>, count_admins(Participants)}
        ,{<<"duration">>, conference_runtime({ok, Confs})}
    ].

-spec count_admins(kz_json:objects()) -> integer().
count_admins(Participants) ->
    erlang:length([ P || P <- Participants, kz_json:is_true(<<"Is-Moderator">>, P) ]).

-spec count_members(kz_json:objects()) -> integer().
count_members(Participants) ->
    erlang:length([ P || P <- Participants, kz_json:is_false(<<"Is-Moderator">>, P) ]).

-spec normalize_users_results(kz_json:object(), kz_json:objects(), ne_binary()) ->
                                     api_objects().
normalize_users_results(JObj, Acc, UserId) ->
    case kz_json:get_value([<<"value">>, <<"owner_id">>], JObj) of
        'undefined' -> normalize_view_results(JObj, Acc);
        UserId -> normalize_view_results(JObj, Acc);
        _ -> Acc
    end.
