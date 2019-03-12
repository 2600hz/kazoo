%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_port_request).

-export([current_state/1
        ,public_fields/0
        ,public_fields/1
        ,get/1
        ,new/2
        ,account_active_ports/1
        ,descendant_active_ports/1
        ,account_has_active_port/1
        ,normalize_attachments/1
        ,normalize_numbers/1
        ,transition_to_complete/2
        ,attempt_transition/3
        ,compatibility_transition/2
        ,assign_to_app/3
        ,send_submitted_requests/0
        ,migrate/0
        ]).

-export([transition_metadata/2, transition_metadata/3]).
-export_type([transition_metadata/0]).

-compile({'no_auto_import', [get/1]}).

-include("knm.hrl").
-include_lib("kazoo_number_manager/include/knm_port_request.hrl").

-define(VIEW_LISTING_SUBMITTED, <<"port_requests/listing_submitted">>).
-define(ACTIVE_PORT_LISTING, <<"port_requests/active_port_request">>).
-define(DESCENDANT_ACTIVE_PORT_LISTING, <<"port_requests/listing_by_descendant_state">>).
-define(ACTIVE_PORT_IN_NUMBERS, <<"port_requests/port_in_numbers">>).

-type transition_response() :: {'ok', kz_json:object()} |
                               {'error', 'invalid_state_transition' | 'user_not_allowed' | kz_json:object()}.

-define(NAME_KEY, <<"name">>).
-define(NUMBERS_KEY, <<"numbers">>).
-define(USED_BY_KEY, <<"used_by">>).

-define(SHOULD_ALLOW_FROM_SUBMITTED
       ,kapps_config:get_is_true(?KNM_CONFIG_CAT
                                ,<<"allow_port_transition_from_submitted_to_scheduled">>
                                ,'false'
                                )).


%%% API

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec current_state(kz_json:object()) -> kz_term:api_binary().
current_state(JObj) ->
    kzd_port_requests:pvt_port_state(JObj, ?PORT_UNCONFIRMED).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec public_fields() -> kz_term:ne_binaries().
public_fields() ->
    [<<"id">>
    ,<<"created">>
    ,<<"updated">>
    ,<<"uploads">>
    ,<<"port_state">>
    ,<<"sent">>
    ,<<"_read_only">>
    ].

-spec public_fields(kz_json:object()) -> kz_json:object().
public_fields(JObj) ->
    As = kz_doc:attachments(JObj, kz_json:new()),
    ReadOnly = read_only_public_fields(JObj),
    kz_json:set_values([{<<"id">>, kz_doc:id(JObj)}
                       ,{<<"created">>, kz_doc:created(JObj)}
                       ,{<<"updated">>, kz_doc:modified(JObj)}
                       ,{<<"uploads">>, normalize_attachments(As)}
                       ,{<<"port_state">>, current_state(JObj)}
                       ,{<<"sent">>, kzd_port_requests:pvt_sent(JObj, 'false')}
                       ,{<<"_read_only">>, ReadOnly}
                       ]
                      ,kz_doc:public_fields(JObj)
                      ).

-spec read_only_public_fields(kz_json:object()) -> kz_term:api_object().
read_only_public_fields(Doc) ->
    JObj = kz_json:from_list(
             [{<<"account_name">>, kzd_port_requests:pvt_account_name(Doc)}
             ,{<<"port_authority">>, kzd_port_requests:pvt_port_authority(Doc)}
             ,{<<"port_authority_name">>, kzd_port_requests:pvt_port_authority_name(Doc)}
             ,{<<"ported_numbers">>, kzd_port_requests:pvt_ported_numbers(Doc)}
             ]
            ),
    case kz_json:is_empty(JObj) of
        'true' -> 'undefined';
        'false' -> JObj
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get(kz_term:ne_binary()) -> {'ok', kz_json:object()} |
                                  {'error', any()}.
-ifdef(TEST).
get(?TEST_NEW_PORT_NUM) -> {ok, ?TEST_NEW_PORT_REQ};
get(?NE_BINARY) -> {error, not_found}.
-else.
get(DID=?NE_BINARY) ->
    View = ?ACTIVE_PORT_IN_NUMBERS,
    ViewOptions = [{key, DID}, include_docs],
    case kz_datamgr:get_single_result(?KZ_PORT_REQUESTS_DB, View, ViewOptions) of
        {ok, Port} -> {ok, kz_json:get_value(<<"doc">>, Port)};
        {error, _E}=Error ->
            lager:debug("failed to query for port number '~s': ~p", [DID, _E]),
            Error
    end.
-endif.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec account_active_ports(kz_term:ne_binary()) -> {'ok', kz_json:objects()} |
                                                   {'error', 'not_found'}.
account_active_ports(AccountId) ->
    ViewOptions = [{'key', AccountId}
                  ,'include_docs'
                  ],
    case kz_datamgr:get_results(?KZ_PORT_REQUESTS_DB, ?ACTIVE_PORT_LISTING, ViewOptions) of
        {'ok', []} -> {'error', 'not_found'};
        {'ok', Ports} -> {'ok', [kz_json:get_value(<<"doc">>, Doc) || Doc <- Ports]};
        {'error', _R} ->
            lager:error("failed to query for account port numbers ~p", [_R]),
            {'error', 'not_found'}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec descendant_active_ports(kz_term:ne_binary()) -> {'ok', kz_json:objects()} |
                                                      {'error', 'not_found'}.
descendant_active_ports(AccountId) ->
    ViewOptions = [{'startkey', [AccountId]}
                  ,{'endkey', [AccountId, kz_json:new()]}
                  ,'include_docs'
                  ],
    case kz_datamgr:get_results(?KZ_PORT_REQUESTS_DB, ?DESCENDANT_ACTIVE_PORT_LISTING, ViewOptions) of
        {'ok', []} -> {'error', 'not_found'};
        {'ok', Ports} ->
            {'ok', [kz_json:get_value(<<"doc">>, Doc)
                    || Doc <- Ports
                           ,is_active_descendant_port(Doc)
                   ]
            };
        {'error', _R} ->
            lager:error("failed to query for descendant port numbers ~p", [_R]),
            {'error', 'not_found'}
    end.

-spec is_active_descendant_port(kz_json:object()) -> boolean().
is_active_descendant_port(JObj) ->
    Doc = kz_json:get_value(<<"doc">>, JObj),
    lists:member(kzd_port_requests:pvt_port_state(Doc), ?PORT_ACTIVE_STATES).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec account_has_active_port(kz_term:ne_binary()) -> boolean().
account_has_active_port(AccountId) ->
    case account_active_ports(AccountId) of
        {'ok', [_|_]} -> 'true';
        {'error', 'not_found'} -> 'false'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec normalize_attachments(kz_json:object()) -> kz_json:object().
normalize_attachments(Attachments) ->
    kz_json:map(fun normalize_attachments_map/2, Attachments).

-spec normalize_attachments_map(kz_json:path(), kz_json:json_term()) ->
                                       {kz_json:path(), kz_json:json_term()}.
normalize_attachments_map(K, V) ->
    {K, kz_json:delete_keys([<<"digest">>, <<"revpos">>, <<"stub">>], V)}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec normalize_numbers(kz_json:object()) -> kz_json:object().
normalize_numbers(PortReq) ->
    Numbers = kzd_port_requests:numbers(PortReq, kz_json:new()),
    Normalized = kz_json:map(fun normalize_number_map/2, Numbers),
    kzd_port_requests:set_numbers(PortReq, Normalized).

-spec normalize_number_map(kz_json:path(), kz_json:json_term()) ->
                                  {kz_json:path(), kz_json:json_term()}.
normalize_number_map(N, Meta) ->
    {knm_converters:normalize(N), Meta}.

-spec new(kz_json:object(), kz_term:proplist()) -> kz_json:object().
new(PortReq, Options) ->
    Normalized = normalize_numbers(PortReq),
    Metadata = transition_metadata(props:get_value('auth_by', Options)
                                  ,props:get_value('auth_user_id', Options)
                                  ),
    Unconf = [{?PORT_PVT_TYPE, ?TYPE_PORT_REQUEST}
             ,{?PORT_PVT_STATE, ?PORT_UNCONFIRMED}
             ,{?PORT_PVT_TRANSITIONS, [transition_metadata_jobj('undefined', ?PORT_UNCONFIRMED, Metadata)]}
             ,{<<"pvt_account_name">>, props:get_value('account_name', Options)} %% makes port listing sane in crossbar
             ,{<<"pvt_port_authority">>, props:get_value('port_authority_id', Options)}
             ,{<<"pvt_port_authority_name">>, props:get_value('port_authority_name', Options)}
             ],
    kz_json:set_values(Unconf, Normalized).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec transition_to_submitted(kz_json:object(), transition_metadata()) -> transition_response().
transition_to_submitted(JObj, Metadata) ->
    transition(JObj, Metadata, [?PORT_UNCONFIRMED, ?PORT_REJECTED], ?PORT_SUBMITTED).

-spec transition_to_pending(kz_json:object(), transition_metadata()) -> transition_response().
transition_to_pending(JObj, Metadata) ->
    transition(JObj, Metadata, [?PORT_SUBMITTED], ?PORT_PENDING).

-spec transition_to_scheduled(kz_json:object(), transition_metadata()) -> transition_response().
transition_to_scheduled(JObj, Metadata) ->
    ToScheduled = states_to_scheduled(?SHOULD_ALLOW_FROM_SUBMITTED),
    transition(JObj, Metadata, ToScheduled, ?PORT_SCHEDULED).

states_to_scheduled(_AllowFromSubmitted='false') ->
    [?PORT_PENDING];
states_to_scheduled(_AllowFromSubmitted='true') ->
    [?PORT_SUBMITTED | states_to_scheduled('false')].

-spec transition_to_complete(kz_json:object(), transition_metadata()) -> transition_response().
transition_to_complete(JObj, Metadata) ->
    case transition(JObj, Metadata, [?PORT_PENDING, ?PORT_SCHEDULED, ?PORT_REJECTED], ?PORT_COMPLETED) of
        {'error', _}=E -> E;
        {'ok', Transitioned} -> completed_port(Transitioned)
    end.

-spec transition_to_rejected(kz_json:object(), transition_metadata()) -> transition_response().
transition_to_rejected(JObj, Metadata) ->
    transition(JObj, Metadata, [?PORT_SUBMITTED, ?PORT_PENDING, ?PORT_SCHEDULED], ?PORT_REJECTED).

-spec transition_to_canceled(kz_json:object(), transition_metadata()) -> transition_response().
transition_to_canceled(JObj, Metadata) ->
    transition(JObj, Metadata, [?PORT_UNCONFIRMED, ?PORT_SUBMITTED, ?PORT_PENDING, ?PORT_SCHEDULED, ?PORT_REJECTED], ?PORT_CANCELED).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec attempt_transition(kz_json:object(), transition_metadata(), kz_term:ne_binary()) -> transition_response().
attempt_transition(PortReq, Metadata, ToState) ->
    PortAuthority = kzd_port_requests:find_port_authority(PortReq),
    case is_user_allowed_to_move_state(PortReq, Metadata, ToState, PortAuthority) of
        'true' -> maybe_transition(PortReq, Metadata, ToState);
        'false' ->
            {'error', 'user_not_allowed'}
    end.

-spec is_user_allowed_to_move_state(kz_json:object(), transition_metadata(), kz_term:ne_binary(), kz_term:api_ne_binary()) ->
                                           transition_response().
is_user_allowed_to_move_state(PortReq, #{}, _, 'undefined') ->
    lager:debug("port authority id is missing, disallowing state change for port ~s", [kz_doc:id(PortReq)]),
    'false';
is_user_allowed_to_move_state(PortReq, #{auth_account_id := undefined}, _, _) ->
    lager:debug("auth account is is missing, disallowing state change for port ~s", [kz_doc:id(PortReq)]),
    'false';
is_user_allowed_to_move_state(PortReq, #{auth_account_id := AuthAccountId}, ToState, PortAuthority)
  when ToState =:= ?PORT_UNCONFIRMED;
       ToState =:= ?PORT_SUBMITTED ->
    AuthAccountId =:= PortAuthority
        orelse kz_doc:account_id(PortReq) =:= AuthAccountId
        orelse kz_services_reseller:get_id(kz_doc:id(PortReq)) =:= AuthAccountId
        orelse kz_services_reseller:get_id('undefined') =:= AuthAccountId; %% checks if superduper
is_user_allowed_to_move_state(PortReq, #{auth_account_id := AuthAccountId}, ?PORT_CANCELED, PortAuthority) ->
    AuthAccountId =:= PortAuthority
        orelse (current_state(PortReq) =:= ?PORT_UNCONFIRMED
                andalso (kz_doc:account_id(PortReq) =:= AuthAccountId
                         orelse kz_services_reseller:get_id(kz_doc:id(PortReq)) =:= AuthAccountId
                        )
               )
        orelse kz_services_reseller:get_id('undefined') =:= AuthAccountId; %% checks if superduper
is_user_allowed_to_move_state(_, #{auth_account_id := AuthAccountId}, _, PortAuthority) ->
    AuthAccountId =:= PortAuthority
        orelse kz_services_reseller:get_id('undefined') =:= AuthAccountId. %% checks if superduper

-spec maybe_transition(kz_json:object(), transition_metadata(), kz_term:ne_binary()) -> transition_response().
maybe_transition(PortReq, Metadata, ?PORT_SUBMITTED) ->
    transition_to_submitted(PortReq, Metadata);
maybe_transition(PortReq, Metadata, ?PORT_PENDING) ->
    transition_to_pending(PortReq, Metadata);
maybe_transition(PortReq, Metadata, ?PORT_SCHEDULED) ->
    transition_to_scheduled(PortReq, Metadata);
maybe_transition(PortReq, Metadata, ?PORT_COMPLETED) ->
    transition_to_complete(PortReq, Metadata);
maybe_transition(PortReq, Metadata, ?PORT_REJECTED) ->
    transition_to_rejected(PortReq, Metadata);
maybe_transition(PortReq, Metadata, ?PORT_CANCELED) ->
    transition_to_canceled(PortReq, Metadata).

%%------------------------------------------------------------------------------
%% @doc Transition `port_in' number to complete. Thus compatible with old way of
%% `port_request' which the number doc is already created.
%% @end
%%------------------------------------------------------------------------------
-spec compatibility_transition(knm_number_options:extra_options(), transition_metadata()) -> 'ok' | {'error', any()}.
compatibility_transition(NumberProps, Metadata) ->
    Num = knm_number_options:number(NumberProps),
    AccountId = knm_number_options:account_id(NumberProps),
    completed_portin(Num, AccountId, Metadata).

-spec transition(kz_json:object(), transition_metadata(), kz_term:ne_binaries(), kz_term:ne_binary()) ->
                        transition_response().
transition(JObj, Metadata, FromStates, ToState) ->
    transition(JObj, Metadata, FromStates, ToState, current_state(JObj)).

-spec transition(kz_json:object(), transition_metadata(), kz_term:ne_binaries(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                        transition_response().
transition(_JObj, _Metadata, [], _ToState, _CurrentState) ->
    lager:debug("cant go from ~s to ~s", [_CurrentState, _ToState]),
    lager:debug("metadata: ~p", [_Metadata]),
    {'error', 'invalid_state_transition'};
transition(JObj, Metadata, [CurrentState | _], ToState, CurrentState) ->
    lager:debug("going from ~s to ~s", [CurrentState, ToState]),
    {ok, successful_transition(JObj, CurrentState, ToState, Metadata)};
transition(JObj, Metadata, [_FromState | FromStates], ToState, CurrentState) ->
    lager:debug("skipping from ~s to ~s c ~p", [_FromState, ToState, CurrentState]),
    transition(JObj, Metadata, FromStates, ToState, CurrentState).

-spec successful_transition(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary(), transition_metadata()) -> kz_json:object().
successful_transition(JObj, FromState, ToState, Metadata) ->
    MetadataJObj = transition_metadata_jobj(FromState, ToState, Metadata),
    NewTransitions = [MetadataJObj | kzd_port_requests:pvt_transitions(JObj, [])],
    Setters = [{fun kzd_port_requests:set_pvt_port_state/2, ToState}
              ,{fun kzd_port_requests:set_pvt_tranisitions/2, NewTransitions}
              ],
    kz_doc:setters(kz_json:delete_keys(public_fields(), JObj), Setters).

-spec transition_metadata_jobj(kz_term:api_ne_binary(), kz_term:ne_binary(), transition_metadata()) -> kz_json:object().
transition_metadata_jobj(FromState, ToState, #{auth_account_id := AuthAccountId
                                              ,auth_account_name := AuthAccountName
                                              ,optional_reason := OptionalReason
                                              }=Metadata) ->
    kz_json:from_list_recursive(
      [{?TRANSITION_TIMESTAMP, kz_time:now_s()}
      ,{?TRANSITION_TYPE, ?PORT_TRANSITION}
      ,{?TRANSITION_REASON, OptionalReason}
      ,{<<"transition">>, [{<<"new">>, ToState}
                          ,{<<"previous">>, FromState}
                          ]}
      ,{<<"authorization">>, [{<<"reason">>, OptionalReason}
                             ,{<<"account">>, [{<<"id">>, AuthAccountId}
                                              ,{<<"name">>, AuthAccountName}
                                              ]}
                              | maybe_user(Metadata)
                             ]}
      ]).

-spec maybe_user(transition_metadata()) -> kz_term:proplist().
maybe_user(#{auth_user_id := 'undefined'}) -> [];
maybe_user(#{auth_user_id := UserId
            ,user_first_name := OptionalFirstName
            ,user_last_name := OptionalLastName
            ,user_full_name := OptionalFullName
            }) ->
    [{<<"user">>, [{<<"id">>, UserId}
                  ,{<<"first_name">>, OptionalFirstName}
                  ,{<<"last_name">>, OptionalLastName}
                  ,{<<"full_name">>, OptionalFullName}
                  ]}
    ].

-type transition_metadata() :: #{auth_account_id => kz_term:ne_binary()
                                ,auth_account_name => kz_term:api_ne_binary()
                                ,auth_user_id => kz_term:api_ne_binary()
                                ,user_first_name => kz_term:api_ne_binary()
                                ,user_last_name => kz_term:api_ne_binary()
                                ,user_full_name => kz_term:api_ne_binary()
                                ,optional_reason => kz_term:api_ne_binary()
                                }.

-spec transition_metadata(kz_term:ne_binary(), kz_term:api_ne_binary()) -> transition_metadata().
transition_metadata(AuthAccountId, AuthUserId) ->
    transition_metadata(AuthAccountId, AuthUserId, undefined).

-spec transition_metadata(kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:api_ne_binary()) -> transition_metadata().
transition_metadata(?MATCH_ACCOUNT_RAW(AuthAccountId), UserId, Reason) ->
    OptionalUserId = case UserId of
                         ?NE_BINARY -> UserId;
                         _ -> 'undefined'
                     end,
    UserJObj = get_user_name(AuthAccountId, OptionalUserId),
    OptionalReason = case Reason of
                         ?NE_BINARY -> Reason;
                         _ -> 'undefined'
                     end,
    #{auth_account_id => AuthAccountId
     ,auth_account_name => kzd_accounts:fetch_name(AuthAccountId)
     ,auth_user_id => OptionalUserId
     ,user_first_name => kzd_users:first_name(UserJObj)
     ,user_last_name => kzd_users:last_name(UserJObj)
     ,user_full_name => kzd_users:full_name(UserJObj, kzd_users:username(UserJObj, kzd_users:email(UserJObj)))
     ,optional_reason => OptionalReason
     }.

-spec get_user_name(kz_term:ne_binary(), kz_term:api_ne_binary()) -> kz_json:object().
get_user_name(AuthAccountId, UserId) ->
    case kzd_users:fetch(AuthAccountId, UserId) of
        {ok, UserJObj} -> UserJObj;
        {error, _R} ->
            lager:warning("cannot read ~s's username: ~p", [UserId, _R]),
            kz_json:new()
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec assign_to_app(kz_term:ne_binary(), kz_term:api_ne_binary(), kz_json:object()) ->
                           {'ok', kz_json:object()} |
                           {'error', any()}.
assign_to_app(Number, NewApp, JObj) ->
    case kz_json:get_value([?NUMBERS_KEY, Number, ?USED_BY_KEY], JObj) of
        NewApp -> {'ok', JObj};
        _OldApp ->
            lager:debug("assigning number ~s in port request ~s to ~s"
                       ,[Number, kz_doc:id(JObj), NewApp]),
            NumberJObj = kz_json:from_list(
                           props:filter_empty(
                             [{?USED_BY_KEY, NewApp}
                              | kz_json:to_proplist(
                                  kz_json:get_value([?NUMBERS_KEY, Number], JObj)
                                 )
                             ])
                          ),
            save_doc(kz_json:set_value([?NUMBERS_KEY, Number], NumberJObj, JObj))
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec send_submitted_requests() -> 'ok'.
send_submitted_requests() ->
    View = ?VIEW_LISTING_SUBMITTED,
    case kz_datamgr:get_results(?KZ_PORT_REQUESTS_DB, View, [include_docs]) of
        {'error', _R} ->
            lager:error("failed to open view ~s ~p", [View, _R]);
        {'ok', []} -> 'ok';
        {'ok', JObjs} ->
            lists:foreach(fun send_submitted_request/1, JObjs),
            lager:debug("requests sent")
    end.

send_submitted_request(JObj) ->
    maybe_send_request(kz_json:get_value(<<"doc">>, JObj)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec migrate() -> 'ok'.
migrate() ->
    kz_util:put_callid(<<"port_request_migration">>),
    ?SUP_LOG_DEBUG("migrating port request documents, if necessary"),
    migrate(undefined, 50),
    ?SUP_LOG_DEBUG("finished migrating port request documents").

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec completed_port(kz_json:object()) -> transition_response().
completed_port(PortReq) ->
    lager:debug("transitioning numbers to active"),
    transition_numbers(PortReq).

-spec completed_portin(kz_term:ne_binary(), kz_term:ne_binary(), transition_metadata()) -> 'ok' | {'error', any()}.
completed_portin(Num, AccountId, #{optional_reason := OptionalReason}) ->
    Options = [{auth_by, ?KNM_DEFAULT_AUTH_BY}
              ,{assign_to, AccountId}
              ],
    Routins = [{fun knm_phone_number:set_state/2, ?NUMBER_STATE_IN_SERVICE}
              ,{fun knm_phone_number:set_ported_in/2, 'true'}
              ,{fun knm_phone_number:update_doc/2, kz_json:from_list([{<<"portin_reason">>, OptionalReason}])}
              ],

    lager:debug("transitioning legacy port_in number ~s to in_service", [Num]),
    case knm_number:update(Num, Routins, Options) of
        {ok, _} ->
            lager:debug("number ~s ported successfully", [Num]);
        {error, _Reason} ->
            lager:debug("failed to transition number ~s: ~p", [Num, _Reason]),
            {error, <<"transition_failed">>}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec transition_numbers(kz_json:object()) -> transition_response().
transition_numbers(PortReq) ->
    PortReqId = kz_doc:id(PortReq),
    AccountId = kz_doc:account_id(PortReq),
    Options = [{auth_by, ?KNM_DEFAULT_AUTH_BY}
              ,{assign_to, AccountId}
              ,{dry_run, false}
              ,{ported_in, true}
              ,{public_fields, kz_json:from_list([{<<"port_id">>, PortReqId}])}
              ],
    lager:debug("creating local numbers for port ~s", [PortReqId]),
    Numbers = kz_json:get_keys(kzd_port_requests:numbers(PortReq)),
    case knm_numbers:create(Numbers, Options) of
        #{ko := KOs} when map_size(KOs) =:= 0 ->
            lager:debug("all numbers ported, removing from port request"),
            clear_numbers_from_port(PortReq);
        #{ko := KOs, ok := _OKs} ->
            NumsKO = maps:keys(KOs),
            case numbers_not_in_account_nor_in_service(AccountId, NumsKO) of
                [] ->
                    lager:debug("these were already assigned and in service: ~p", [NumsKO]),
                    clear_numbers_from_port(PortReq);
                _NumsNotTransitioned ->
                    lager:debug("failed to transition ~p/~p numbers"
                               ,[length(_NumsNotTransitioned), length(_OKs)]),
                    {error, PortReq}
            end
    end.

numbers_not_in_account_nor_in_service(AccountId, Nums) ->
    #{ko := KOs, ok := Ns} = knm_numbers:get(Nums),
    [knm_phone_number:number(PN)
     || N <- Ns,
        PN <- [knm_number:phone_number(N)],
        not is_in_account_and_in_service(AccountId, PN)
    ]
        ++ maps:keys(KOs).

is_in_account_and_in_service(AccountId, PN) ->
    AccountId =:= knm_phone_number:assigned_to(PN)
        andalso ?NUMBER_STATE_IN_SERVICE =:= knm_phone_number:state(PN).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec clear_numbers_from_port(kz_json:object()) -> {ok, kz_json:object()}.
clear_numbers_from_port(PortReq) ->
    Setters = [{fun kzd_port_requests:set_numbers/2, kz_json:new()}
              ,{fun kzd_port_requests:set_pvt_ported_numbers/2, kzd_port_requests:numbers(PortReq)}
              ],
    Cleared = kz_doc:setters(PortReq, Setters),
    case save_doc(Cleared) of
        {'ok', _PortReq1}=Ok ->
            lager:debug("port numbers cleared"),
            Ok;
        {'error', 'conflict'} ->
            lager:error("port request doc was updated before we could re-save"),
            {ok, PortReq};
        {'error', _E} ->
            lager:debug("failed to clear numbers: ~p", [_E]),
            {ok, PortReq}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec maybe_send_request(kz_json:object()) -> 'ok'.
maybe_send_request(JObj) ->
    kz_util:put_callid(kz_doc:id(JObj)),
    AccountId = kz_doc:account_id(JObj),
    case kzd_accounts:fetch(AccountId) of
        {'ok', AccountDoc} ->
            Url = kz_json:get_value(<<"submitted_port_requests_url">>, AccountDoc),
            maybe_send_request(JObj, Url);
        {'error', _R} ->
            lager:error("failed to open account ~s:~p", [AccountId, _R])
    end.

-spec maybe_send_request(kz_json:object(), kz_term:api_binary()) -> 'ok'.
maybe_send_request(JObj, 'undefined') ->
    lager:debug("'submitted_port_requests_url' is not set for account ~s", [kz_doc:account_id(JObj)]);
maybe_send_request(JObj, Url)->
    case send_request(JObj, Url) of
        'error' -> 'ok';
        'ok' ->
            case send_attachements(Url, JObj) of
                'error' -> 'ok';
                'ok' -> set_flag(JObj)
            end
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec send_request(kz_json:object(), kz_term:ne_binary()) -> 'error' | 'ok'.
send_request(JObj, Url) ->
    Headers = [{"Content-Type", "application/json"}
              ,{"User-Agent", kz_term:to_list(node())}
              ],
    Uri = kz_term:to_list(<<Url/binary, "/", (kz_doc:id(JObj))/binary>>),
    Remove = [?PORT_PVT_REV
             ,<<"ui_metadata">>
             ,<<"_attachments">>
             ,<<"pvt_request_id">>
             ,?PORT_PVT_TYPE
             ,?PORT_PVT_VSN
             ,?PORT_PVT_ACCOUNT_DB
             ],
    Replace = [{?PORT_PVT_ID, <<"id">>}
              ,{?PORT_PVT_STATE, <<"port_state">>}
              ,{?PORT_PVT_ACCOUNT_ID, <<"account_id">>}
              ,{?PORT_PVT_CREATED, <<"created">>}
              ,{?PORT_PVT_MODIFIED, <<"modified">>}
              ],
    Data = kz_json:encode(kz_json:normalize_jobj(JObj, Remove, Replace)),
    case kz_http:post(Uri, Headers, Data) of
        {'ok', 200, _Headers, _Resp} ->
            lager:debug("submitted_port_request successfully sent");
        _Other ->
            lager:error("failed to send submitted_port_request ~p", [_Other]),
            'error'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec send_attachements(kz_term:ne_binary(), kz_json:object()) -> 'error' | 'ok'.
send_attachements(Url, JObj) ->
    try fetch_and_send(Url, JObj) of
        'ok' -> 'ok'
    catch
        'throw':'error' -> 'error'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fetch_and_send(kz_term:ne_binary(), kz_json:object()) -> 'ok'.
fetch_and_send(Url, JObj) ->
    Id = kz_doc:id(JObj),
    Attachments = kz_doc:attachments(JObj, kz_json:new()),
    F = fun(Key, Value, 'ok') ->
                case kz_datamgr:fetch_attachment(?KZ_PORT_REQUESTS_DB, Id, Key) of
                    {'error', _R} ->
                        lager:error("failed to fetch attachment ~s : ~p", [Key, _R]),
                        throw('error');
                    {'ok', Attachment} ->
                        send_attachment(Url, Id, Key, Value, Attachment)
                end
        end,
    kz_json:foldl(F, 'ok', Attachments).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec send_attachment(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), binary()) ->
                             'error' | 'ok'.
send_attachment(Url, Id, Name, Options, Attachment) ->
    ContentType = kz_json:get_value(<<"content_type">>, Options),
    Headers = [{"Content-Type", kz_term:to_list(ContentType)}
              ,{"User-Agent", kz_term:to_list(node())}
              ],
    Uri = kz_term:to_list(<<Url/binary, "/", Id/binary, "/", Name/binary>>),
    case kz_http:post(Uri, Headers, Attachment) of
        {'ok', 200, _Headers, _Resp} ->
            lager:debug("attachment ~s for submitted_port_request successfully sent", [Name]);
        _Other ->
            lager:error("failed to send attachment ~s for submitted_port_request ~p", [Name, _Other]),
            'error'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_flag(kz_json:object()) -> 'ok'.
set_flag(JObj) ->
    Doc = kzd_port_requests:set_pvt_sent(JObj, 'true'),
    case save_doc(Doc) of
        {'ok', _} -> lager:debug("flag for submitted_port_request successfully set");
        {'error', _R} ->
            lager:debug("failed to set flag for submitted_port_request: ~p", [_R])
    end.


-spec migrate(any(), pos_integer()) -> 'ok'.
migrate(StartKey, Limit) ->
    ?SUP_LOG_DEBUG(":: getting next batch of ~b port requests starting with key ~1000p"
                  ,[Limit, StartKey]
                  ),
    {'ok', Docs} = fetch_docs(StartKey, Limit),
    try lists:split(Limit, Docs) of
        {Results, []} ->
            Migrated = migrate_docs(Results),
            ?SUP_LOG_DEBUG("-- migrated ~b port requests", [Migrated]);
        {Results, [NextResult]} ->
            Migrated = migrate_docs(Results),
            ?SUP_LOG_DEBUG("-- migrated ~b port requests", [Migrated]),
            timer:sleep(5000),
            migrate(kz_json:get_value(<<"key">>, NextResult), Limit)
    catch
        'error':'badarg' ->
            Migrated = migrate_docs(Docs),
            ?SUP_LOG_DEBUG("-- migrated ~b port requests", [Migrated])
    end.

-spec migrate_docs(kz_json:objects()) -> integer().
migrate_docs([]) -> 0;
migrate_docs(Docs) ->
    case [UpdatedDoc || Doc <- Docs,
                        {UpdatedDoc, 'true'} <- [migrate_doc(kz_json:get_value(<<"doc">>, Doc))]
         ]
    of
        [] -> 0;
        UpdatedDocs ->
            {'ok', _} = kz_datamgr:save_docs(?KZ_PORT_REQUESTS_DB, UpdatedDocs),
            length(UpdatedDocs)
    end.

-spec migrate_doc(kz_json:object()) -> {kz_json:object(), boolean()}.
migrate_doc(PortRequest) ->
    MigrateFuns = [fun add_pvt_tree/1
                  ,fun add_pvt_port_authority/1
                  ,fun add_pvt_account_name/1
                  ,fun rename_superduper_comment/1
                  ],
    lists:foldl(fun migrate_doc/2, {PortRequest, 'false'}, MigrateFuns).

-spec migrate_doc(fun((kz_json:object()) -> kz_term:api_object()), {kz_json:object(), boolean()}) ->
                         {kz_json:object(), boolean()}.
migrate_doc(Fun, {Doc, IsUpdated}) ->
    case Fun(Doc) of
        'undefined' -> {Doc, IsUpdated};
        NewDoc -> {NewDoc, 'true'}
    end.

-spec add_pvt_tree(kz_json:object()) -> kz_term:api_object().
add_pvt_tree(PortRequest) ->
    AccountId = kz_doc:account_id(PortRequest),
    case kzd_accounts:tree(PortRequest, 'undefined') of
        'undefined' ->
            {'ok', AccountDoc} = kzd_accounts:fetch(AccountId),
            kzd_accounts:set_tree(PortRequest, kzd_accounts:tree(AccountDoc));
        _Tree -> 'undefined'
    end.

-spec add_pvt_port_authority(kz_json:object()) -> kz_term:api_object().
add_pvt_port_authority(PortRequest) ->
    AccountId = kz_doc:account_id(PortRequest),
    case kzd_port_requests:pvt_port_authority(PortRequest) == 'undefined'
        orelse kzd_port_requests:pvt_port_authority_name(PortRequest) == 'undefined'
    of
        'true' ->
            case kzd_port_requests:find_port_authority(AccountId) of
                'undefined' -> 'undefined';
                AuthorityId ->
                    Setters = [{fun kzd_port_requests:set_pvt_port_authority/2, AuthorityId}
                              ,{fun kzd_port_requests:set_pvt_port_authority_name/2
                               ,kzd_accounts:fetch_name(AuthorityId)
                               }
                              ],
                    kz_doc:setters(PortRequest, Setters)
            end;
        'false' ->
            'undefined'
    end.

-spec add_pvt_account_name(kz_json:object()) -> kz_term:api_object().
add_pvt_account_name(PortRequest) ->
    AccountId = kz_doc:account_id(PortRequest),
    case kzd_port_requests:pvt_account_name(PortRequest) of
        'undefined' ->
            kzd_port_requests:set_pvt_account_name(PortRequest, kzd_accounts:fetch_name(AccountId));
        _Tree -> 'undefined'
    end.

-spec rename_superduper_comment(kz_json:object()) -> kz_term:api_object().
rename_superduper_comment(PortRequest) ->
    Comments = kzd_port_requests:comments(PortRequest, []),
    case lists:foldl(fun rename_to_is_private/2, {[], 'false'}, Comments) of
        {_, 'false'} -> 'undefined';
        {Updated, 'true'} ->
            kzd_port_requests:set_comments(PortRequest, lists:reverse(Updated))
    end.

rename_to_is_private(Comment, {Acc, IsUpdated}) ->
    case kz_json:get_value(<<"superduper_comment">>, Comment) of
        'undefined' -> {[Comment | Acc], IsUpdated};
        Boolean ->
            {[kzd_comment:set_is_private(kz_json:delete_key(<<"superduper_comment">>, Comment)
                                        ,Boolean
                                        )
              | Acc
             ]
            ,'true'
            }
    end.

-spec fetch_docs(any(), pos_integer()) -> {'ok', kz_json:objects()}.
fetch_docs(StartKey, Limit) ->
    ViewOptions = props:filter_undefined(
                    [{'startkey', StartKey}
                    ,{'limit', Limit + 1}
                    ,'include_docs'
                    ]
                   ),
    kz_datamgr:get_results(?KZ_PORT_REQUESTS_DB, <<"port_requests/listing_by_state">>,  ViewOptions).

-spec save_doc(kz_json:object()) -> {'ok', kz_json:object()} |
                                    {'error', any()}.
save_doc(JObj) ->
    kz_datamgr:save_doc(?KZ_PORT_REQUESTS_DB, JObj).
