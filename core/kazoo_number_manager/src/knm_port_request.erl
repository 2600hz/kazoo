%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_port_request).

-export([init/0
        ,current_state/1
        ,public_fields/1
        ,get/1
        ,new/3
        ,account_active_ports/1
        ,account_has_active_port/1
        ,normalize_attachments/1
        ,normalize_numbers/1
        ,transition_to_complete/2
        ,maybe_transition/3
        ,compatibility_transition/2
        ,charge_for_port/1, charge_for_port/2
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
-define(ACTIVE_PORT_IN_NUMBERS, <<"port_requests/port_in_numbers">>).

-type transition_response() :: {'ok', kz_json:object()} |
                               {'error', 'invalid_state_transition' | kz_json:object()}.

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
-spec init() -> any().
init() ->
    _ = kz_datamgr:db_create(?KZ_PORT_REQUESTS_DB),
    kz_datamgr:revise_doc_from_file(?KZ_PORT_REQUESTS_DB, 'crossbar', <<"views/port_requests.json">>).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec current_state(kz_json:object()) -> kz_term:api_binary().
current_state(JObj) ->
    kz_json:get_value(?PORT_PVT_STATE, JObj, ?PORT_UNCONFIRMED).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec public_fields(kz_json:object()) -> kz_json:object().
public_fields(JObj) ->
    As = kz_doc:attachments(JObj, kz_json:new()),
    kz_json:set_values([{<<"id">>, kz_doc:id(JObj)}
                       ,{<<"created">>, kz_doc:created(JObj)}
                       ,{<<"updated">>, kz_doc:modified(JObj)}
                       ,{<<"uploads">>, normalize_attachments(As)}
                       ,{<<"port_state">>, kz_json:get_ne_binary_value(?PORT_PVT_STATE, JObj, ?PORT_UNCONFIRMED)}
                       ,{<<"sent">>, kz_json:is_true(?PORT_PVT_SENT, JObj)}
                       ]
                      ,kz_doc:public_fields(JObj)
                      ).

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
    Numbers = kz_json:get_value(?NUMBERS_KEY, PortReq, kz_json:new()),
    Normalized = kz_json:map(fun normalize_number_map/2, Numbers),
    kz_json:set_value(?NUMBERS_KEY, Normalized, PortReq).

-spec normalize_number_map(kz_json:path(), kz_json:json_term()) ->
                                  {kz_json:path(), kz_json:json_term()}.
normalize_number_map(N, Meta) ->
    {knm_converters:normalize(N), Meta}.

-spec new(kz_json:object(), kz_term:ne_binary(), kz_term:api_ne_binary()) -> kz_json:object().
new(PortReq, ?MATCH_ACCOUNT_RAW(AuthAccountId), AuthUserId) ->
    Normalized = normalize_numbers(PortReq),
    Metadata = transition_metadata(AuthAccountId, AuthUserId),
    Unconf = [{?PORT_PVT_TYPE, ?TYPE_PORT_REQUEST}
             ,{?PORT_PVT_STATE, ?PORT_UNCONFIRMED}
             ,{?PORT_PVT_TRANSITIONS, [transition_metadata_jobj(undefined, ?PORT_UNCONFIRMED, Metadata)]}
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
    NewTransitions = [MetadataJObj | kz_json:get_list_value(?PORT_PVT_TRANSITIONS, JObj, [])],
    Values = [{?PORT_PVT_STATE, ToState}
             ,{?PORT_PVT_TRANSITIONS, NewTransitions}
             ],
    kz_json:set_values(Values, JObj).

-spec transition_metadata_jobj(kz_term:api_ne_binary(), kz_term:ne_binary(), transition_metadata()) -> kz_json:object().
transition_metadata_jobj(FromState, ToState, #{auth_account_id := AuthAccountId
                                              ,auth_account_name := AuthAccountName
                                              ,auth_user_id := OptionalUserId
                                              ,user_first_name := OptionalFirstName
                                              ,user_last_name := OptionalLastName
                                              ,optional_reason := OptionalReason
                                              }) ->
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
                              | maybe_user(OptionalUserId, OptionalFirstName, OptionalLastName)
                             ]}
      ]).

-spec maybe_user(kz_term:api_ne_binary(), kz_term:api_ne_binary(), kz_term:api_ne_binary()) -> kz_term:proplist().
maybe_user(undefined, _, _) -> [];
maybe_user(UserId, OptionalFirstName, OptionalLastName) ->
    [{<<"user">>, [{<<"id">>, UserId}
                  ,{<<"first_name">>, OptionalFirstName}
                  ,{<<"last_name">>, OptionalLastName}
                  ]}
    ].

-type transition_metadata() :: #{auth_account_id => kz_term:ne_binary()
                                ,auth_account_name => kz_term:api_ne_binary()
                                ,auth_user_id => kz_term:api_ne_binary()
                                ,user_first_name => kz_term:api_ne_binary()
                                ,user_last_name => kz_term:api_ne_binary()
                                ,optional_reason => kz_term:api_ne_binary()
                                }.

-spec transition_metadata(kz_term:ne_binary(), kz_term:api_ne_binary()) -> transition_metadata().
transition_metadata(AuthAccountId, AuthUserId) ->
    transition_metadata(AuthAccountId, AuthUserId, undefined).

-spec transition_metadata(kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:api_ne_binary()) -> transition_metadata().
transition_metadata(?MATCH_ACCOUNT_RAW(AuthAccountId), UserId, Reason) ->
    OptionalUserId = case UserId of
                         ?NE_BINARY -> UserId;
                         _ -> undefined
                     end,
    {FirstName, LastName} = get_user_name(AuthAccountId, OptionalUserId),
    OptionalReason = case Reason of
                         ?NE_BINARY -> Reason;
                         _ -> undefined
                     end,
    #{auth_account_id => AuthAccountId
     ,auth_account_name => kzd_accounts:fetch_name(AuthAccountId)
     ,auth_user_id => OptionalUserId
     ,user_first_name => FirstName
     ,user_last_name => LastName
     ,optional_reason => OptionalReason
     }.

get_user_name(AuthAccountId, UserId) ->
    case kzd_user:fetch(AuthAccountId, UserId) of
        {ok, UserJObj} -> {kzd_user:first_name(UserJObj), kzd_user:last_name(UserJObj)};
        {error, _R} ->
            lager:warning("cannot read ~s's username: ~p", [UserId, _R]),
            {undefined, undefined}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec charge_for_port(kz_json:object()) -> 'ok' | 'error'.
charge_for_port(JObj) ->
    charge_for_port(JObj, kz_doc:account_id(JObj)).

-spec charge_for_port(kz_json:object(), kz_term:ne_binary()) -> 'ok' | 'error'.
charge_for_port(_JObj, AccountId) ->
    Services = kz_services:fetch(AccountId),
    Cost = kz_services:activation_charges(<<"number_services">>, ?FEATURE_PORT, Services),
    Transaction = kz_transaction:debit(AccountId, wht_util:dollars_to_units(Cost)),
    kz_services:commit_transactions(Services, [Transaction]).

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
    lager:debug("migrating port request documents, if necessary"),
    migrate(<<>>, 10),
    lager:debug("finished migrating port request documents").

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec completed_port(kz_json:object()) -> transition_response().
completed_port(PortReq) ->
    case charge_for_port(PortReq) of
        'error' -> throw({'error', 'failed_to_charge'});
        'ok' ->
            lager:debug("successfully charged for port, transitioning numbers to active"),
            transition_numbers(PortReq)
    end.

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
    AccountId = kz_json:get_value(?PORT_PVT_ACCOUNT_ID, PortReq),
    Options = [{auth_by, ?KNM_DEFAULT_AUTH_BY}
              ,{assign_to, AccountId}
              ,{dry_run, false}
              ,{ported_in, true}
              ,{public_fields, kz_json:from_list([{<<"port_id">>, PortReqId}])}
              ],
    lager:debug("creating local numbers for port ~s", [PortReqId]),
    Numbers = kz_json:get_keys(?NUMBERS_KEY, PortReq),
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
    Cleared = kz_json:set_value(?NUMBERS_KEY, kz_json:new(), PortReq),
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
maybe_send_request(JObj, 'undefined')->
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
    Doc = kz_json:set_value(?PORT_PVT_SENT, 'true', JObj),
    case save_doc(Doc) of
        {'ok', _} -> lager:debug("flag for submitted_port_request successfully set");
        {'error', _R} ->
            lager:debug("failed to set flag for submitted_port_request: ~p", [_R])
    end.


-spec migrate(binary(), pos_integer()) -> 'ok'.
migrate(StartKey, Limit) ->
    {'ok', Docs} = fetch_docs(StartKey, Limit),
    try lists:split(Limit, Docs) of
        {Results, []} ->
            migrate_docs(Results);
        {Results, [NextResult]} ->
            migrate_docs(Results),
            lager:debug("migrated batch of ~p port requests", [Limit]),
            timer:sleep(5000),
            migrate(kz_json:get_value(<<"key">>, NextResult), Limit)
    catch
        'error':'badarg' ->
            migrate_docs(Docs)
    end.

-spec migrate_docs(kz_json:objects()) -> 'ok'.
migrate_docs([]) -> 'ok';
migrate_docs(Docs) ->
    case prepare_docs_for_migrate(Docs) of
        [] -> 'ok';
        UpdatedDocs ->
            {'ok', _} = kz_datamgr:save_docs(?KZ_PORT_REQUESTS_DB, UpdatedDocs),
            'ok'
    end.

-spec prepare_docs_for_migrate(kz_json:objects()) -> kz_json:objects().
prepare_docs_for_migrate(Docs) ->
    [UpdatedDoc || Doc <- Docs,
                   (UpdatedDoc = migrate_doc(kz_json:get_value(<<"doc">>, Doc)))
                       =/= 'undefined'
    ].

-spec migrate_doc(kz_json:object()) -> kz_term:api_object().
migrate_doc(PortRequest) ->
    case kz_json:get_value(?PORT_PVT_TREE, PortRequest) of
        'undefined' -> update_doc(PortRequest);
        _Tree -> 'undefined'
    end.

-spec update_doc(kz_json:object()) -> kz_term:api_object().
update_doc(PortRequest) ->
    update_doc(PortRequest, kz_doc:account_id(PortRequest)).

-spec update_doc(kz_json:object(), kz_term:api_binary()) -> kz_term:api_object().
update_doc(_Doc, 'undefined') ->
    lager:debug("no account id in doc ~s", [kz_doc:id(_Doc)]),
    'undefined';
update_doc(PortRequest, AccountId) ->
    {'ok', AccountDoc} = kzd_accounts:fetch(AccountId),
    kz_json:set_value(?PORT_PVT_TREE, kzd_accounts:tree(AccountDoc), PortRequest).

-spec fetch_docs(binary(), pos_integer()) -> {'ok', kz_json:objects()}.
fetch_docs(StartKey, Limit) ->
    ViewOptions = [{'startkey', StartKey}
                  ,{'limit', Limit + 1}
                  ,'include_docs'
                  ],
    kz_datamgr:all_docs(?KZ_PORT_REQUESTS_DB, ViewOptions).

-spec save_doc(kz_json:object()) -> {'ok', kz_json:object()} |
                                    {'error', any()}.
save_doc(JObj) ->
    kz_datamgr:save_doc(?KZ_PORT_REQUESTS_DB, JObj).
