%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors:
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(knm_port_request).

-export([init/0
        ,current_state/1
        ,public_fields/1
        ,get/1
        ,account_active_ports/1
        ,account_has_active_port/1
        ,normalize_attachments/1
        ,normalize_numbers/1
        ,transition_to_complete/1
        ,maybe_transition/2
        ,charge_for_port/1, charge_for_port/2
        ,assign_to_app/3
        ,send_submitted_requests/0
        ,migrate/0
        ]).

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
-define(PVT_ACCOUNT_DB, <<"pvt_account_db">>).
-define(PVT_ACCOUNT_ID, <<"pvt_account_id">>).
-define(PVT_ID, <<"_id">>).
-define(PVT_REV, <<"_rev">>).
-ifndef(PVT_TREE).
- define(PVT_TREE, <<"pvt_tree">>).
-endif.
-define(PVT_VSN, <<"pvt_vsn">>).

%%% API

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec init() -> any().
init() ->
    _ = kz_datamgr:db_create(?KZ_PORT_REQUESTS_DB),
    kz_datamgr:revise_doc_from_file(?KZ_PORT_REQUESTS_DB, 'crossbar', <<"views/port_requests.json">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec current_state(kz_json:object()) -> api_binary().
current_state(JObj) ->
    kz_json:get_value(?PORT_PVT_STATE, JObj, ?PORT_UNCONFIRMED).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec public_fields(kz_json:object()) -> kz_json:object().
public_fields(JObj) ->
    As = kz_doc:attachments(JObj, kz_json:new()),

    kz_json:set_values([{<<"id">>, kz_doc:id(JObj)}
                       ,{<<"created">>, kz_doc:created(JObj)}
                       ,{<<"updated">>, kz_doc:modified(JObj)}
                       ,{<<"uploads">>, normalize_attachments(As)}
                       ,{<<"port_state">>, kz_json:get_value(?PORT_PVT_STATE, JObj, ?PORT_UNCONFIRMED)}
                       ,{<<"sent">>, kz_json:get_value(?PVT_SENT, JObj, 'false')}
                       ]
                      ,kz_doc:public_fields(JObj)
                      ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get(ne_binary()) -> {'ok', kz_json:object()} |
                          {'error', 'not_found'}.
get(DID=?NE_BINARY) ->
    View = ?ACTIVE_PORT_IN_NUMBERS,
    ViewOptions = [{'key', DID}, 'include_docs'],
    case kz_datamgr:get_single_result(?KZ_PORT_REQUESTS_DB, View, ViewOptions) of
        {'ok', Port} -> {'ok', kz_json:get_value(<<"doc">>, Port)};
        {'error', _E} ->
            lager:debug("failed to query for port number '~s': ~p", [DID, _E]),
            {'error', 'not_found'}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec account_active_ports(ne_binary()) -> {'ok', kz_json:objects()} |
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec account_has_active_port(ne_binary()) -> boolean().
account_has_active_port(AccountId) ->
    case account_active_ports(AccountId) of
        {'ok', [_|_]} -> 'true';
        {'error', 'not_found'} -> 'false'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec normalize_attachments(kz_json:object()) -> kz_json:object().
normalize_attachments(Attachments) ->
    kz_json:map(fun normalize_attachments_map/2, Attachments).

%% @private
-spec normalize_attachments_map(kz_json:path(), kz_json:json_term()) ->
                                       {kz_json:path(), kz_json:json_term()}.
normalize_attachments_map(K, V) ->
    {K, kz_json:delete_keys([<<"digest">>, <<"revpos">>, <<"stub">>], V)}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec normalize_numbers(kz_json:object()) -> kz_json:object().
normalize_numbers(PortReq) ->
    Numbers = kz_json:get_value(?NUMBERS_KEY, PortReq, kz_json:new()),
    Normalized = kz_json:map(fun normalize_number_map/2, Numbers),
    kz_json:set_value(?NUMBERS_KEY, Normalized, PortReq).

%% @private
-spec normalize_number_map(kz_json:path(), kz_json:json_term()) ->
                                  {kz_json:path(), kz_json:json_term()}.
normalize_number_map(N, Meta) ->
    {knm_converters:normalize(N), Meta}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec transition_to_submitted(kz_json:object()) -> transition_response().
-spec transition_to_pending(kz_json:object()) -> transition_response().
-spec transition_to_scheduled(kz_json:object()) -> transition_response().
-spec transition_to_complete(kz_json:object()) -> transition_response().
-spec transition_to_rejected(kz_json:object()) -> transition_response().
-spec transition_to_canceled(kz_json:object()) -> transition_response().

transition_to_submitted(JObj) ->
    transition(JObj, [?PORT_UNCONFIRMED, ?PORT_REJECTED], ?PORT_SUBMITTED).

transition_to_pending(JObj) ->
    transition(JObj, [?PORT_SUBMITTED], ?PORT_PENDING).

transition_to_scheduled(JObj) ->
    FromSubmitted = kapps_config:get_is_true(?KNM_CONFIG_CAT
                                            ,<<"allow_port_transition_from_submitted_to_scheduled">>
                                            ,'false'),
    transition(JObj, states_to_scheduled(FromSubmitted), ?PORT_SCHEDULED).

states_to_scheduled(_AllowFromSubmitted='false') ->
    [?PORT_PENDING];
states_to_scheduled(_AllowFromSubmitted='true') ->
    [?PORT_SUBMITTED | states_to_scheduled('false')].

transition_to_complete(JObj) ->
    case transition(JObj, [?PORT_PENDING, ?PORT_SCHEDULED, ?PORT_REJECTED], ?PORT_COMPLETED) of
        {'error', _}=E -> E;
        {'ok', Transitioned} -> completed_port(Transitioned)
    end.

transition_to_rejected(JObj) ->
    transition(JObj, [?PORT_SUBMITTED, ?PORT_PENDING, ?PORT_SCHEDULED], ?PORT_REJECTED).

transition_to_canceled(JObj) ->
    transition(JObj, [?PORT_UNCONFIRMED, ?PORT_SUBMITTED, ?PORT_PENDING, ?PORT_SCHEDULED, ?PORT_REJECTED], ?PORT_CANCELED).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_transition(kz_json:object(), ne_binary()) -> transition_response().
maybe_transition(PortReq, ?PORT_SUBMITTED) ->
    transition_to_submitted(PortReq);
maybe_transition(PortReq, ?PORT_PENDING) ->
    transition_to_pending(PortReq);
maybe_transition(PortReq, ?PORT_SCHEDULED) ->
    transition_to_scheduled(PortReq);
maybe_transition(PortReq, ?PORT_COMPLETED) ->
    transition_to_complete(PortReq);
maybe_transition(PortReq, ?PORT_REJECTED) ->
    transition_to_rejected(PortReq);
maybe_transition(PortReq, ?PORT_CANCELED) ->
    transition_to_canceled(PortReq).

-spec transition(kz_json:object(), ne_binaries(), ne_binary()) ->
                        transition_response().
-spec transition(kz_json:object(), ne_binaries(), ne_binary(), ne_binary()) ->
                        transition_response().
transition(JObj, FromStates, ToState) ->
    transition(JObj, FromStates, ToState, current_state(JObj)).

transition(_JObj, [], _ToState, _CurrentState) ->
    lager:debug("cant go from ~s to ~s", [_CurrentState, _ToState]),
    {'error', 'invalid_state_transition'};
transition(JObj, [CurrentState | _], ToState, CurrentState) ->
    lager:debug("going from ~s to ~s", [CurrentState, ToState]),
    {'ok', kz_json:set_value(?PORT_PVT_STATE, ToState, JObj)};
transition(JObj, [_FromState | FromStates], ToState, CurrentState) ->
    lager:debug("skipping from ~s to ~s c ~p", [_FromState, ToState, CurrentState]),
    transition(JObj, FromStates, ToState, CurrentState).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec charge_for_port(kz_json:object()) -> 'ok' | 'error'.
-spec charge_for_port(kz_json:object(), ne_binary()) -> 'ok' | 'error'.
charge_for_port(JObj) ->
    charge_for_port(JObj, kz_doc:account_id(JObj)).
charge_for_port(_JObj, AccountId) ->
    Services = kz_services:fetch(AccountId),
    Cost = kz_services:activation_charges(<<"number_services">>, ?FEATURE_PORT, Services),
    Transaction = kz_transaction:debit(AccountId, wht_util:dollars_to_units(Cost)),
    kz_services:commit_transactions(Services, [Transaction]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec assign_to_app(ne_binary(), api_ne_binary(), kz_json:object()) ->
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec send_submitted_requests() -> 'ok'.
send_submitted_requests() ->
    case kz_datamgr:get_results(?KZ_PORT_REQUESTS_DB
                               ,?VIEW_LISTING_SUBMITTED
                               ,['include_docs']
                               )
    of
        {'error', _R} ->
            lager:error("failed to open view ~s ~p", [?VIEW_LISTING_SUBMITTED, _R]);
        {'ok', []} -> 'ok';
        {'ok', JObjs} ->
            lists:foreach(fun send_submitted_request/1, JObjs),
            lager:debug("sent requests")
    end.

send_submitted_request(JObj) ->
    maybe_send_request(kz_json:get_value(<<"doc">>, JObj)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec migrate() -> 'ok'.
migrate() ->
    kz_util:put_callid(<<"port_request_migration">>),
    lager:debug("migrating port request documents, if necessary"),
    migrate(<<>>, 10),
    lager:debug("finished migrating port request documents").

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec completed_port(kz_json:object()) -> transition_response().
completed_port(PortReq) ->
    case charge_for_port(PortReq) of
        'error' -> throw({'error', 'failed_to_charge'});
        'ok' ->
            lager:debug("successfully charged for port, transitioning numbers to active"),
            transition_numbers(PortReq)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec transition_numbers(kz_json:object()) -> transition_response().
transition_numbers(PortReq) ->
    PortReqId = kz_doc:id(PortReq),
    AccountId = kz_json:get_value(?PVT_ACCOUNT_ID, PortReq),
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_send_request(kz_json:object()) -> 'ok'.
-spec maybe_send_request(kz_json:object(), api_binary()) -> 'ok'.
maybe_send_request(JObj) ->
    kz_util:put_callid(kz_doc:id(JObj)),
    AccountId = kz_doc:account_id(JObj),
    case kz_account:fetch(AccountId) of
        {'ok', AccountDoc} ->
            Url = kz_json:get_value(<<"submitted_port_requests_url">>, AccountDoc),
            maybe_send_request(JObj, Url);
        {'error', _R} ->
            lager:error("failed to open account ~s:~p", [AccountId, _R])
    end.

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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec send_request(kz_json:object(), ne_binary()) -> 'error' | 'ok'.
send_request(JObj, Url) ->
    Headers = [{"Content-Type", "application/json"}
              ,{"User-Agent", kz_term:to_list(node())}
              ],
    Uri = kz_term:to_list(<<Url/binary, "/", (kz_doc:id(JObj))/binary>>),
    Remove = [?PVT_REV
             ,<<"ui_metadata">>
             ,<<"_attachments">>
             ,<<"pvt_request_id">>
             ,?PVT_TYPE
             ,?PVT_VSN
             ,?PVT_ACCOUNT_DB
             ],
    Replace = [{?PVT_ID, <<"id">>}
              ,{?PORT_PVT_STATE, <<"port_state">>}
              ,{?PVT_ACCOUNT_ID, <<"account_id">>}
              ,{?PVT_CREATED, <<"created">>}
              ,{?PVT_MODIFIED, <<"modified">>}
              ],
    Data = kz_json:encode(kz_json:normalize_jobj(JObj, Remove, Replace)),
    case kz_http:post(Uri, Headers, Data) of
        {'ok', 200, _Headers, _Resp} ->
            lager:debug("submitted_port_request successfully sent");
        _Other ->
            lager:error("failed to send submitted_port_request ~p", [_Other]),
            'error'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec send_attachements(ne_binary(), kz_json:object()) -> 'error' | 'ok'.
send_attachements(Url, JObj) ->
    try fetch_and_send(Url, JObj) of
        'ok' -> 'ok'
    catch
        'throw':'error' -> 'error'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch_and_send(ne_binary(), kz_json:object()) -> 'ok'.
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec send_attachment(ne_binary(), ne_binary(), ne_binary(), kz_json:object(), binary()) ->
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_flag(kz_json:object()) -> 'ok'.
set_flag(JObj) ->
    Doc = kz_json:set_value(?PVT_SENT, 'true', JObj),
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

-spec migrate_doc(kz_json:object()) -> api_object().
migrate_doc(PortRequest) ->
    case kz_json:get_value(?PVT_TREE, PortRequest) of
        'undefined' -> update_doc(PortRequest);
        _Tree -> 'undefined'
    end.

-spec update_doc(kz_json:object()) -> api_object().
-spec update_doc(kz_json:object(), api_binary()) -> api_object().
update_doc(PortRequest) ->
    update_doc(PortRequest, kz_doc:account_id(PortRequest)).

update_doc(_Doc, 'undefined') ->
    lager:debug("no account id in doc ~s", [kz_doc:id(_Doc)]),
    'undefined';
update_doc(PortRequest, AccountId) ->
    {'ok', AccountDoc} = kz_account:fetch(AccountId),
    kz_json:set_value(?PVT_TREE, kz_account:tree(AccountDoc), PortRequest).

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
