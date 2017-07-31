%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%   Sponsored by Conversant Ltd,
%%%     implemented by SIPLABS, LLC (Ilya Ashchepkov)
%%%-------------------------------------------------------------------
-module(cf_util).

-export([presence_probe/2]).
-export([presence_mwi_query/2]).
-export([notification_register/2]).
-export([unsolicited_owner_mwi_update/2]).
-export([unsolicited_endpoint_mwi_update/2]).
-export([alpha_to_dialpad/1]).

-export([handle_bridge_failure/2, handle_bridge_failure/3]).
-export([send_default_response/2]).

-export([get_operator_callflow/1]).
-export([endpoint_id_by_sip_username/2]).
-export([owner_ids_by_sip_username/2]).
-export([apply_dialplan/2]).

-export([sip_users_from_device_ids/2]).

-export([caller_belongs_to_group/2
        ,maybe_belongs_to_group/3
        ,caller_belongs_to_user/2
        ,find_endpoints/3
        ,find_channels/2
        ,find_user_endpoints/3
        ,find_group_endpoints/2
        ,check_value_of_fields/4
        ,get_timezone/2
        ]).

-export([wait_for_noop/2]).
-export([start_task/3]).
-export([start_event_listener/3
        ,event_listener_name/2
        ]).

-include("callflow.hrl").
-include_lib("kazoo_stdlib/include/kazoo_json.hrl").

-define(OWNER_KEY(Db, User), {?MODULE, 'owner_id', Db, User}).
-define(SIP_USER_OWNERS_KEY(Db, User), {?MODULE, 'sip_user_owners', Db, User}).
-define(SIP_ENDPOINT_ID_KEY(Db, User), {?MODULE, 'sip_endpoint_id', Db, User}).
-define(PARKING_PRESENCE_KEY(Db, Request), {?MODULE, 'parking_callflow', Db, Request}).
-define(MANUAL_PRESENCE_KEY(Db), {?MODULE, 'manual_presence', Db}).
-define(OPERATOR_KEY, kapps_config:get_ne_binary(?CF_CONFIG_CAT, <<"operator_key">>, <<"0">>)).
-define(MWI_SEND_UNSOLICITATED_UPDATES, <<"mwi_send_unsoliciated_updates">>).
-define(VM_CACHE_KEY(Db, Id), {?MODULE, 'vmbox', Db, Id}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec presence_probe(kz_json:object(), kz_proplist()) -> any().
presence_probe(JObj, _Props) ->
    'true' = kapi_presence:probe_v(JObj),
    Username = kz_json:get_value(<<"Username">>, JObj),
    Realm = kz_json:get_value(<<"Realm">>, JObj),
    ProbeRepliers = [fun manual_presence/2
                    ,fun presence_parking_slot/2
                    ],
    lists:takewhile(fun(Fun) ->
                            Fun(Username, Realm) =:= 'not_found'
                    end, ProbeRepliers).

-spec presence_parking_slot(ne_binary(), ne_binary()) -> 'ok' | 'not_found'.
presence_parking_slot(Username, Realm) ->
    case kapps_util:get_account_by_realm(Realm) of
        {'ok', AccountDb} ->
            maybe_presence_parking_slot_resp(Username, Realm, AccountDb);
        _E -> 'not_found'
    end.

-spec maybe_presence_parking_slot_resp(ne_binary(), ne_binary(), ne_binary()) -> 'ok' | 'not_found'.
maybe_presence_parking_slot_resp(Username, Realm, AccountDb) ->
    case kz_cache:fetch_local(?CACHE_NAME, ?PARKING_PRESENCE_KEY(AccountDb, Username)) of
        {'ok', 'false'} -> 'not_found';
        {'ok', SlotNumber} ->
            presence_parking_slot_resp(Username, Realm, AccountDb, SlotNumber);
        {'error', 'not_found'} ->
            maybe_presence_parking_flow(Username, Realm, AccountDb)
    end.

-spec maybe_presence_parking_flow(ne_binary(), ne_binary(), ne_binary()) -> 'ok' | 'not_found'.
maybe_presence_parking_flow(Username, Realm, AccountDb) ->
    AccountId = kz_util:format_account_id(AccountDb, 'raw'),
    _ = cf_flow:lookup(Username, AccountId),
    case kz_cache:fetch_local(?CACHE_NAME, ?CF_FLOW_CACHE_KEY(Username, AccountDb)) of
        {'error', 'not_found'} -> 'not_found';
        {'ok', Flow} ->
            case kz_json:get_value([<<"flow">>, <<"module">>], Flow) of
                <<"park">> ->
                    SlotNumber = kz_json:get_ne_value(<<"capture_group">>, Flow, Username),
                    kz_cache:store_local(?CACHE_NAME, ?PARKING_PRESENCE_KEY(AccountDb, Username), SlotNumber),
                    presence_parking_slot_resp(Username, Realm, AccountDb, SlotNumber);
                _Else ->
                    kz_cache:store_local(?CACHE_NAME, ?PARKING_PRESENCE_KEY(AccountDb, Username), 'false'),
                    'not_found'
            end
    end.

-spec presence_parking_slot_resp(ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
presence_parking_slot_resp(Username, Realm, AccountDb, SlotNumber) ->
    cf_park:update_presence(SlotNumber, <<Username/binary, "@", Realm/binary>>, AccountDb).

-spec manual_presence(ne_binary(), ne_binary()) -> 'ok' | 'not_found'.
manual_presence(Username, Realm) ->
    case kapps_util:get_account_by_realm(Realm) of
        {'ok', AccountDb} -> check_manual_presence(Username, Realm, AccountDb);
        _E -> 'not_found'
    end.

-spec check_manual_presence(ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
check_manual_presence(Username, Realm, AccountDb) ->
    case kz_cache:fetch_local(?CACHE_NAME, ?MANUAL_PRESENCE_KEY(AccountDb)) of
        {'ok', JObj} -> manual_presence_resp(Username, Realm, JObj);
        {'error', 'not_found'} -> fetch_manual_presence_doc(Username, Realm, AccountDb)
    end.

-spec fetch_manual_presence_doc(ne_binary(), ne_binary(), ne_binary()) -> 'ok' | 'not_found'.
fetch_manual_presence_doc(Username, Realm, AccountDb) ->
    case kz_datamgr:open_doc(AccountDb, ?MANUAL_PRESENCE_DOC) of
        {'ok', JObj} ->
            CacheProps = [{'origin', {'db', AccountDb, ?MANUAL_PRESENCE_DOC}}],
            kz_cache:store_local(?CACHE_NAME, ?MANUAL_PRESENCE_KEY(AccountDb), JObj, CacheProps),
            manual_presence_resp(Username, Realm, JObj);
        {'error', 'not_found'} ->
            CacheProps = [{'origin', {'db', AccountDb, ?MANUAL_PRESENCE_DOC}}],
            kz_cache:store_local(?CACHE_NAME, ?MANUAL_PRESENCE_KEY(AccountDb), kz_json:new(), CacheProps);
        {'error', _} -> 'not_found'
    end.

-spec manual_presence_resp(ne_binary(), ne_binary(), kz_json:object()) -> 'ok' | 'not_found'.
manual_presence_resp(Username, Realm, JObj) ->
    PresenceId = <<Username/binary, "@", Realm/binary>>,
    case kz_json:get_value(PresenceId, JObj) of
        'undefined' -> 'not_found';
        State ->
            PresenceUpdate = [{<<"Presence-ID">>, PresenceId}
                             ,{<<"State">>, State}
                             ,{<<"Call-ID">>, kz_term:to_hex_binary(crypto:hash(md5, PresenceId))}
                              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                             ],
            kapps_util:amqp_pool_send(PresenceUpdate, fun kapi_presence:publish_update/1)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec presence_mwi_query(kz_json:object(), kz_proplist()) -> 'ok'.
presence_mwi_query(JObj, _Props) ->
    'true' = kapi_presence:mwi_query_v(JObj),
    _ = kz_util:put_callid(JObj),
    mwi_query(JObj).

-spec notification_register(kz_json:object(), kz_proplist()) -> 'ok'.
notification_register(JObj, _Props) ->
    'true' = kapi_notifications:register_v(JObj),
    _ = kz_util:put_callid(JObj),
    mwi_query(JObj).

-spec mwi_query(kz_json:object()) -> 'ok'.
mwi_query(JObj) ->
    Realm = kz_json:get_value(<<"Realm">>, JObj),
    case kapps_util:get_account_by_realm(Realm) of
        {'ok', AccountDb} ->
            lager:debug("replying to mwi query"),
            Username = kz_json:get_value(<<"Username">>, JObj),
            maybe_vm_mwi_resp(Username, Realm, AccountDb, JObj);
        _Else -> 'ok'
    end.

-spec maybe_vm_mwi_resp(api_binary(), ne_binary(), ne_binary(), kz_json:object()) -> 'ok'.
maybe_vm_mwi_resp('undefined', _Realm, _AccountDb, _JObj) -> 'ok';
maybe_vm_mwi_resp(<<_/binary>> = VMNumber, Realm, AccountDb, JObj) ->
    case mailbox(AccountDb, VMNumber) of
        {'ok', Doc} -> vm_mwi_resp(Doc, VMNumber, Realm, JObj);
        {'error', _} -> mwi_resp(VMNumber, Realm, AccountDb, JObj)
    end.

-spec vm_mwi_resp(kz_json:object(), ne_binary(), ne_binary(), kz_json:object()) -> 'ok'.
vm_mwi_resp(Doc, VMNumber, Realm, JObj) ->
    {New, Saved} = vm_count(Doc),
    send_mwi_update(New, Saved, VMNumber, Realm, JObj).

-spec mwi_resp(ne_binary(), ne_binary(), ne_binary(), kz_json:object()) -> 'ok'.
mwi_resp(Username, Realm, AccountDb, JObj) ->
    case owner_ids_by_sip_username(AccountDb, Username) of
        {'ok', [<<_/binary>> = OwnerId]} ->
            mwi_resp(Username, Realm, OwnerId, AccountDb, JObj);
        _Else -> 'ok'
    end.

-spec mwi_resp(ne_binary(), ne_binary(), ne_binary(), ne_binary(), kz_json:object()) -> 'ok'.
mwi_resp(Username, Realm, OwnerId, AccountDb, JObj) ->
    {New, Saved} = vm_count_by_owner(AccountDb, OwnerId),
    send_mwi_update(New, Saved, Username, Realm, JObj).

-spec is_unsolicited_mwi_enabled(ne_binary()) -> boolean().
is_unsolicited_mwi_enabled(AccountId) ->
    kapps_config:get_is_true(?CF_CONFIG_CAT, ?MWI_SEND_UNSOLICITATED_UPDATES, 'true')
        andalso kz_term:is_true(kapps_account_config:get(AccountId, ?CF_CONFIG_CAT, ?MWI_SEND_UNSOLICITATED_UPDATES, 'true')).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-type mwi_update_return() :: 'missing_account_db' |
                             'missing_owner_id'.
-spec unsolicited_owner_mwi_update(api_binary(), api_binary()) ->
                                          'ok' |
                                          {'error', mwi_update_return()} |
                                          kz_datamgr:data_error().
-spec unsolicited_owner_mwi_update(ne_binary(), ne_binary(), boolean()) ->
                                          'ok' |
                                          {'error', mwi_update_return()} |
                                          kz_datamgr:data_error().
unsolicited_owner_mwi_update('undefined', _) -> {'error', 'missing_account_db'};
unsolicited_owner_mwi_update(_, 'undefined') -> {'error', 'missing_owner_id'};
unsolicited_owner_mwi_update(AccountDb, OwnerId) ->
    AccountId = kz_util:format_account_id(AccountDb),
    MWIUpdate = is_unsolicited_mwi_enabled(AccountId),
    unsolicited_owner_mwi_update(AccountDb, OwnerId, MWIUpdate).

unsolicited_owner_mwi_update(_AccountDb, _OwnerId, 'false') ->
    lager:debug("unsolicitated mwi updated disabled : ~s", [_AccountDb]);
unsolicited_owner_mwi_update(AccountDb, OwnerId, 'true') ->
    ViewOptions = [{'key', [OwnerId, <<"device">>]}
                  ,'include_docs'
                  ],
    case kz_datamgr:get_results(AccountDb, <<"attributes/owned">>, ViewOptions) of
        {'ok', JObjs} ->
            {New, Saved} = vm_count_by_owner(AccountDb, OwnerId),
            AccountId = kz_util:format_account_id(AccountDb, 'raw'),
            lists:foreach(
              fun(JObj) -> maybe_send_mwi_update(JObj, AccountId, New, Saved) end
                         ,JObjs
             );
        {'error', _R}=E ->
            lager:warning("failed to find devices owned by ~s: ~p", [OwnerId, _R]),
            E
    end.

-spec maybe_send_mwi_update(kz_json:object(), ne_binary(), integer(), integer()) -> 'ok'.
maybe_send_mwi_update(JObj, AccountId, New, Saved) ->
    J = kz_json:get_value(<<"doc">>, JObj),
    Username = kz_device:sip_username(J),
    Realm = kz_endpoint:get_sip_realm(J, AccountId),
    OwnerId = get_endpoint_owner(J),
    case kz_device:sip_method(J) =:= <<"password">>
        andalso Username =/= 'undefined'
        andalso Realm =/= 'undefined'
        andalso OwnerId =/= 'undefined'
        andalso kz_device:unsolicitated_mwi_updates(J)
    of
        'true' -> send_mwi_update(New, Saved, Username, Realm);
        'false' -> 'ok'
    end.

-spec unsolicited_endpoint_mwi_update(api_binary(), api_binary()) ->
                                             'ok' | {'error', any()}.
-spec unsolicited_endpoint_mwi_update(ne_binary(), ne_binary(), boolean()) ->
                                             'ok' | {'error', any()}.
unsolicited_endpoint_mwi_update('undefined', _) ->
    {'error', 'missing_account_db'};
unsolicited_endpoint_mwi_update(_, 'undefined') ->
    {'error', 'missing_owner_id'};
unsolicited_endpoint_mwi_update(AccountDb, EndpointId) ->
    AccountId = kz_util:format_account_id(AccountDb),
    MWIUpdate = is_unsolicited_mwi_enabled(AccountId),
    unsolicited_endpoint_mwi_update(AccountDb, EndpointId, MWIUpdate).

unsolicited_endpoint_mwi_update(_AccountDb, _EndpointId, 'false') ->
    lager:debug("unsolicitated mwi updated disabled : ~s", [_AccountDb]);
unsolicited_endpoint_mwi_update(AccountDb, EndpointId, 'true') ->
    case kz_datamgr:open_cache_doc(AccountDb, EndpointId) of
        {'error', _}=E -> E;
        {'ok', JObj} -> maybe_send_endpoint_mwi_update(AccountDb, JObj)
    end.

-spec maybe_send_endpoint_mwi_update(ne_binary(), kz_json:object()) ->
                                            'ok' | {'error', 'not_appropriate'}.
-spec maybe_send_endpoint_mwi_update(ne_binary(), kz_json:object(), boolean()) ->
                                            'ok' | {'error', 'not_appropriate'}.

maybe_send_endpoint_mwi_update(AccountDb, JObj) ->
    maybe_send_endpoint_mwi_update(AccountDb, JObj, kz_device:unsolicitated_mwi_updates(JObj)).

maybe_send_endpoint_mwi_update(_AccountDb, _JObj, 'false') ->
    lager:debug("unsolicitated mwi updates disabled for ~s/~s", [_AccountDb, kz_doc:id(_JObj)]);
maybe_send_endpoint_mwi_update(AccountDb, JObj, 'true') ->
    AccountId = kz_util:format_account_id(AccountDb, 'raw'),
    Username = kz_device:sip_username(JObj),
    Realm = kz_endpoint:get_sip_realm(JObj, AccountId),
    OwnerId = get_endpoint_owner(JObj),
    case kz_device:sip_method(JObj) =:= <<"password">>
        andalso Username =/= 'undefined'
        andalso Realm =/= 'undefined'
    of
        'false' -> {'error', 'not_appropriate'};
        'true' ->
            {New, Saved} = vm_count_by_owner(AccountDb, OwnerId),
            send_mwi_update(New, Saved, Username, Realm)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-type vm_count() :: non_neg_integer().
-spec send_mwi_update(vm_count(), vm_count(), ne_binary(), ne_binary()) -> 'ok'.
send_mwi_update(New, Saved, Username, Realm) ->
    send_mwi_update(New, Saved, Username, Realm, kz_json:new()).

-spec send_mwi_update(vm_count(), vm_count(), ne_binary(), ne_binary(), kz_json:object()) -> 'ok'.
send_mwi_update(New, Saved, Username, Realm, JObj) ->
    Command = [{<<"To">>, <<Username/binary, "@", Realm/binary>>}
              ,{<<"Messages-New">>, New}
              ,{<<"Messages-Saved">>, Saved}
              ,{<<"Call-ID">>, kz_json:get_value(<<"Call-ID">>, JObj)}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    lager:debug("updating MWI for ~s@~s (~p/~p)", [Username, Realm, New, Saved]),
    kapps_util:amqp_pool_send(Command, fun kapi_presence:publish_unsolicited_mwi_update/1).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec alpha_to_dialpad(ne_binary()) -> ne_binary().
alpha_to_dialpad(Value) ->
    << <<(dialpad_digit(C))>> || <<C>> <= kz_term:to_lower_binary(Value), is_alpha(C) >>.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_alpha(char()) -> boolean().
is_alpha(Char) ->
    Char =< $z
        andalso Char >= $a.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec dialpad_digit(97..122) -> 50..57.
dialpad_digit(ABC) when ABC =:= $a
                        orelse ABC =:= $b
                        orelse ABC =:= $c -> $2;
dialpad_digit(DEF) when DEF =:= $d
                        orelse DEF =:= $e
                        orelse DEF =:= $f -> $3;
dialpad_digit(GHI) when GHI =:= $g
                        orelse GHI =:= $h
                        orelse GHI =:= $i -> $4;
dialpad_digit(JKL) when JKL =:= $j
                        orelse JKL =:= $k
                        orelse JKL =:= $l -> $5;
dialpad_digit(MNO) when MNO =:= $m
                        orelse MNO =:= $n
                        orelse MNO =:= $o -> $6;
dialpad_digit(PQRS) when PQRS =:= $p
                         orelse PQRS =:= $q
                         orelse PQRS =:= $r
                         orelse PQRS =:= $s -> $7;
dialpad_digit(TUV) when TUV =:= $t
                        orelse TUV =:= $u
                        orelse TUV =:= $v -> $8;
dialpad_digit(WXYZ) when WXYZ =:= $w
                         orelse WXYZ =:= $x
                         orelse WXYZ =:= $y
                         orelse WXYZ =:= $z -> $9.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec owner_ids_by_sip_username(ne_binary(), ne_binary()) ->
                                       {'ok', ne_binaries()} |
                                       {'error', any()}.
owner_ids_by_sip_username(AccountDb, Username) ->
    case kz_cache:peek_local(?CACHE_NAME, ?SIP_USER_OWNERS_KEY(AccountDb, Username)) of
        {'ok', _}=Ok -> Ok;
        {'error', 'not_found'} ->
            get_owner_ids_by_sip_username(AccountDb, Username)
    end.

-spec get_owner_ids_by_sip_username(ne_binary(), ne_binary()) ->
                                           {'ok', ne_binaries()} |
                                           {'error', any()}.
get_owner_ids_by_sip_username(AccountDb, Username) ->
    ViewOptions = [{'key', Username}],
    case kz_datamgr:get_single_result(AccountDb, <<"attributes/sip_username">>, ViewOptions) of
        {'ok', JObj} ->
            EndpointId = kz_doc:id(JObj),
            OwnerIds = kz_json:get_value(<<"value">>, JObj, []),
            CacheProps = [{'origin', {'db', AccountDb, EndpointId}}],
            kz_cache:store_local(?CACHE_NAME, ?SIP_USER_OWNERS_KEY(AccountDb, Username), OwnerIds, CacheProps),
            {'ok', OwnerIds};
        {'error', _R}=E ->
            lager:warning("unable to lookup sip username ~s for owner ids: ~p", [Username, _R]),
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec endpoint_id_by_sip_username(ne_binary(), ne_binary()) ->
                                         {'ok', ne_binary()} |
                                         {'error', 'not_found'}.
endpoint_id_by_sip_username(AccountDb, Username) ->
    case kz_cache:peek_local(?CACHE_NAME, ?SIP_ENDPOINT_ID_KEY(AccountDb, Username)) of
        {'ok', _}=Ok -> Ok;
        {'error', 'not_found'} ->
            get_endpoint_id_by_sip_username(AccountDb, Username)
    end.

-spec get_endpoint_id_by_sip_username(ne_binary(), ne_binary()) ->
                                             {'ok', ne_binary()} |
                                             {'error', 'not_found'}.
get_endpoint_id_by_sip_username(AccountDb, Username) ->
    ViewOptions = [{'key', Username}],
    case kz_datamgr:get_single_result(AccountDb, <<"attributes/sip_username">>, ViewOptions) of
        {'ok', JObj} ->
            EndpointId = kz_doc:id(JObj),
            CacheProps = [{'origin', {'db', AccountDb, EndpointId}}],
            kz_cache:store_local(?CACHE_NAME, ?SIP_ENDPOINT_ID_KEY(AccountDb, Username), EndpointId, CacheProps),
            {'ok', EndpointId};
        {'error', _R} ->
            lager:warning("lookup sip username ~s for owner ids failed: ~p", [Username, _R]),
            {'error', 'not_found'}
    end.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec get_operator_callflow(ne_binary()) -> {'ok', kz_json:object()} |
                                            kz_datamgr:data_error().
get_operator_callflow(Account) ->
    AccountDb = kz_util:format_account_db(Account),
    Options = [{'key', ?OPERATOR_KEY}
              ,'include_docs'
              ,'first_when_multiple'
              ],
    case kz_datamgr:get_single_result(AccountDb, ?LIST_BY_NUMBER, Options) of
        {'ok', JObj} ->
            {'ok', kz_json:get_value([<<"doc">>, <<"flow">>], JObj, kz_json:new())};
        {'error', _R}=E ->
            lager:warning("unable to find operator callflow in ~s: ~p", [Account, _R]),
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Look for children branches to handle the failure replies of
%% certain actions, like cf_offnet and cf_resources
%% @end
%%--------------------------------------------------------------------
-spec handle_bridge_failure({'fail', kz_json:object()} | api_binary(), kapps_call:call()) ->
                                   'ok' | 'not_found'.
handle_bridge_failure({'fail', Reason}, Call) ->
    {Cause, Code} = kapps_util:get_call_termination_reason(Reason),
    handle_bridge_failure(Cause, Code, Call);
handle_bridge_failure('undefined', _) ->
    'not_found';
handle_bridge_failure(Failure, Call) ->
    case cf_exe:attempt(Failure, Call) of
        {'attempt_resp', 'ok'} ->
            lager:info("found child branch to handle failure: ~s", [Failure]),
            'ok';
        {'attempt_resp', _} ->
            'not_found'
    end.

-spec handle_bridge_failure(api_binary(), api_binary(), kapps_call:call()) ->
                                   'ok' | 'not_found'.
handle_bridge_failure(Cause, Code, Call) ->
    lager:info("attempting to find failure branch for ~s:~s", [Code, Cause]),
    case (handle_bridge_failure(Cause, Call) =:= 'ok')
        orelse (handle_bridge_failure(Code, Call) =:= 'ok')
    of
        'true' -> 'ok';
        'false' -> 'not_found'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Send and wait for a call failure cause response
%% @end
%%--------------------------------------------------------------------
-spec send_default_response(ne_binary(), kapps_call:call()) -> 'ok'.
send_default_response(Cause, Call) ->
    case cf_exe:wildcard_is_empty(Call) of
        'false' -> 'ok';
        'true' ->
            case kz_call_response:send_default(Call, Cause) of
                {'error', 'no_response'} -> 'ok';
                {'ok', NoopId} ->
                    _ = kapps_call_command:wait_for_noop(Call, NoopId),
                    'ok'
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_endpoint_owner(kz_json:object()) -> api_binary().
get_endpoint_owner(JObj) ->
    maybe_get_endpoint_hotdesk_owner(JObj).

-spec maybe_get_endpoint_hotdesk_owner(kz_json:object()) -> api_binary().
maybe_get_endpoint_hotdesk_owner(JObj) ->
    case kz_json:get_keys([<<"hotdesk">>, <<"users">>], JObj) of
        [] -> maybe_get_endpoint_assigned_owner(JObj);
        [OwnerId] -> OwnerId;
        [_|_] -> 'undefined'
    end.

-spec maybe_get_endpoint_assigned_owner(kz_json:object()) -> api_binary().
maybe_get_endpoint_assigned_owner(JObj) ->
    kz_json:get_ne_value(<<"owner_id">>, JObj).

-spec apply_dialplan(ne_binary(), api_object()) -> ne_binary().
apply_dialplan(N, 'undefined') -> N;
apply_dialplan(Number, DialPlan) ->
    Regexs = kz_json:get_keys(DialPlan),
    case Regexs of
        [] -> Number;
        _ -> maybe_apply_dialplan(Regexs, DialPlan, Number)
    end.

-spec maybe_apply_dialplan(kz_json:path(), kz_json:object(), ne_binary()) -> ne_binary().
maybe_apply_dialplan([], _, Number) -> Number;
maybe_apply_dialplan([<<"system">>], DialPlan, Number) ->
    SystemDialPlans = load_system_dialplans(kz_json:get_value(<<"system">>, DialPlan)),
    SystemRegexs = lists:sort(kz_json:get_keys(SystemDialPlans)),
    maybe_apply_dialplan(SystemRegexs, SystemDialPlans, Number);
maybe_apply_dialplan([<<"system">>|Regexs], DialPlan, Number) ->
    maybe_apply_dialplan(Regexs ++ [<<"system">>], DialPlan, Number);
maybe_apply_dialplan([Key|_]=Keys, DialPlan, Number) ->
    case kz_json:get_value([Key, <<"regex">>], DialPlan) of
        'undefined' -> apply_dialplan(Key, Keys, DialPlan, Number);
        Regex -> apply_dialplan(Regex, Keys, DialPlan, Number)
    end.

-spec apply_dialplan(ne_binary(), kz_json:path(), kz_json:object(), ne_binary()) -> ne_binary().
apply_dialplan(Regex, [Key|Keys], DialPlan, Number) ->
    case re:run(Number, Regex, [{'capture', 'all', 'binary'}]) of
        'nomatch' ->
            maybe_apply_dialplan(Keys, DialPlan, Number);
        'match' ->
            Number;
        {'match', Captures} ->
            Root = lists:last(Captures),
            Prefix = kz_json:get_binary_value([Key, <<"prefix">>], DialPlan, <<>>),
            Suffix = kz_json:get_binary_value([Key, <<"suffix">>], DialPlan, <<>>),
            N = <<Prefix/binary, Root/binary, Suffix/binary>>,
            case kz_json:get_value([Key, <<"dialplan">>], DialPlan) of
                'undefined' -> maybe_apply_dialplan(Keys, DialPlan, N);
                InnerPlan -> InnerRegexs = kz_json:get_keys(InnerPlan),
                             N1 = maybe_apply_dialplan(InnerRegexs, InnerPlan, N),
                             maybe_apply_dialplan(Keys, DialPlan, N1)
            end
    end.

-spec load_system_dialplans(ne_binaries()) -> kz_json:object().
load_system_dialplans(Names) ->
    LowerNames = [kz_term:to_lower_binary(Name) || Name <- Names],
    Plans = kapps_config:get_all_kvs(<<"dialplans">>),
    lists:foldl(fold_system_dialplans(LowerNames), kz_json:new(), Plans).

-spec fold_system_dialplans(ne_binaries()) ->
                                   fun(({ne_binary(), kz_json:object()}, kz_json:object()) -> kz_json:object()).
fold_system_dialplans(Names) ->
    fun({Key, Val}, Acc) when is_list(Val) ->
            lists:foldl(fun(ValElem, A) -> maybe_dialplan_suits({Key, ValElem}, A, Names) end, Acc, Val);
       ({Key, Val}, Acc) ->
            maybe_dialplan_suits({Key, Val}, Acc, Names)
    end.

-spec maybe_dialplan_suits({ne_binary(), kz_json:object()} ,kz_json:object(), ne_binaries()) -> kz_json:object().
maybe_dialplan_suits({Key, Val}=KV, Acc, Names) ->
    Name = kz_term:to_lower_binary(kz_json:get_value(<<"name">>, Val)),
    case lists:member(Name, Names) of
        'true' -> kz_json:set_value(Key, Val, Acc);
        'false' -> maybe_system_dialplan_name(KV, Acc, Names)
    end.

-spec maybe_system_dialplan_name({ne_binary(), kz_json:object()} ,kz_json:object(), ne_binaries()) -> kz_json:object().
maybe_system_dialplan_name({Key, Val}, Acc, Names) ->
    Name = kz_term:to_lower_binary(Key),
    case lists:member(Name, Names) of
        'true' ->
            N = kz_term:to_binary(index_of(Name, Names)),
            kz_json:set_value(<<N/binary, "-", Key/binary>>, Val, Acc);
        'false' -> Acc
    end.

-spec index_of(ne_binary(), list()) -> api_integer().
index_of(Value, List) ->
    Map = lists:zip(List, lists:seq(1, length(List))),
    case dict:find(Value, dict:from_list(Map)) of
        {ok, Index} -> Index;
        error -> 'undefined'
    end.

-spec start_event_listener(kapps_call:call(), atom(), list()) ->
                                  {'ok', pid()} | {'error', any()}.
start_event_listener(Call, Mod, Args) ->
    lager:debug("starting evt listener ~p", [Mod]),
    Name = event_listener_name(Call, Mod),
    try cf_event_handler_sup:new(Name, Mod, [kapps_call:clear_helpers(Call) | Args]) of
        {'ok', P} -> {'ok', P};
        _E -> lager:debug("error starting event listener ~p: ~p", [Mod, _E]),
              {'error', _E}
    catch
        _:_R ->
            lager:info("failed to spawn ~p: ~p", [Mod, _R]),
            {'error', _R}
    end.

-spec event_listener_name(kapps_call:call(), atom() | ne_binary()) -> ne_binary().
event_listener_name(Call, Module) ->
    <<(kapps_call:call_id_direct(Call))/binary, "-", (kz_term:to_binary(Module))/binary>>.

-spec caller_belongs_to_group(ne_binary(), kapps_call:call()) -> boolean().
caller_belongs_to_group(GroupId, Call) ->
    maybe_belongs_to_group(kapps_call:authorizing_id(Call), GroupId, Call).

-spec maybe_belongs_to_group(ne_binary(), ne_binary(), kapps_call:call()) -> boolean().
maybe_belongs_to_group(TargetId, GroupId, Call) ->
    lists:member(TargetId, find_group_endpoints(GroupId, Call)).

-spec caller_belongs_to_user(ne_binary(), kapps_call:call()) -> boolean().
caller_belongs_to_user(UserId, Call) ->
    lists:member(kapps_call:authorizing_id(Call), find_user_endpoints([UserId],[],Call)).

-spec find_group_endpoints(ne_binary(), kapps_call:call()) -> ne_binaries().
find_group_endpoints(GroupId, Call) ->
    GroupsJObj = kz_attributes:groups(Call),
    case [kz_json:get_value(<<"value">>, JObj)
          || JObj <- GroupsJObj,
             kz_doc:id(JObj) =:= GroupId
         ]
    of
        [] -> [];
        [GroupEndpoints] ->
            Ids = kz_json:get_keys(GroupEndpoints),
            find_endpoints(Ids, GroupEndpoints, Call)
    end.

-spec find_endpoints(ne_binaries(), kz_json:object(), kapps_call:call()) ->
                            ne_binaries().
find_endpoints(Ids, GroupEndpoints, Call) ->
    {DeviceIds, UserIds} =
        lists:partition(fun(Id) ->
                                kz_json:get_value([Id, <<"type">>], GroupEndpoints) =:= <<"device">>
                        end, Ids),
    find_user_endpoints(UserIds, lists:sort(DeviceIds), Call).

-spec find_user_endpoints(ne_binaries(), ne_binaries(), kapps_call:call()) ->
                                 ne_binaries().
find_user_endpoints([], DeviceIds, _) -> DeviceIds;
find_user_endpoints(UserIds, DeviceIds, Call) ->
    UserDeviceIds = kz_attributes:owned_by(UserIds, <<"device">>, Call),
    lists:merge(lists:sort(UserDeviceIds), DeviceIds).

-spec find_channels(ne_binaries(), kapps_call:call()) -> kz_json:objects().
find_channels(Usernames, Call) ->
    Realm = kz_account:fetch_realm(kapps_call:account_id(Call)),
    lager:debug("finding channels for realm ~p, usernames ~p", [Realm, Usernames]),
    Req = [{<<"Realm">>, Realm}
          ,{<<"Usernames">>, Usernames}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case kapps_util:amqp_pool_request(Req
                                     ,fun kapi_call:publish_query_user_channels_req/1
                                     ,fun kapi_call:query_user_channels_resp_v/1
                                     )
    of
        {'ok', Resp} -> kz_json:get_value(<<"Channels">>, Resp, []);
        {'error', _E} ->
            lager:debug("failed to get channels: ~p", [_E]),
            []
    end.

-spec check_value_of_fields(kz_proplist(), boolean(), kz_json:object(), kapps_call:call()) ->
                                   boolean().
check_value_of_fields(Perms, Def, Data, Call) ->
    case lists:dropwhile(fun({K, _F}) ->
                                 kz_json:get_value(K, Data) =:= 'undefined'
                         end
                        ,Perms
                        )
    of
        [] -> Def;
        [{K, F}|_] -> F(kz_json:get_value(K, Data), Call)
    end.

-spec sip_users_from_device_ids(ne_binaries(), kapps_call:call()) -> ne_binaries().
sip_users_from_device_ids(EndpointIds, Call) ->
    lists:foldl(fun(EID, Acc) -> sip_users_from_device_id(EID, Acc, Call) end
               ,[]
               ,EndpointIds
               ).

-spec sip_users_from_device_id(ne_binary(), ne_binaries(), kapps_call:call()) ->
                                      ne_binaries().
sip_users_from_device_id(EndpointId, Acc, Call) ->
    case sip_user_from_device_id(EndpointId, Call) of
        'undefined' -> Acc;
        Username -> [Username|Acc]
    end.

-spec sip_user_from_device_id(ne_binary(), kapps_call:call()) -> api_binary().
sip_user_from_device_id(EndpointId, Call) ->
    case kz_endpoint:get(EndpointId, Call) of
        {'error', _} -> 'undefined';
        {'ok', Endpoint} ->
            kz_device:sip_username(Endpoint)
    end.

-spec wait_for_noop(kapps_call:call(), ne_binary()) ->
                           {'ok', kapps_call:call()} |
                           {'error', 'channel_hungup' | kz_json:object()}.
wait_for_noop(Call, NoopId) ->
    case kapps_call_command:receive_event(?MILLISECONDS_IN_DAY) of
        {'ok', JObj} ->
            process_event(Call, NoopId, JObj);
        {'error', 'timeout'} ->
            lager:debug("timed out waiting for noop(~s) to complete", [NoopId]),
            {'ok', Call}
    end.

-spec process_event(kapps_call:call(), ne_binary(), kz_json:object()) ->
                           {'ok', kapps_call:call()} |
                           {'error', any()}.
process_event(Call, NoopId, JObj) ->
    case kapps_call_command:get_event_type(JObj) of
        {<<"call_event">>, <<"CHANNEL_DESTROY">>, _} ->
            lager:debug("channel was destroyed"),
            {'error', 'channel_hungup'};
        {<<"error">>, _, <<"noop">>} ->
            lager:debug("channel execution error while waiting for ~s: ~s", [NoopId, kz_json:encode(JObj)]),
            {'error', JObj};
        {<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"noop">>} ->
            lager:debug("noop has returned"),
            {'ok', Call};
        {<<"call_event">>, <<"DTMF">>, _} ->
            DTMF = kz_json:get_value(<<"DTMF-Digit">>, JObj),
            lager:debug("recv DTMF ~s, adding to default", [DTMF]),
            wait_for_noop(kapps_call:add_to_dtmf_collection(DTMF, Call), NoopId);
        _Ignore ->
            wait_for_noop(Call, NoopId)
    end.

-spec get_timezone(kz_json:object(), kapps_call:call()) -> ne_binary().
get_timezone(JObj, Call) ->
    case kz_json:get_ne_binary_value(<<"timezone">>, JObj) of
        'undefined'   -> kz_account:timezone(kapps_call:account_id(Call));
        <<"inherit">> -> kz_account:timezone(kapps_call:account_id(Call)); %% UI-1808
        TZ -> TZ
    end.

-spec start_task(fun(), list(), kapps_call:call()) -> 'ok'.
start_task(Fun, Args, Call) ->
    SpawnInfo = {'cf_task', [Fun, Args]},
    cf_exe:add_event_listener(Call, SpawnInfo).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec mailbox(ne_binary(), ne_binary()) -> {'ok', kz_json:object()} |
                                           {'error', any()}.
mailbox(AccountDb, VMNumber) ->
    try kz_term:to_integer(VMNumber) of
        Number -> maybe_cached_mailbox(AccountDb, Number)
    catch
        _E:_R ->  {'error', 'not_found'}
    end.

-spec maybe_cached_mailbox(ne_binary(), integer()) -> {'ok', kz_json:object()} |
                                                      {'error', any()}.
maybe_cached_mailbox(AccountDb, VMNumber) ->
    case kz_cache:peek_local(?CACHE_NAME, ?VM_CACHE_KEY(AccountDb, VMNumber)) of
        {'ok', _}=Ok -> Ok;
        {'error', 'not_found'} -> get_mailbox(AccountDb, VMNumber)
    end.

-spec get_mailbox(ne_binary(), integer()) -> {'ok', kz_json:object()} |
                                             {'error', any()}.
get_mailbox(AccountDb, VMNumber) ->
    ViewOptions = [{'key', VMNumber}, 'include_docs'],
    case kz_datamgr:get_single_result(AccountDb, <<"vmboxes/listing_by_mailbox">>, ViewOptions) of
        {'ok', JObj} ->
            Doc = kz_json:get_value(<<"doc">>, JObj),
            EndpointId = kz_doc:id(Doc),
            CacheProps = [{'origin', {'db', AccountDb, EndpointId}}],
            kz_cache:store_local(?CACHE_NAME, ?VM_CACHE_KEY(AccountDb, VMNumber), Doc, CacheProps),
            {'ok', Doc};
        {'error', 'multiple_results'} ->
            lager:debug("multiple voicemail boxes with same number (~b)  in account db ~s", [VMNumber, AccountDb]),
            {'error', 'not_found'};
        {'error', _R}=E ->
            lager:warning("unable to lookup voicemail number ~b in account ~s: ~p", [VMNumber, AccountDb, _R]),
            E
    end.

-spec vm_count(kz_json:object()) -> {non_neg_integer(), non_neg_integer()}.
vm_count(JObj) ->
    AccountId = kz_doc:account_id(JObj),
    BoxId = kz_doc:id(JObj),
    kvm_messages:count_none_deleted(AccountId, BoxId).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec vm_count_by_owner(ne_binary(), api_binary()) -> {non_neg_integer(), non_neg_integer()}.
vm_count_by_owner(_AccountDb, 'undefined') -> {0, 0};
vm_count_by_owner(<<_/binary>> = AccountDb, <<_/binary>> = OwnerId) ->
    kvm_messages:count_by_owner(AccountDb, OwnerId).
