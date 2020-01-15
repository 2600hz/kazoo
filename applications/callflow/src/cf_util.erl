%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @author James Aimonetti
%%% @author Sponsored by Conversant Ltd, Implemented by SIPLABS, LLC (Ilya Ashchepkov)
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_util).

-export([presence_probe/2]).
-export([presence_mwi_query/2]).
-export([notification_register/2]).
-export([unsolicited_owner_mwi_update/2]).
-export([unsolicited_endpoint_mwi_update/2]).
-export([alpha_to_dialpad/1]).

-export([handle_bridge_failure/2, handle_bridge_failure/3]).
-export([send_default_response/2]).

-export([get_operator_callflow/1, get_operator_callflow/2]).
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

-export([flush_control_queue/1]).

-export([normalize_capture_group/1, normalize_capture_group/2]).

-export([token_check/2]).

-include("callflow.hrl").
-include_lib("kazoo_stdlib/include/kazoo_json.hrl").

-define(OWNER_KEY(Db, User), {?MODULE, 'owner_id', Db, User}).
-define(SIP_USER_OWNERS_KEY(Db, User), {?MODULE, 'sip_user_owners', Db, User}).
-define(SIP_ENDPOINT_ID_KEY(Db, User), {?MODULE, 'sip_endpoint_id', Db, User}).
-define(PARKING_PRESENCE_KEY(Db, Request), {?MODULE, 'parking_callflow', Db, Request}).
-define(MANUAL_PRESENCE_KEY(Db), {?MODULE, 'manual_presence', Db}).
-define(OPERATOR_KEY, kapps_config:get_ne_binary(?CF_CONFIG_CAT, <<"operator_key">>, <<"0">>)).

-define(VM_CACHE_KEY(Db, Id), {?MODULE, 'vmbox', Db, Id}).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec token_check(kapps_call:call(), kz_json:object()) -> boolean().
token_check(Call, Flow) ->
    case kapps_config:get_is_true(?CF_CONFIG_CAT, <<"calls_consume_tokens">>, 'true') of
        'false' ->
            %% If configured to not consume tokens then don't block the call
            'true';
        'true' ->
            {Name, Cost} = bucket_info(Call, Flow),
            DryRun = kapps_config:get_is_true(?CF_CONFIG_CAT, <<"should_dry_run_token_restrictions">>, 'false'),

            case kz_buckets:consume_tokens(?APP_NAME, Name, Cost) of
                'true' -> 'true';
                'false' when DryRun ->
                    lager:info("dry-run: bucket ~s does not have enough tokens(~b needed) for this call", [Name, Cost]),
                    'true';
                'false' ->
                    lager:warning("bucket ~s does not have enough tokens(~b needed) for this call", [Name, Cost]),
                    'false'
            end
    end.

-spec bucket_info(kapps_call:call(), kz_json:object()) ->
          {kz_term:ne_binary(), pos_integer()}.
bucket_info(Call, Flow) ->
    case kz_json:get_value(<<"pvt_bucket_name">>, Flow) of
        'undefined' -> {bucket_name_from_call(Call, Flow), bucket_cost(Flow)};
        Name -> {Name, bucket_cost(Flow)}
    end.

-spec bucket_name_from_call(kapps_call:call(), kz_json:object()) -> kz_term:ne_binary().
bucket_name_from_call(Call, Flow) ->
    FlowId = case kz_doc:id(Flow) of
                 'undefined' -> <<"cf_exe_", (kz_term:to_binary(self()))/binary>>;
                 FlowIdVal   -> FlowIdVal
             end,

    <<(kapps_call:account_id(Call))/binary, ":", (FlowId)/binary>>.

-spec bucket_cost(kz_json:object()) -> pos_integer().
bucket_cost(Flow) ->
    Min = kapps_config:get_integer(?CF_CONFIG_CAT, <<"min_bucket_cost">>, 5),
    case kz_json:get_integer_value(<<"pvt_bucket_cost">>, Flow) of
        'undefined' -> Min;
        N when N < Min -> Min;
        N -> N
    end.

-spec presence_probe(kz_json:object(), kz_term:proplist()) -> any().
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

-spec presence_parking_slot(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok' | 'not_found'.
presence_parking_slot(Username, Realm) ->
    case kapps_util:get_account_by_realm(Realm) of
        {'ok', AccountDb} ->
            maybe_presence_parking_slot_resp(Username, Realm, AccountDb);
        _E -> 'not_found'
    end.

-spec maybe_presence_parking_slot_resp(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok' | 'not_found'.
maybe_presence_parking_slot_resp(Username, Realm, AccountDb) ->
    case kz_cache:fetch_local(?CACHE_NAME, ?PARKING_PRESENCE_KEY(AccountDb, Username)) of
        {'ok', 'false'} -> 'not_found';
        {'ok', SlotNumber} ->
            presence_parking_slot_resp(Username, Realm, AccountDb, SlotNumber);
        {'error', 'not_found'} ->
            maybe_presence_parking_flow(Username, Realm, AccountDb)
    end.

-spec maybe_presence_parking_flow(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok' | 'not_found'.
maybe_presence_parking_flow(Username, Realm, AccountDb) ->
    AccountId = kzs_util:format_account_id(AccountDb),
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

-spec presence_parking_slot_resp(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
presence_parking_slot_resp(Username, Realm, AccountDb, SlotNumber) ->
    cf_park:update_presence(SlotNumber, <<Username/binary, "@", Realm/binary>>, AccountDb).

-spec manual_presence(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok' | 'not_found'.
manual_presence(Username, Realm) ->
    case kapps_util:get_account_by_realm(Realm) of
        {'ok', AccountDb} -> check_manual_presence(Username, Realm, AccountDb);
        _E -> 'not_found'
    end.

-spec check_manual_presence(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
check_manual_presence(Username, Realm, AccountDb) ->
    case kz_cache:fetch_local(?CACHE_NAME, ?MANUAL_PRESENCE_KEY(AccountDb)) of
        {'ok', JObj} -> manual_presence_resp(Username, Realm, JObj);
        {'error', 'not_found'} -> fetch_manual_presence_doc(Username, Realm, AccountDb)
    end.

-spec fetch_manual_presence_doc(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok' | 'not_found'.
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

-spec manual_presence_resp(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> 'ok' | 'not_found'.
manual_presence_resp(Username, Realm, JObj) ->
    PresenceId = <<Username/binary, "@", Realm/binary>>,
    case kz_json:get_value(PresenceId, JObj) of
        'undefined' -> 'not_found';
        State -> kapps_call_command:presence(State, PresenceId)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec presence_mwi_query(kz_json:object(), kz_term:proplist()) -> 'ok'.
presence_mwi_query(JObj, _Props) ->
    'true' = kapi_presence:mwi_query_v(JObj),
    _ = kz_log:put_callid(JObj),
    mwi_query(JObj).

-spec notification_register(kz_json:object(), kz_term:proplist()) -> 'ok'.
notification_register(JObj, _Props) ->
    'true' = kapi_notifications:register_v(JObj),
    _ = kz_log:put_callid(JObj),
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

-spec maybe_vm_mwi_resp(kz_term:api_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
maybe_vm_mwi_resp('undefined', _Realm, _AccountDb, _JObj) -> 'ok';
maybe_vm_mwi_resp(<<_/binary>> = VMNumber, Realm, AccountDb, JObj) ->
    case mailbox(AccountDb, VMNumber) of
        {'ok', Doc} -> kvm_mwi:notify_vmbox(AccountDb, kz_doc:id(Doc));
        {'error', _} -> mwi_resp(VMNumber, Realm, AccountDb, JObj)
    end.

-spec mwi_resp(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
mwi_resp(Username, _Realm, AccountDb, _JObj) ->
    case endpoint_id_by_sip_username(AccountDb, Username) of
        {'ok', EndpointId} -> kvm_mwi:notify_endpoint(AccountDb, EndpointId);
        _Else -> 'ok'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec unsolicited_owner_mwi_update(kz_term:api_binary(), kz_term:api_binary()) ->
          'ok' |
          {'error', atom()} |
          kz_datamgr:data_error().
unsolicited_owner_mwi_update(AccountDb, OwnerId) ->
    kvm_mwi:notify_owner(AccountDb, OwnerId).

-spec unsolicited_endpoint_mwi_update(kz_term:api_binary(), kz_term:api_binary()) ->
          'ok' | {'error', any()}.
unsolicited_endpoint_mwi_update(AccountDb, EndpointId) ->
    kvm_mwi:notify_endpoint(AccountDb, EndpointId).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec alpha_to_dialpad(kz_term:ne_binary()) -> kz_term:ne_binary().
alpha_to_dialpad(Value) ->
    << <<(dialpad_digit(C))>> || <<C>> <= kz_term:to_lower_binary(Value), is_alpha(C) >>.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_alpha(char()) -> boolean().
is_alpha(Char) ->
    Char =< $z
        andalso Char >= $a.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec owner_ids_by_sip_username(kz_term:ne_binary(), kz_term:ne_binary()) ->
          {'ok', kz_term:ne_binaries()} |
          {'error', any()}.
owner_ids_by_sip_username(AccountDb, Username) ->
    case kz_cache:peek_local(?CACHE_NAME, ?SIP_USER_OWNERS_KEY(AccountDb, Username)) of
        {'ok', _}=Ok -> Ok;
        {'error', 'not_found'} ->
            get_owner_ids_by_sip_username(AccountDb, Username)
    end.

-spec get_owner_ids_by_sip_username(kz_term:ne_binary(), kz_term:ne_binary()) ->
          {'ok', kz_term:ne_binaries()} |
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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec endpoint_id_by_sip_username(kz_term:ne_binary(), kz_term:ne_binary()) ->
          {'ok', kz_term:ne_binary()} |
          {'error', 'not_found'}.
endpoint_id_by_sip_username(AccountDb, Username) ->
    case kz_cache:peek_local(?CACHE_NAME, ?SIP_ENDPOINT_ID_KEY(AccountDb, Username)) of
        {'ok', _}=Ok -> Ok;
        {'error', 'not_found'} ->
            get_endpoint_id_by_sip_username(AccountDb, Username)
    end.

-spec get_endpoint_id_by_sip_username(kz_term:ne_binary(), kz_term:ne_binary()) ->
          {'ok', kz_term:ne_binary()} |
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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_operator_callflow(kz_term:ne_binary()) -> {'ok', kz_json:object()} |
          {'error', any()}.
get_operator_callflow(Account) -> get_operator_callflow(Account, 'undefined').

-spec get_operator_callflow(kz_term:ne_binary(), kz_term:api_ne_binary()) -> {'ok', kz_json:object()} |
          {'error', any()}.
get_operator_callflow(Account, 'undefined') -> get_operator_callflow(Account, ?OPERATOR_KEY);
get_operator_callflow(Account, OpNum) ->
    case cf_flow:lookup(OpNum, Account) of
        {'ok', _, 'true'} ->
            lager:warning("unable to find operator callflow in ~s: lookup only returned no_match", [Account]),
            {'error', 'no_match'};
        {'ok', JObj, _} ->
            {'ok', kz_json:get_json_value(<<"flow">>, JObj, kz_json:new())};
        {'error', _R}=E ->
            lager:warning("unable to find operator callflow in ~s: ~p", [Account, _R]),
            E
    end.

%%------------------------------------------------------------------------------
%% @doc Look for children branches to handle the failure replies of
%% certain actions, like {@link cf_offnet} and {@link cf_resources}.
%% @end
%%------------------------------------------------------------------------------
-spec handle_bridge_failure({'fail', kz_json:object()} | kz_term:api_binary(), kapps_call:call()) ->
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

-spec handle_bridge_failure(kz_term:api_binary(), kz_term:api_binary(), kapps_call:call()) ->
          'ok' | 'not_found'.
handle_bridge_failure(Cause, Code, Call) ->
    lager:info("attempting to find failure branch for ~s:~s", [Code, Cause]),
    case (handle_bridge_failure(Cause, Call) =:= 'ok')
        orelse (handle_bridge_failure(Code, Call) =:= 'ok')
    of
        'true' -> 'ok';
        'false' -> 'not_found'
    end.

%%------------------------------------------------------------------------------
%% @doc Send and wait for a call failure cause response.
%% @end
%%------------------------------------------------------------------------------
-spec send_default_response(kz_term:ne_binary(), kapps_call:call()) -> 'ok'.
send_default_response(Cause, Call) ->
    case cf_exe:wildcard_is_empty(Call) of
        'false' -> lager:debug("non-empty wildcard; not sending ~s", [Cause]);
        'true' ->
            case kz_call_response:send_default(Call, Cause) of
                {'error', 'no_response'} ->
                    lager:debug("failed to send default response for ~s", [Cause]);
                {'ok', NoopId} ->
                    _ = kapps_call_command:wait_for_noop(Call, NoopId),
                    lager:debug("sent default response for ~s (~s)", [Cause, NoopId])
            end
    end.

-spec apply_dialplan(kz_term:ne_binary(), kz_term:api_object()) -> kz_term:ne_binary().
apply_dialplan(N, 'undefined') -> N;
apply_dialplan(Number, DialPlan) ->
    case kz_json:get_keys(DialPlan) of
        [] -> Number;
        Regexps -> maybe_apply_dialplan(Regexps, DialPlan, Number)
    end.

-spec maybe_apply_dialplan(kz_json:path(), kz_json:object(), kz_term:ne_binary()) -> kz_term:ne_binary().
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

-spec apply_dialplan(kz_term:ne_binary(), kz_json:path(), kz_json:object(), kz_term:ne_binary()) -> kz_term:ne_binary().
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

-spec load_system_dialplans(kz_term:ne_binaries()) -> kz_json:object().
load_system_dialplans(Names) ->
    LowerNames = [kz_term:to_lower_binary(Name) || Name <- Names],
    Plans = kapps_config:get_all_kvs(<<"dialplans">>),
    lists:foldl(fold_system_dialplans(LowerNames), kz_json:new(), Plans).

-spec fold_system_dialplans(kz_term:ne_binaries()) ->
          fun(({kz_term:ne_binary(), kz_json:object()}, kz_json:object()) -> kz_json:object()).
fold_system_dialplans(Names) ->
    fun({Key, Val}, Acc) when is_list(Val) ->
            lists:foldl(fun(ValElem, A) -> maybe_dialplan_suits({Key, ValElem}, A, Names) end, Acc, Val);
       ({Key, Val}, Acc) ->
            maybe_dialplan_suits({Key, Val}, Acc, Names)
    end.

-spec maybe_dialplan_suits({kz_term:ne_binary(), kz_json:object()} ,kz_json:object(), kz_term:ne_binaries()) -> kz_json:object().
maybe_dialplan_suits({Key, Val}=KV, Acc, Names) ->
    Name = kz_term:to_lower_binary(kz_json:get_value(<<"name">>, Val)),
    case lists:member(Name, Names) of
        'true' -> kz_json:set_value(Key, Val, Acc);
        'false' -> maybe_system_dialplan_name(KV, Acc, Names)
    end.

-spec maybe_system_dialplan_name({kz_term:ne_binary(), kz_json:object()} ,kz_json:object(), kz_term:ne_binaries()) -> kz_json:object().
maybe_system_dialplan_name({Key, Val}, Acc, Names) ->
    Name = kz_term:to_lower_binary(Key),
    case lists:member(Name, Names) of
        'true' ->
            N = kz_term:to_binary(index_of(Name, Names)),
            kz_json:set_value(<<N/binary, "-", Key/binary>>, Val, Acc);
        'false' -> Acc
    end.

-spec index_of(kz_term:ne_binary(), list()) -> kz_term:api_integer().
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

-spec event_listener_name(kapps_call:call(), atom() | kz_term:ne_binary()) -> kz_term:ne_binary().
event_listener_name(Call, Module) ->
    <<(kapps_call:call_id_direct(Call))/binary, "-", (kz_term:to_binary(Module))/binary>>.

-spec caller_belongs_to_group(kz_term:ne_binary(), kapps_call:call()) -> boolean().
caller_belongs_to_group(GroupId, Call) ->
    maybe_belongs_to_group(kapps_call:authorizing_id(Call), GroupId, Call).

-spec maybe_belongs_to_group(kz_term:ne_binary(), kz_term:ne_binary(), kapps_call:call()) -> boolean().
maybe_belongs_to_group(TargetId, GroupId, Call) ->
    lists:member(TargetId, find_group_endpoints(GroupId, Call)).

-spec caller_belongs_to_user(kz_term:ne_binary(), kapps_call:call()) -> boolean().
caller_belongs_to_user(UserId, Call) ->
    lists:member(kapps_call:authorizing_id(Call), find_user_endpoints([UserId],[],Call)).

-spec find_group_endpoints(kz_term:ne_binary(), kapps_call:call()) -> kz_term:ne_binaries().
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

-spec find_endpoints(kz_term:ne_binaries(), kz_json:object(), kapps_call:call()) ->
          kz_term:ne_binaries().
find_endpoints(Ids, GroupEndpoints, Call) ->
    {DeviceIds, UserIds} =
        lists:partition(fun(Id) ->
                                kz_json:get_value([Id, <<"type">>], GroupEndpoints) =:= <<"device">>
                        end, Ids),
    find_user_endpoints(UserIds, lists:sort(DeviceIds), Call).

-spec find_user_endpoints(kz_term:ne_binaries(), kz_term:ne_binaries(), kapps_call:call()) ->
          kz_term:ne_binaries().
find_user_endpoints([], DeviceIds, _) -> DeviceIds;
find_user_endpoints(UserIds, DeviceIds, Call) ->
    UserDeviceIds = kz_attributes:owned_by(UserIds, <<"device">>, Call),
    lists:merge(lists:sort(UserDeviceIds), DeviceIds).

-spec find_channels(kz_term:ne_binaries(), kapps_call:call()) -> kz_json:objects().
find_channels(Usernames, Call) ->
    Realm = kzd_accounts:fetch_realm(kapps_call:account_id(Call)),
    lager:debug("finding channels for realm ~p, usernames ~p", [Realm, Usernames]),
    Req = [{<<"Realm">>, Realm}
          ,{<<"Usernames">>, Usernames}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case kz_amqp_worker:call(Req
                            ,fun kapi_call:publish_query_user_channels_req/1
                            ,fun kapi_call:query_user_channels_resp_v/1
                            )
    of
        {'ok', Resp} -> kz_json:get_value(<<"Channels">>, Resp, []);
        {'error', _E} ->
            lager:debug("failed to get channels: ~p", [_E]),
            []
    end.

-spec check_value_of_fields(kz_term:proplist(), boolean(), kz_json:object(), kapps_call:call()) ->
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

-spec sip_users_from_device_ids(kz_term:ne_binaries(), kapps_call:call()) -> kz_term:ne_binaries().
sip_users_from_device_ids(EndpointIds, Call) ->
    lists:foldl(fun(EID, Acc) -> sip_users_from_device_id(EID, Acc, Call) end
               ,[]
               ,EndpointIds
               ).

-spec sip_users_from_device_id(kz_term:ne_binary(), kz_term:ne_binaries(), kapps_call:call()) ->
          kz_term:ne_binaries().
sip_users_from_device_id(EndpointId, Acc, Call) ->
    case sip_user_from_device_id(EndpointId, Call) of
        'undefined' -> Acc;
        Username -> [Username|Acc]
    end.

-spec sip_user_from_device_id(kz_term:ne_binary(), kapps_call:call()) -> kz_term:api_binary().
sip_user_from_device_id(EndpointId, Call) ->
    case kz_endpoint:get(EndpointId, Call) of
        {'error', _} -> 'undefined';
        {'ok', Endpoint} ->
            kzd_devices:sip_username(Endpoint)
    end.

-spec wait_for_noop(kapps_call:call(), kz_term:ne_binary()) ->
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

-spec process_event(kapps_call:call(), kz_term:ne_binary(), kz_json:object()) ->
          {'ok', kapps_call:call()} |
          {'error', any()}.
process_event(Call, NoopId, JObj) ->
    MsgId = kz_api:msg_id(JObj),
    case kapps_call_command:get_event_type(JObj) of
        {<<"call_event">>, <<"CHANNEL_DESTROY">>, _} ->
            lager:debug("channel was destroyed"),
            {'error', 'channel_hungup'};
        {<<"error">>, _, <<"noop">>} ->
            lager:debug("channel execution error while waiting for ~s: ~s", [NoopId, kz_json:encode(JObj)]),
            {'error', JObj};
        {<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"noop">>}
          when NoopId =:= MsgId ->
            lager:debug("noop ~s received", [NoopId]),
            {'ok', Call};
        {<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"noop">>} ->
            case kz_json:get_ne_binary_value(<<"Application-Response">>, JObj) of
                NoopId ->
                    lager:debug("noop ~s received", [NoopId]),
                    {'ok', Call};
                _Resp ->
                    lager:debug("ignoring noop ~s(~s) (waiting for ~s)", [MsgId, _Resp, NoopId]),
                    wait_for_noop(Call, NoopId)
            end;
        {<<"call_event">>, <<"DTMF">>, _} ->
            DTMF = kz_json:get_value(<<"DTMF-Digit">>, JObj),
            lager:debug("recv DTMF ~s, adding to default", [DTMF]),
            Call1 = kapps_call:add_to_dtmf_collection(DTMF, Call),
            cf_exe:set_call(Call1),
            wait_for_noop(Call1, NoopId);
        _Ignore ->
            wait_for_noop(Call, NoopId)
    end.

-spec get_timezone(kz_json:object(), kapps_call:call()) -> kz_term:ne_binary().
get_timezone(JObj, Call) ->
    case kz_json:get_ne_binary_value(<<"timezone">>, JObj) of
        'undefined'   -> kzd_accounts:timezone(kapps_call:account_id(Call));
        <<"inherit">> -> kzd_accounts:timezone(kapps_call:account_id(Call)); %% UI-1808
        TZ -> TZ
    end.

-spec start_task(fun(), list(), kapps_call:call()) -> 'ok'.
start_task(Fun, Args, Call) ->
    SpawnInfo = {'cf_task', [Fun, Args]},
    cf_exe:add_event_listener(Call, SpawnInfo).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec mailbox(kz_term:ne_binary(), kz_term:ne_binary()) -> {'ok', kz_json:object()} |
          {'error', any()}.
mailbox(AccountDb, VMNumber) ->
    try kz_term:to_integer(VMNumber) of
        Number -> maybe_cached_mailbox(AccountDb, Number)
    catch
        _E:_R ->  {'error', 'not_found'}
    end.

-spec maybe_cached_mailbox(kz_term:ne_binary(), integer()) -> {'ok', kz_json:object()} |
          {'error', any()}.
maybe_cached_mailbox(AccountDb, VMNumber) ->
    case kz_cache:peek_local(?CACHE_NAME, ?VM_CACHE_KEY(AccountDb, VMNumber)) of
        {'ok', _}=Ok -> Ok;
        {'error', 'not_found'} -> get_mailbox(AccountDb, VMNumber)
    end.

-spec get_mailbox(kz_term:ne_binary(), integer()) -> {'ok', kz_json:object()} |
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

-spec flush_control_queue(kapps_call:call()) -> 'ok'.
flush_control_queue(Call) ->
    ControlQueue = kapps_call:control_queue_direct(Call),
    CallId = kapps_call:call_id_direct(Call),

    NoopId = kz_datamgr:get_uuid(),
    Command = [{<<"Application-Name">>, <<"noop">>}
              ,{<<"Msg-ID">>, NoopId}
              ,{<<"Insert-At">>, <<"flush">>}
              ,{<<"Call-ID">>, CallId}
               | kz_api:default_headers(<<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    lager:debug("flushing with ~p", [Command]),
    kz_amqp_worker:cast(Command, fun(C) -> kapi_dialplan:publish_command(ControlQueue, C) end).

%% @equiv normalize_capture_group(CaptureGroup, 'undefined')
-spec normalize_capture_group(kz_term:api_binary()) -> kz_term:api_ne_binary().
normalize_capture_group(CaptureGroup) ->
    normalize_capture_group(CaptureGroup, 'undefined').

%%------------------------------------------------------------------------------
%% @doc Normalize CaptureGroup number.
%%
%% If a module is using capture group as destination number, it should normalize
%% the number before continue/branch callflow or lookup callflow for the number.
%%
%% @param CaptureGroup the capture group number.
%% @param Call {@link kapps_call:call()} object or Account ID or undefined
%% to use system default normalizer.
%% @end
%%------------------------------------------------------------------------------
-spec normalize_capture_group(kz_term:api_binary(), kapps_call:call() | kapps_call:api_ne_binary()) -> kz_term:api_ne_binary().
normalize_capture_group('undefined', _) ->
    'undefined';
normalize_capture_group(<<>>, _) ->
    'undefined';
normalize_capture_group(CaptureGroup, 'undefined') ->
    knm_converters:normalize(CaptureGroup);
normalize_capture_group(CaptureGroup, <<AccountId/binary>>) ->
    knm_converters:normalize(CaptureGroup, AccountId);
normalize_capture_group(CaptureGroup, Call) ->
    normalize_capture_group(CaptureGroup, kapps_call:account_id(Call)).
