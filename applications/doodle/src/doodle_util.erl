%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @author Luis Azedo
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(doodle_util).

-include("doodle.hrl").

-define(SIP_ENDPOINT_ID_KEY(Realm, User)
       ,{?MODULE, 'sip_endpoint_id', Realm, User}
       ).
-define(SIP_ENDPOINT_KEY(Realm, User)
       ,{?MODULE, 'sip_endpoint', Realm, User}
       ).
-define(DEFAULT_INCEPTION, <<"offnet">>).
-define(MDN_VIEW, <<"mobile/listing_by_mdn">>).
-define(CONVERT_MDN, 'true').

-define(RESCHEDULE_COUNTERS, [<<"attempts">>, <<"total_attempts">>]).

-export([endpoint_id_from_sipdb/2, get_endpoint_id_from_sipdb/2]).
-export([endpoint_from_sipdb/2, get_endpoint_from_sipdb/2]).
-export([save_sms/1, save_sms/2]).
-export([replay_sms/2]).
-export([get_sms_body/1, set_sms_body/2]).
-export([set_flow_status/2, set_flow_status/3]).
-export([set_flow_error/2, set_flow_error/3, clear_flow_error/1]).
-export([handle_bridge_failure/2, handle_bridge_failure/3]).
-export([sms_status/1, sms_status/2]).
-export([get_callee_id/2 , set_callee_id/2]).
-export([get_caller_id/2 , get_caller_id/3]).
-export([set_caller_id/2, set_caller_id/3]).
-export([get_inbound_destination/1]).
-export([lookup_mdn/1]).
-export([maybe_reschedule_sms/1, maybe_reschedule_sms/2, maybe_reschedule_sms/3]).

%%==============================================================================
%% API functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_sms_body(kz_term:ne_binary(), kapps_call:call()) -> kapps_call:call().
set_sms_body(Body, Call) ->
    kapps_call:kvs_store(<<"Body">>, Body, Call).

-spec get_sms_body(kapps_call:call()) -> kz_term:ne_binary().
get_sms_body(Call) ->
    kapps_call:kvs_fetch(<<"Body">>, Call).

-spec set_flow_status(kz_term:ne_binary() | {binary(), binary()}, kapps_call:call()) -> kapps_call:call().
set_flow_status({Status, Message}, Call) ->
    Props = [{<<"flow_status">>, Status}
            ,{<<"flow_message">>, Message}
            ],
    kapps_call:kvs_store_proplist(Props, Call);
set_flow_status(Status, Call) ->
    kapps_call:kvs_store(<<"flow_status">>, Status, Call).

-spec set_flow_status(kz_term:ne_binary(), kz_term:ne_binary(), kapps_call:call()) -> kapps_call:call().
set_flow_status(Status, Message, Call) ->
    Props = [{<<"flow_status">>, Status}
            ,{<<"flow_message">>, Message}
            ],
    kapps_call:kvs_store_proplist(Props, Call).

-spec set_flow_error(kz_term:api_binary() | {binary(), binary()}, kapps_call:call()) -> kapps_call:call().
set_flow_error({Status, Error}, Call) ->
    Props = [{<<"flow_status">>, Status}
            ,{<<"flow_error">>, Error}
            ],
    kapps_call:kvs_store_proplist(Props, Call);
set_flow_error(Error, Call) ->
    set_flow_error(<<"pending">>, Error, Call).

-spec set_flow_error(kz_term:ne_binary(), kz_term:api_binary(), kapps_call:call()) -> kapps_call:call().
set_flow_error(Status, Error, Call) ->
    Props = [{<<"flow_status">>, Status}
            ,{<<"flow_error">>, Error}
            ],
    kapps_call:kvs_store_proplist(Props, Call).

-spec clear_flow_error(kapps_call:call()) -> kapps_call:call().
clear_flow_error(Call) ->
    Props = [<<"flow_status">>, <<"flow_error">>],
    kapps_call:kvs_erase(Props, Call).

-spec get_sms_revision(kapps_call:call()) -> kz_term:api_binary().
get_sms_revision(Call) ->
    case kapps_call:kvs_fetch(<<"_rev">>, Call) of
        'undefined' -> kapps_call:custom_channel_var(<<"Doc-Revision">>, Call);
        Rev -> Rev
    end.

-spec set_sms_revision(kz_term:api_binary(), kapps_call:call()) -> kapps_call:call().
set_sms_revision(Rev, Call) ->
    kapps_call:kvs_store(<<"_rev">>, Rev, Call).

-spec save_sms(kapps_call:call()) -> kapps_call:call().
save_sms(Call) ->
    Id = kapps_call:kvs_fetch('sms_docid', kapps_call:custom_channel_var(<<"Doc-ID">>, Call), Call),
    save_sms(kz_json:new(), Id, Call).

-spec save_sms(kz_json:object(), kapps_call:call()) -> kapps_call:call().
save_sms(JObj, Call) ->
    Id = kapps_call:kvs_fetch('sms_docid', kapps_call:custom_channel_var(<<"Doc-ID">>, Call), Call),
    save_sms(JObj, Id, Call).

-spec save_sms(kz_json:object(), kz_term:api_binary(), kapps_call:call()) -> kapps_call:call().
save_sms(JObj, 'undefined', Call) ->
    {Year, Month, _} = erlang:date(),
    SmsDocId = kz_term:to_binary(
                 io_lib:format("~B~s-~s",
                               [Year
                               ,kz_date:pad_month(Month)
                               ,kapps_call:call_id(Call)
                               ])
                ),
    UpdatedCall = kapps_call:kvs_store('sms_docid', SmsDocId, Call),
    Doc = kz_doc:set_created(kz_json:new(), kz_time:now_s()),
    save_sms(JObj, SmsDocId, Doc, UpdatedCall);
save_sms(JObj, DocId, Call) ->
    AccountId = kapps_call:account_id(Call),
    ?MATCH_MODB_PREFIX(Year,Month,_) = DocId,
    {'ok', Doc} = kazoo_modb:open_doc(AccountId, DocId, Year, Month),
    save_sms(JObj, DocId, Doc, Call).

-spec save_sms(kz_json:object(), kz_term:api_binary(), kz_json:object(), kapps_call:call()) ->
                      kapps_call:call().
save_sms(JObj, ?MATCH_MODB_PREFIX(Year,Month,_) = DocId, Doc, Call) ->
    AccountId = kapps_call:account_id(Call),
    AccountMODB = kazoo_modb:get_modb(AccountId, Year, Month),
    OwnerId = kapps_call:owner_id(Call),
    AuthType = kapps_call:authorizing_type(Call),
    AuthId = kapps_call:authorizing_id(Call),
    Body = get_sms_body(Call),
    Bits = bit_size(Body),
    To = kapps_call:to(Call),
    From = kapps_call:from(Call),
    Request = kapps_call:request(Call),
    [ToUser, ToRealm] = binary:split(To, <<"@">>),
    [FromUser, FromRealm] = binary:split(From, <<"@">>),
    [RequestUser, RequestRealm] = binary:split(Request, <<"@">>),
    MessageId = kz_json:get_value(<<"Message-ID">>, JObj),
    Rev = get_sms_revision(Call),
    Opts = props:filter_undefined([{'rev', Rev}]),
    Created = kz_doc:created(JObj, kz_time:now_s()),
    Modified = kz_time:now_s(),
    Status = kapps_call:kvs_fetch(<<"flow_status">>, <<"queued">>, Call),
    Schedule = kapps_call:kvs_fetch(<<"flow_schedule">>, Call),
    Props = props:filter_empty(
              [{<<"_id">>, DocId}
              ,{<<"pvt_type">>, <<"sms">>}
              ,{<<"account_id">>, AccountId}
              ,{<<"pvt_account_id">>, AccountId}
              ,{<<"pvt_account_db">>, AccountMODB}
              ,{<<"owner_id">>, OwnerId}
              ,{<<"pvt_owner_id">>, OwnerId}
              ,{<<"authorization_type">>, AuthType}
              ,{<<"authorization_id">>, AuthId}
              ,{<<"pvt_authorization_type">>, AuthType}
              ,{<<"pvt_authorization_id">>, AuthId}
              ,{<<"pvt_target_device_id">>, kapps_call:kvs_fetch(<<"target_device_id">>, Call)}
              ,{<<"pvt_target_owner_id">>, kapps_call:kvs_fetch(<<"target_owner_id">>, Call)}
              ,{<<"to">>, To}
              ,{<<"to_user">>, ToUser}
              ,{<<"to_realm">>, ToRealm}
              ,{<<"from">>, From}
              ,{<<"from_user">>, FromUser}
              ,{<<"from_realm">>, FromRealm}
              ,{<<"request">>, Request}
              ,{<<"request_user">>, RequestUser}
              ,{<<"request_realm">>, RequestRealm}
              ,{<<"body">>, Body}
              ,{<<"bits">>, Bits}
              ,{<<"message_id">>, MessageId}
              ,{<<"pvt_created">>, Created}
              ,{<<"pvt_modified">>, Modified}
              ,{<<"pvt_schedule">>, Schedule}
              ,{<<"pvt_status">>, Status}
              ,{<<"call_id">>, kapps_call:call_id_direct(Call)}
              ,{<<"pvt_call">>, kapps_call:to_json(remove_keys(Call))}
              ,{<<"_rev">>, Rev}
              ]),
    JObjDoc = kz_json:set_values(Props, Doc),
    case kazoo_modb:save_doc(AccountMODB, JObjDoc, Opts) of
        {'ok', Saved} ->
            kapps_call:kvs_store(<<"_rev">>, kz_doc:revision(Saved), Call);
        {'error', E} ->
            lager:error("error saving sms doc : ~p", [E]),
            Call
    end.

-define(REMOVE_KEYS, [<<"_rev">>, <<"flow_schedule">>]).

-spec remove_keys(kapps_call:call()) -> kapps_call:call().
remove_keys(Call) ->
    remove_keys(Call, ?REMOVE_KEYS).

-spec remove_keys(kapps_call:call(), list()) -> kapps_call:call().
remove_keys(Call, Keys) ->
    lists:foldl(fun kapps_call:kvs_erase/2, Call, Keys).

-spec endpoint_id_from_sipdb(kz_term:ne_binary(), kz_term:ne_binary()) ->
                                    {'ok', kz_term:ne_binary()} |
                                    {'error', any()}.
endpoint_id_from_sipdb(Realm, Username) ->
    case kz_cache:peek_local(?CACHE_NAME, ?SIP_ENDPOINT_ID_KEY(Realm, Username)) of
        {'ok', _}=Ok -> Ok;
        {'error', 'not_found'} ->
            get_endpoint_id_from_sipdb(Realm, Username)
    end.

-spec get_endpoint_id_from_sipdb(kz_term:ne_binary(), kz_term:ne_binary()) ->
                                        {'ok', kz_term:ne_binary(), kz_term:ne_binary()} |
                                        {'error', any()}.
get_endpoint_id_from_sipdb(Realm, Username) ->
    ViewOptions = [{'key', [kz_term:to_lower_binary(Realm)
                           ,kz_term:to_lower_binary(Username)
                           ]
                   }],
    case kz_datamgr:get_single_result(?KZ_SIP_DB, <<"credentials/lookup">>, ViewOptions) of
        {'ok', JObj} ->
            EndpointId = kz_doc:id(JObj),
            AccountDb = kz_json:get_value([<<"value">>, <<"account_db">>], JObj),
            CacheProps = [{'origin', {'db', ?KZ_SIP_DB, EndpointId}}],
            kz_cache:store_local(?CACHE_NAME, ?SIP_ENDPOINT_ID_KEY(Realm, Username), {AccountDb, EndpointId}, CacheProps),
            {'ok', EndpointId};
        {'error', _R}=E ->
            lager:warning("unable to lookup sip username ~s for owner ids: ~p", [Username, _R]),
            E
    end.

-spec endpoint_from_sipdb(kz_term:ne_binary(), kz_term:ne_binary()) ->
                                 {'ok', kz_json:object()} |
                                 {'error', any()}.
endpoint_from_sipdb(Realm, Username) ->
    case kz_cache:peek_local(?CACHE_NAME, ?SIP_ENDPOINT_KEY(Realm, Username)) of
        {'ok', _}=Ok -> Ok;
        {'error', 'not_found'} ->
            get_endpoint_from_sipdb(Realm, Username)
    end.

-spec get_endpoint_from_sipdb(kz_term:ne_binary(), kz_term:ne_binary()) ->
                                     {'ok', kz_json:object()} |
                                     {'error', any()}.
get_endpoint_from_sipdb(Realm, Username) ->
    ViewOptions = [{'key', [kz_term:to_lower_binary(Realm)
                           ,kz_term:to_lower_binary(Username)
                           ]
                   }
                  ,'include_docs'
                  ],
    case kz_datamgr:get_single_result(?KZ_SIP_DB, <<"credentials/lookup">>, ViewOptions) of
        {'ok', JObj} ->
            EndpointId = kz_doc:id(JObj),
            CacheProps = [{'origin', {'db', ?KZ_SIP_DB, EndpointId}}],
            Doc = kz_json:get_value(<<"doc">>, JObj),
            kz_cache:store_local(?CACHE_NAME, ?SIP_ENDPOINT_ID_KEY(Realm, Username), Doc, CacheProps),
            {'ok', Doc};
        {'error', _R}=E ->
            lager:warning("lookup sip username ~s in sipdb failed: ~p", [Username, _R]),
            E
    end.

-spec replay_sms(kz_term:ne_binary(), kz_term:ne_binary()) -> any().
replay_sms(AccountId, DocId) ->
    lager:debug("trying to replay sms ~s for account ~s",[DocId, AccountId]),
    {'ok', Doc} = kazoo_modb:open_doc(AccountId, DocId),
    Flow = kz_json:get_value(<<"pvt_call">>, Doc),
    Schedule = kz_json:get_value(<<"pvt_schedule">>, Doc),
    Rev = kz_doc:revision(Doc),
    replay_sms_flow(AccountId, DocId, Rev, Flow, Schedule).

-spec replay_sms_flow(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_object(), kz_term:api_object()) -> any().
replay_sms_flow(_AccountId, _DocId, _Rev, 'undefined', _) -> 'ok';
replay_sms_flow(AccountId, <<_:7/binary, CallId/binary>> = DocId, Rev, JObj, Schedule) ->
    lager:debug("replaying sms ~s for account ~s",[DocId, AccountId]),
    Routines = [{fun kapps_call:set_call_id/2, CallId}
               ,fun(C) -> set_sms_revision(Rev, C) end
               ,fun(C) -> set_flow_status(<<"resumed">>, C) end
               ,{fun kapps_call:kvs_store/3, <<"flow_schedule">>, Schedule}
               ],

    Call = kapps_call:exec(Routines, kapps_call:from_json(JObj)),
    kapps_call:put_callid(Call),
    lager:info("doodle received sms resume for ~s of account ~s, taking control", [DocId, AccountId]),
    doodle_route_win:execute_text_flow(JObj, Call).

-spec sms_status(kz_term:api_object()) -> kz_term:ne_binary().
sms_status('undefined') -> <<"pending">>;
sms_status(JObj) ->
    DeliveryCode = kz_json:get_value(<<"Delivery-Result-Code">>, JObj),
    Status = kz_json:get_value(<<"Status">>, JObj),
    sms_status(DeliveryCode, Status).

-spec sms_status(kz_term:api_binary(), kz_term:api_binary()) -> kz_term:ne_binary().
sms_status(<<"sip:", Code/binary>>, Status) -> sms_status(Code, Status);
sms_status(<<"200">>, _) -> <<"delivered">>;
sms_status(<<"202">>, _) -> <<"accepted">>;
sms_status(_, <<"Success">>) -> <<"completed">>;
sms_status(_, _) -> <<"pending">>.

%%------------------------------------------------------------------------------
%% @doc Look for children branches to handle the failure replies of
%% certain actions, like cf_sms_offnet and cf_sms_resources
%% @end
%%------------------------------------------------------------------------------
-spec handle_bridge_failure({'fail' | 'error', kz_json:object() | atom()} | kz_term:api_binary(), kapps_call:call()) ->
                                   'ok' | 'not_found'.
handle_bridge_failure({'fail', Reason}, Call) ->
    {Cause, Code} = kapps_util:get_call_termination_reason(Reason),
    handle_bridge_failure(Cause, Code, Call);
handle_bridge_failure('undefined', _) ->
    'not_found';
handle_bridge_failure(<<_/binary>> = Failure, Call) ->
    case doodle_exe:attempt(Failure, Call) of
        {'attempt_resp', 'ok'} ->
            lager:info("found child branch to handle failure: ~s", [Failure]),
            'ok';
        {'attempt_resp', _} ->
            'not_found'
    end;
handle_bridge_failure(_, _Call) -> 'not_found'.

-spec handle_bridge_failure(kz_term:api_binary(), kz_term:api_binary(), kapps_call:call()) ->
                                   'ok' | 'not_found'.
handle_bridge_failure(Cause, Code, Call) ->
    lager:info("attempting to find failure branch for ~s:~s", [Code, Cause]),
    case (handle_bridge_failure(Cause, Call) =:= 'ok')
        orelse (handle_bridge_failure(Code, Call) =:= 'ok') of
        'true' -> 'ok';
        'false' -> 'not_found'
    end.

-spec get_caller_id(kz_json:object(), kapps_call:call()) -> {kz_term:api_binary(), kz_term:api_binary()}.
get_caller_id(Data, Call) ->
    get_caller_id(Data, <<"external">>, Call).

-spec get_caller_id(kz_json:object(), binary(), kapps_call:call()) -> {kz_term:api_binary(), kz_term:api_binary()}.
get_caller_id(Data, Default, Call) ->
    Type = kz_json:get_value(<<"caller_id_type">>, Data, Default),
    kz_attributes:caller_id(Type, Call).

-spec set_caller_id(kz_json:object() | binary(), kapps_call:call()) -> kapps_call:call().
set_caller_id(CIDNumber, Call)
  when is_binary(CIDNumber) ->
    set_caller_id(CIDNumber, CIDNumber, Call);
set_caller_id(Data, Call) ->
    {CIDNumber, CIDName} = get_caller_id(Data, Call),
    set_caller_id(CIDNumber, CIDName, Call).

-spec set_caller_id(binary(), binary(), kapps_call:call()) -> kapps_call:call().
set_caller_id(CIDNumber, CIDName, Call) ->
    Props = props:filter_empty(
              [{<<"Caller-ID-Name">>, CIDName}
              ,{<<"Caller-ID-Number">>, CIDNumber}
              ]),
    Routines = [{fun kapps_call:set_caller_id_number/2, CIDNumber}
               ,{fun kapps_call:set_caller_id_name/2, CIDName}
               ,{fun kapps_call:set_custom_channel_vars/2, Props}
               ],
    kapps_call:exec(Routines, Call).

-spec get_callee_id(binary(), kapps_call:call()) -> {kz_term:api_binary(), kz_term:api_binary()}.
get_callee_id(EndpointId, Call) ->
    kz_attributes:callee_id(EndpointId, Call).

-spec set_callee_id(binary(), kapps_call:call()) -> kapps_call:call().
set_callee_id(EndpointId, Call) ->
    {CIDNumber, CIDName} = get_callee_id(EndpointId, Call),
    Props = props:filter_empty(
              [{<<"Callee-ID-Name">>, CIDName}
              ,{<<"Callee-ID-Number">>, CIDNumber}
              ]),
    kapps_call:set_custom_channel_vars(Props, Call).

-spec get_inbound_field(kz_term:ne_binary()) -> kz_term:ne_binaries().
get_inbound_field(Inception) ->
    case Inception of
        <<"onnet">> -> [<<"Caller-ID-Number">>, <<"From">>];
        <<"offnet">> -> [<<"Callee-ID-Number">>, <<"To">>];
        _ -> get_inbound_field(?DEFAULT_INCEPTION)
    end.

-spec get_inbound_destination(kz_json:object()) -> {kz_term:ne_binary(), kz_term:ne_binary()}.
get_inbound_destination(JObj) ->
    Inception = kz_json:get_value(<<"Route-Type">>, JObj, ?DEFAULT_INCEPTION),
    Keys = get_inbound_field(Inception),
    Number = kz_json:get_first_defined(Keys, JObj),
    {knm_converters:normalize(Number), Inception}.

-spec lookup_mdn(kz_term:ne_binary()) ->
                        {'ok', kz_term:ne_binary(), kz_term:api_binary()} |
                        {'error', any()}.
lookup_mdn(Number) ->
    Num = knm_converters:normalize(Number),
    case kz_cache:fetch_local(?CACHE_NAME, cache_key_mdn(Num)) of
        {'ok', {Id, OwnerId}} ->
            lager:debug("cached number ~s is associated with ~s/~s", [Num, OwnerId, Id]),
            {'ok', Id, OwnerId};
        {'error', 'not_found'} -> fetch_mdn(Num)
    end.

-spec fetch_mdn(kz_term:ne_binary()) ->
                       {'ok', kz_term:ne_binary(), kz_term:api_binary()} |
                       {'error', any()}.
fetch_mdn(Num) ->
    case knm_number:lookup_account(Num) of
        {'ok', AccountId, _Props} ->
            fetch_mdn_result(AccountId, Num);
        {'error', Reason}=E ->
            lager:debug("~s is not associated with any account, ~p", [Num, Reason]),
            E
    end.

-spec fetch_mdn_result(kz_term:ne_binary(), kz_term:ne_binary()) ->
                              {'ok', kz_term:ne_binary(), kz_term:api_binary()} |
                              {'error', 'not_found'}.
fetch_mdn_result(AccountId, Num) ->
    AccountDb = kz_util:format_account_db(AccountId),
    ViewOptions = [{'key', mdn_from_e164(Num)}],
    case kz_datamgr:get_single_result(AccountDb, ?MDN_VIEW, ViewOptions) of
        {'ok', JObj} ->
            Id = kz_doc:id(JObj),
            OwnerId = kz_json:get_value([<<"value">>, <<"owner_id">>], JObj),
            lager:debug("~s is associated with mobile device ~s in account ~s", [Num, Id, AccountId]),
            cache_mdn_result(AccountDb, Id, OwnerId);
        {'error', _R}=E ->
            lager:debug("could not fetch mdn for ~p: ~p", [Num, _R]),
            E
    end.

-spec cache_mdn_result(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_binary()) ->
                              {'ok', kz_term:ne_binary(), kz_term:api_binary()}.
cache_mdn_result(AccountDb, Id, OwnerId) ->
    CacheProps = [{'origin', [{'db', AccountDb, Id}]}],
    kz_cache:store_local(?CACHE_NAME, cache_key_mdn(Id), {Id, OwnerId}, CacheProps),
    {'ok', Id, OwnerId}.

-spec cache_key_mdn(kz_term:ne_binary()) -> {'sms_mdn', kz_term:ne_binary()}.
cache_key_mdn(Number) ->
    {'sms_mdn', Number}.

%% TODO
%% MDN should be stored in e164 format
-spec mdn_from_e164(binary()) -> binary().
mdn_from_e164(<<"+1", Number/binary>>) -> Number;
mdn_from_e164(<<"1", Number/binary>>) -> Number;
mdn_from_e164(Number) -> Number.

-spec maybe_reschedule_sms(kapps_call:call()) -> 'ok'.
maybe_reschedule_sms(Call) ->
    maybe_reschedule_sms(<<>>, <<>>, Call).

-spec maybe_reschedule_sms(kz_term:api_binary(), kapps_call:call()) -> 'ok'.
maybe_reschedule_sms(<<"sip:", Code/binary>>, Call) ->
    maybe_reschedule_sms(Code, <<>>, Call);
maybe_reschedule_sms(Code, Call) ->
    maybe_reschedule_sms(Code, <<>>, Call).

-spec maybe_reschedule_sms(kz_term:api_binary(), kz_term:api_binary(), kapps_call:call()) -> 'ok'.
maybe_reschedule_sms(Code, 'undefined', Call) ->
    maybe_reschedule_sms(Code, set_flow_error(<<"unknown error">>, Call));
maybe_reschedule_sms(<<"sip:", Code/binary>>, Message, Call) ->
    maybe_reschedule_sms(Code, Message, Call);
maybe_reschedule_sms(Code, Message, Call) ->
    maybe_reschedule_sms(Code, Message, kapps_call:account_id(Call), set_flow_error(Message, Call)).

-spec maybe_reschedule_sms(kz_term:api_binary(), kz_term:api_binary(), kz_term:ne_binary(), kapps_call:call()) -> 'ok'.
maybe_reschedule_sms(Code, Message, AccountId, Call) ->
    put('call', Call),
    Rules = kapps_account_config:get_global(AccountId, ?CONFIG_CAT, <<"reschedule">>, kz_json:new()),
    Schedule = kz_json:set_values([{<<"code">>, Code}
                                  ,{<<"reason">>, Message}
                                  ]
                                 ,kapps_call:kvs_fetch(<<"flow_schedule">>, kz_json:new(), Call)
                                 ),
    case apply_reschedule_logic(kz_json:get_values(Rules), Schedule) of
        'no_rule' ->
            lager:debug("no rules configured for account-id ~s", [AccountId]),
            doodle_exe:stop(set_flow_status(<<"error">>, Call));
        'end_rules' ->
            lager:debug("end rules configured for account-id ~s", [AccountId]),
            doodle_exe:stop(set_flow_status(<<"error">>,Call));
        NewSchedule ->
            doodle_exe:stop(kapps_call:kvs_store(<<"flow_schedule">>, NewSchedule, Call))
    end.

-spec inc_counters(kz_json:object(), list()) -> kz_json:object().
inc_counters(JObj, Counters) ->
    lists:foldl(fun inc_counter/2, JObj, Counters).

-spec inc_counter(binary(), kz_json:object()) -> kz_json:object().
inc_counter(Key, JObj) ->
    kz_json:set_value(Key, kz_json:get_integer_value(Key, JObj, 0) + 1, JObj).

-spec apply_reschedule_logic({kz_json:json_terms(), kz_json:path()}, kz_json:object()) ->
                                    'no_rule' | 'end_rules' | kz_json:object().
apply_reschedule_logic({[], []}, _JObj) -> 'no_rule';
apply_reschedule_logic(Rules, JObj) ->
    Step = kz_json:get_integer_value(<<"rule">>, JObj, 1),
    apply_reschedule_logic(Rules, inc_counters(JObj, ?RESCHEDULE_COUNTERS), Step).

-spec apply_reschedule_logic({kz_json:json_terms(), kz_json:path()}, kz_json:object(), integer()) ->
                                    'no_rule' | 'end_rules' | kz_json:object().
apply_reschedule_logic({_Vs, Ks}, _JObj, Step)
  when Step > length(Ks) -> 'end_rules';
apply_reschedule_logic({Vs, Ks}, JObj, Step) ->
    Rules = {lists:sublist(Vs, Step, length(Vs)), lists:sublist(Ks, Step, length(Ks))},
    apply_reschedule_rules(Rules, JObj, Step).

-spec apply_reschedule_rules({kz_json:json_terms(), kz_json:path()}, kz_json:object(), integer()) ->
                                    kz_json:object() | 'end_rules'.
apply_reschedule_rules({[], _}, _JObj, _Step) -> 'end_rules';
apply_reschedule_rules({[Rule | Rules], [Key | Keys]}, JObj, Step) ->
    case apply_reschedule_step(kz_json:get_values(Rule), JObj) of
        'no_match' ->
            NewObj = kz_json:set_values(
                       [{<<"rule_start_time">>, kz_time:now_s()}
                       ,{<<"attempts">>, 0}
                       ], JObj),
            apply_reschedule_rules({Rules, Keys}, NewObj, Step+1);
        Schedule -> kz_json:set_values(
                      [{<<"rule">>, Step}
                      ,{<<"rule_name">>, Key}
                      ], Schedule)
    end.

-spec apply_reschedule_step({kz_json:json_terms(), kz_json:path()}, kz_json:object()) ->
                                   'no_match' | kz_json:object().
apply_reschedule_step({[], []}, JObj) -> JObj;
apply_reschedule_step({[Value | Values], [Key | Keys]}, JObj) ->
    case apply_reschedule_rule(Key, Value, JObj) of
        'no_match' -> 'no_match';
        Schedule -> apply_reschedule_step({Values, Keys}, Schedule)
    end.

-spec apply_reschedule_rule(kz_term:ne_binary(), any(), kz_json:object()) -> 'no_match' | kz_json:object().
apply_reschedule_rule(<<"error">>, ErrorObj, JObj) ->
    Codes = kz_json:get_value(<<"code">>, ErrorObj, []),
    XCodes = kz_json:get_value(<<"xcode">>, ErrorObj, []),
    Reasons = kz_json:get_value(<<"reason">>, ErrorObj, []),
    XReasons = kz_json:get_value(<<"xreason">>, ErrorObj, []),
    Code = kz_json:get_value(<<"code">>, JObj, <<>>),
    Reason = kz_json:get_value(<<"reason">>, JObj, <<>>),
    case (lists:member(Code, Codes)
          orelse Codes =:= []
         )
        andalso (lists:member(Reason, Reasons)
                 orelse Reasons =:= []
                )
        andalso ((not lists:member(Code, XCodes))
                 orelse XCodes =:= []
                )
        andalso ((not lists:member(Reason, XReasons))
                 orelse XReasons =:= []
                )
    of
        'true' -> JObj;
        'false' -> 'no_match'
    end;
apply_reschedule_rule(<<"number">>, Value, JObj) ->
    Attempts = kz_json:get_integer_value(<<"attempts">>, JObj, 0),
    case Attempts > Value of
        'true' -> 'no_match';
        'false' -> JObj
    end;
apply_reschedule_rule(<<"time">>, IntervalJObj, JObj) ->
    {[Value], [Key]} = kz_json:get_values(IntervalJObj),
    Start = kz_json:get_value(<<"rule_start_time">>, JObj),
    Until = time_rule(Key, Value, Start),
    Now = kz_time:now_s(),
    case Until > Now of
        'true' -> JObj;
        'false' -> 'no_match'
    end;
apply_reschedule_rule(<<"interval">>, IntervalJObj, JObj) ->
    {[Value], [Key]} = kz_json:get_values(IntervalJObj),
    Next = time_rule(Key, Value, kz_time:now_s()),
    kz_json:set_value(<<"start_time">>, Next, JObj);
apply_reschedule_rule(<<"report">>, V, JObj) ->
    Call = get('call'),
    Error = list_to_binary([kz_json:get_value(<<"code">>, JObj, <<>>)
                           ," "
                           ,kz_json:get_value(<<"reason">>, JObj, <<>>)
                           ]),

    Props = props:filter_undefined(
              [{<<"To">>, kapps_call:to_user(Call)}
              ,{<<"From">>, kapps_call:from_user(Call)}
              ,{<<"Error">>, kz_binary:strip(Error)}
              ,{<<"Attempts">>, kz_json:get_value(<<"attempts">>, JObj)}
               | safe_to_proplist(V)
              ]),
    Notify = [{<<"Subject">>, <<"System Alert: SMS Error">>}
             ,{<<"Message">>, <<"undelivered sms">>}
             ,{<<"Details">>, kz_json:set_values(Props, kz_json:new())}
             ,{<<"Account-ID">>, kapps_call:account_id(Call)}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ],
    _ = kz_amqp_worker:cast(Notify, fun kapi_notifications:publish_system_alert/1),
    JObj;
apply_reschedule_rule(_, _, JObj) -> JObj.

-spec safe_to_proplist(any()) -> kz_term:proplist().
safe_to_proplist(JObj) ->
    safe_to_proplist(kz_json:is_json_object(JObj), JObj).

-spec safe_to_proplist(boolean(), any()) -> kz_term:proplist().
safe_to_proplist('true', JObj) ->
    kz_json:to_proplist(JObj);
safe_to_proplist(_, _) -> [].

-spec time_rule(kz_term:ne_binary(), integer(), integer()) -> integer().
time_rule(<<"week">>, N, Base) -> Base + N * ?SECONDS_IN_WEEK;
time_rule(<<"day">>, N, Base) -> Base + N * ?SECONDS_IN_DAY;
time_rule(<<"hour">>, N, Base) -> Base + N * ?SECONDS_IN_HOUR;
time_rule(<<"minute">>, N, Base) -> Base + N * ?SECONDS_IN_MINUTE;
time_rule(<<"second">>, N, Base) -> Base + N * 1;
time_rule(_, _N, Base) -> Base.
