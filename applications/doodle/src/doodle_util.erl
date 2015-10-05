%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(doodle_util).

-include("doodle.hrl").

-define(SIP_ENDPOINT_ID_KEY(Realm, User)
        ,{?MODULE, 'sip_endpoint_id', Realm, User}
       ).
-define(SIP_ENDPOINT_KEY(Realm, User)
        ,{?MODULE, 'sip_endpoint', Realm, User}
       ).
-define(DEFAULT_INCEPTION, <<"off-net">>).
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
-export([lookup_number/1]).
-export([get_inbound_destination/1]).
-export([lookup_mdn/1]).
-export([maybe_reschedule_sms/1, maybe_reschedule_sms/2, maybe_reschedule_sms/3]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec set_sms_body(ne_binary(), whapps_call:call()) -> whapps_call:call().
set_sms_body(Body, Call) ->
    whapps_call:kvs_store(<<"Body">>, Body, Call).

-spec get_sms_body(whapps_call:call()) -> ne_binary().
get_sms_body(Call) ->
    whapps_call:kvs_fetch(<<"Body">>, Call).

-spec set_flow_status(ne_binary() | {binary(), binary()}, whapps_call:call()) -> whapps_call:call().
set_flow_status({Status, Message}, Call) ->
    Props = [{<<"flow_status">>, Status}
             ,{<<"flow_message">>, Message}
            ],
    whapps_call:kvs_store_proplist(Props, Call);
set_flow_status(Status, Call) ->
    whapps_call:kvs_store(<<"flow_status">>, Status, Call).

-spec set_flow_status(ne_binary(), ne_binary(), whapps_call:call()) -> whapps_call:call().
set_flow_status(Status, Message, Call) ->
    Props = [{<<"flow_status">>, Status}
             ,{<<"flow_message">>, Message}
            ],
    whapps_call:kvs_store_proplist(Props, Call).

-spec set_flow_error(api_binary() | {binary(), binary()}, whapps_call:call()) -> whapps_call:call().
set_flow_error({Status, Error}, Call) ->
    Props = [{<<"flow_status">>, Status}
             ,{<<"flow_error">>, Error}
            ],
    whapps_call:kvs_store_proplist(Props, Call);
set_flow_error(Error, Call) ->
    set_flow_error(<<"pending">>, Error, Call).

-spec set_flow_error(ne_binary(), api_binary(), whapps_call:call()) -> whapps_call:call().
set_flow_error(Status, Error, Call) ->
    Props = [{<<"flow_status">>, Status}
             ,{<<"flow_error">>, Error}
            ],
    whapps_call:kvs_store_proplist(Props, Call).

-spec clear_flow_error(whapps_call:call()) -> whapps_call:call().
clear_flow_error(Call) ->
    Props = [<<"flow_status">>, <<"flow_error">>],
    whapps_call:kvs_erase(Props, Call).

-spec get_sms_revision(whapps_call:call()) -> api_binary().
get_sms_revision(Call) ->
    case whapps_call:kvs_fetch(<<"_rev">>, Call) of
        'undefined' -> whapps_call:custom_channel_var(<<"Doc-Revision">>, Call);
        Rev -> Rev
    end.

-spec set_sms_revision(api_binary(), whapps_call:call()) -> whapps_call:call().
set_sms_revision(Rev, Call) ->
    whapps_call:kvs_store(<<"_rev">>, Rev, Call).

-spec save_sms(whapps_call:call()) -> whapps_call:call().
save_sms(Call) ->
    Id = whapps_call:kvs_fetch('sms_docid', whapps_call:custom_channel_var(<<"Doc-ID">>, Call), Call),
    save_sms(wh_json:new(), Id, Call).

-spec save_sms(wh_json:object(), whapps_call:call()) -> whapps_call:call().
save_sms(JObj, Call) ->
    Id = whapps_call:kvs_fetch('sms_docid', whapps_call:custom_channel_var(<<"Doc-ID">>, Call), Call),
    save_sms(JObj, Id, Call).

-spec save_sms(wh_json:object(), api_binary(), whapps_call:call()) -> whapps_call:call().
save_sms(JObj, 'undefined', Call) ->
    {Year, Month, _} = erlang:date(),
    SmsDocId = wh_util:to_binary(
                 io_lib:format("~B~s-~s",
                               [Year
                                ,wh_util:pad_month(Month)
                                ,whapps_call:call_id(Call)
                               ])
                ),
    UpdatedCall = whapps_call:kvs_store('sms_docid', SmsDocId, Call),
    Doc = wh_doc:set_created(wh_json:new(), wh_util:current_tstamp()),
    save_sms(JObj, SmsDocId, Doc, UpdatedCall);
save_sms(JObj, DocId, Call) ->
    AccountId = whapps_call:account_id(Call),
    <<Year:4/binary, Month:2/binary, "-", _/binary>> = DocId,
    {'ok', Doc} = kazoo_modb:open_doc(AccountId, DocId, Year, Month),
    save_sms(JObj, DocId, Doc, Call).

-spec save_sms(wh_json:object(), api_binary(), wh_json:object(), whapps_call:call()) -> whapps_call:call().
save_sms(JObj, DocId, Doc, Call) ->
    <<Year:4/binary, Month:2/binary, "-", _/binary>> = DocId,
    AccountId = whapps_call:account_id(Call),
    AccountDb = kazoo_modb:get_modb(AccountId, Year, Month),
    OwnerId = whapps_call:owner_id(Call),
    AuthType = whapps_call:authorizing_type(Call),
    AuthId = whapps_call:authorizing_id(Call),
    Body = get_sms_body(Call),
    Bits = bit_size(Body),
    To = whapps_call:to(Call),
    From = whapps_call:from(Call),
    Request = whapps_call:request(Call),
    [ToUser, ToRealm] = binary:split(To, <<"@">>),
    [FromUser, FromRealm] = binary:split(From, <<"@">>),
    [RequestUser, RequestRealm] = binary:split(Request, <<"@">>),
    MessageId = wh_json:get_value(<<"Message-ID">>, JObj),
    Rev = get_sms_revision(Call),
    Opts = props:filter_undefined([{'rev', Rev}]),
    Created = wh_doc:created(JObj, wh_util:current_tstamp()),
    Modified = wh_util:current_tstamp(),
    Status = whapps_call:kvs_fetch(<<"flow_status">>, <<"queued">>, Call),
    Schedule = whapps_call:kvs_fetch(<<"flow_schedule">>, Call),
    Props = props:filter_empty(
              [{<<"_id">>, DocId}
               ,{<<"pvt_type">>, <<"sms">>}
               ,{<<"account_id">>, AccountId}
               ,{<<"pvt_account_id">>, AccountId}
               ,{<<"pvt_account_db">>, AccountDb}
               ,{<<"owner_id">>, OwnerId}
               ,{<<"pvt_owner_id">>, OwnerId}
               ,{<<"authorization_type">>, AuthType}
               ,{<<"authorization_id">>, AuthId}
               ,{<<"pvt_authorization_type">>, AuthType}
               ,{<<"pvt_authorization_id">>, AuthId}
               ,{<<"pvt_target_device_id">>, whapps_call:kvs_fetch(<<"target_device_id">>, Call)}
               ,{<<"pvt_target_owner_id">>, whapps_call:kvs_fetch(<<"target_owner_id">>, Call)}
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
               ,{<<"call_id">>, whapps_call:call_id_direct(Call)}
               ,{<<"pvt_call">>, whapps_call:to_json(remove_keys(Call))}
               ,{<<"_rev">>, Rev}
              ]),
    JObjDoc = wh_json:set_values(Props, Doc),
    kazoo_modb:create(AccountDb),
    case couch_mgr:save_doc(AccountDb, JObjDoc, Opts) of
        {'ok', Saved} ->
            whapps_call:kvs_store(<<"_rev">>, wh_doc:revision(Saved), Call);
        {'error', E} ->
            lager:error("error saving sms doc : ~p", [E]),
            Call
    end.

-define(REMOVE_KEYS, [<<"_rev">>, <<"flow_schedule">>]).

-spec remove_keys(whapps_call:call()) -> whapps_call:call().
remove_keys(Call) ->
    remove_keys(Call, ?REMOVE_KEYS).

-spec remove_keys(whapps_call:call(), list()) -> whapps_call:call().
remove_keys(Call, Keys) ->
    lists:foldl(fun whapps_call:kvs_erase/2, Call, Keys).

-spec endpoint_id_from_sipdb(ne_binary(), ne_binary()) ->
                                    {'ok', ne_binary()} |
                                    {'error', _}.
endpoint_id_from_sipdb(Realm, Username) ->
    case wh_cache:peek_local(?DOODLE_CACHE, ?SIP_ENDPOINT_ID_KEY(Realm, Username)) of
        {'ok', _}=Ok -> Ok;
        {'error', 'not_found'} ->
            get_endpoint_id_from_sipdb(Realm, Username)
    end.

-spec get_endpoint_id_from_sipdb(ne_binary(), ne_binary()) ->
                                        {'ok', ne_binary(), ne_binary()} |
                                        {'error', _}.
get_endpoint_id_from_sipdb(Realm, Username) ->
    ViewOptions = [{'key', [wh_util:to_lower_binary(Realm)
                            ,wh_util:to_lower_binary(Username)
                           ]
                   }],
    case couch_mgr:get_results(?WH_SIP_DB, <<"credentials/lookup">>, ViewOptions) of
        {'ok', [JObj]} ->
            EndpointId = wh_doc:id(JObj),
            AccountDb = wh_json:get_value([<<"value">>, <<"account_db">>], JObj),
            CacheProps = [{'origin', {'db', ?WH_SIP_DB, EndpointId}}],
            wh_cache:store_local(?DOODLE_CACHE, ?SIP_ENDPOINT_ID_KEY(Realm, Username), {AccountDb, EndpointId}, CacheProps),
            {'ok', EndpointId};
        {'ok', []} ->
            lager:debug("sip username ~s not in sip_db", [Username]),
            {'error', 'not_found'};
        {'error', _R}=E ->
            lager:warning("unable to lookup sip username ~s for owner ids: ~p", [Username, _R]),
            E
    end.

-spec endpoint_from_sipdb(ne_binary(), ne_binary()) ->
                                 {'ok', wh_json:object()} |
                                 {'error', _}.
endpoint_from_sipdb(Realm, Username) ->
    case wh_cache:peek_local(?DOODLE_CACHE, ?SIP_ENDPOINT_KEY(Realm, Username)) of
        {'ok', _}=Ok -> Ok;
        {'error', 'not_found'} ->
            get_endpoint_from_sipdb(Realm, Username)
    end.

-spec get_endpoint_from_sipdb(ne_binary(), ne_binary()) ->
                                     {'ok', wh_json:object()} |
                                     {'error', _}.
get_endpoint_from_sipdb(Realm, Username) ->
    ViewOptions = [{'key', [wh_util:to_lower_binary(Realm)
                            ,wh_util:to_lower_binary(Username)
                           ]
                   }
                   ,'include_docs'
                  ],
    case couch_mgr:get_results(?WH_SIP_DB, <<"credentials/lookup">>, ViewOptions) of
        {'ok', [JObj]} ->
            EndpointId = wh_doc:id(JObj),
            CacheProps = [{'origin', {'db', ?WH_SIP_DB, EndpointId}}],
            Doc = wh_json:get_value(<<"doc">>, JObj),
            wh_cache:store_local(?DOODLE_CACHE, ?SIP_ENDPOINT_ID_KEY(Realm, Username), Doc, CacheProps),
            {'ok', Doc};
        {'ok', []} ->
            lager:debug("sip username ~s not in sip_db", [Username]),
            {'error', 'not_found'};
        {'error', _R}=E ->
            lager:warning("unable to lookup sip username ~s in sipdb: ~p", [Username, _R]),
            E
    end.

-spec replay_sms(ne_binary(), ne_binary()) -> _.
replay_sms(AccountId, DocId) ->
    lager:debug("trying to replay sms ~s for account ~s",[DocId, AccountId]),
    {'ok', Doc} = kazoo_modb:open_doc(AccountId, DocId),
    Flow = wh_json:get_value(<<"pvt_call">>, Doc),
    Schedule = wh_json:get_value(<<"pvt_schedule">>, Doc),
    Rev = wh_doc:revision(Doc),
    replay_sms_flow(AccountId, DocId, Rev, Flow, Schedule).

-spec replay_sms_flow(ne_binary(), ne_binary(), ne_binary(), api_object(), api_object()) -> _.
replay_sms_flow(_AccountId, _DocId, _Rev, 'undefined', _) -> 'ok';
replay_sms_flow(AccountId, <<_:7/binary, CallId/binary>> = DocId, Rev, JObj, Schedule) ->
    lager:debug("replaying sms ~s for account ~s",[DocId, AccountId]),
    Routines = [{fun whapps_call:set_call_id/2, CallId}
                ,fun(C) -> set_sms_revision(Rev, C) end
                ,fun(C) -> set_flow_status(<<"resumed">>, C) end
                ,{fun whapps_call:kvs_store/3, <<"flow_schedule">>, Schedule}
               ],

    Call = whapps_call:exec(Routines, whapps_call:from_json(JObj)),
    whapps_call:put_callid(Call),
    lager:info("doodle received sms resume for ~s of account ~s, taking control", [DocId, AccountId]),
    doodle_route_win:maybe_replay_sms(JObj, Call).

-spec sms_status(api_object()) -> ne_binary().
sms_status('undefined') -> <<"pending">>;
sms_status(JObj) ->
    DeliveryCode = wh_json:get_value(<<"Delivery-Result-Code">>, JObj),
    Status = wh_json:get_value(<<"Status">>, JObj),
    sms_status(DeliveryCode, Status).

-spec sms_status(api_binary(), api_binary()) -> ne_binary().
sms_status(<<"sip:", Code/binary>>, Status) -> sms_status(Code, Status);
sms_status(<<"200">>, _) -> <<"delivered">>;
sms_status(<<"202">>, _) -> <<"accepted">>;
sms_status(_, <<"Success">>) -> <<"completed">>;
sms_status(_, _) -> <<"pending">>.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Look for children branches to handle the failure replies of
%% certain actions, like cf_sms_offnet and cf_sms_resources
%% @end
%%--------------------------------------------------------------------
-spec handle_bridge_failure({'fail' | 'error', wh_json:object() | atom()} | api_binary(), whapps_call:call()) ->
                                   'ok' | 'not_found'.
handle_bridge_failure({'fail', Reason}, Call) ->
    {Cause, Code} = whapps_util:get_call_termination_reason(Reason),
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

-spec handle_bridge_failure(api_binary(), api_binary(), whapps_call:call()) ->
                                   'ok' | 'not_found'.
handle_bridge_failure(Cause, Code, Call) ->
    lager:info("attempting to find failure branch for ~s:~s", [Code, Cause]),
    case (handle_bridge_failure(Cause, Call) =:= 'ok')
        orelse (handle_bridge_failure(Code, Call) =:= 'ok') of
        'true' -> 'ok';
        'false' -> 'not_found'
    end.

-spec get_caller_id(wh_json:object(), whapps_call:call()) -> {api_binary(), api_binary()}.
get_caller_id(Data, Call) ->
    get_caller_id(Data, <<"external">>, Call).

-spec get_caller_id(wh_json:object(), binary(), whapps_call:call()) -> {api_binary(), api_binary()}.
get_caller_id(Data, Default, Call) ->
    Type = wh_json:get_value(<<"caller_id_type">>, Data, Default),
    cf_attributes:caller_id(Type, Call).

-spec set_caller_id(wh_json:object() | binary(), whapps_call:call()) -> whapps_call:call().
set_caller_id(CIDNumber, Call)
  when is_binary(CIDNumber) ->
    set_caller_id(CIDNumber, CIDNumber, Call);
set_caller_id(Data, Call) ->
    {CIDNumber, CIDName} = get_caller_id(Data, Call),
    set_caller_id(CIDNumber, CIDName, Call).

-spec set_caller_id(binary(), binary(), whapps_call:call()) -> whapps_call:call().
set_caller_id(CIDNumber, CIDName, Call) ->
    Props = props:filter_empty(
              [{<<"Caller-ID-Name">>, CIDName}
               ,{<<"Caller-ID-Number">>, CIDNumber}
              ]),
    Routines = [{fun whapps_call:set_caller_id_number/2, CIDNumber}
                ,{fun whapps_call:set_caller_id_name/2, CIDName}
                ,{fun whapps_call:set_custom_channel_vars/2, Props}
               ],
    whapps_call:exec(Routines, Call).

-spec get_callee_id(binary(), whapps_call:call()) -> {api_binary(), api_binary()}.
get_callee_id(EndpointId, Call) ->
    cf_attributes:callee_id(EndpointId, Call).

-spec set_callee_id(binary(), whapps_call:call()) -> whapps_call:call().
set_callee_id(EndpointId, Call) ->
    {CIDNumber, CIDName} = get_callee_id(EndpointId, Call),
    Props = props:filter_empty(
              [{<<"Callee-ID-Name">>, CIDName}
               ,{<<"Callee-ID-Number">>, CIDNumber}
              ]),
    whapps_call:set_custom_channel_vars(Props, Call).

-spec get_inbound_field(ne_binary()) -> ne_binaries().
get_inbound_field(Inception) ->
    case Inception of
        <<"on-net">> -> [<<"Caller-ID-Number">>, <<"From">>];
        <<"off-net">> -> [<<"Callee-ID-Number">>, <<"To">>];
        _ -> get_inbound_field(?DEFAULT_INCEPTION)
    end.

-spec get_inbound_destination(wh_json:object()) -> {ne_binary(), ne_binary()}.
get_inbound_destination(JObj) ->
    Inception = wh_json:get_value(<<"Route-Type">>, JObj, ?DEFAULT_INCEPTION),
    Keys = get_inbound_field(Inception),
    Number = wh_json:get_first_defined(Keys, JObj),
    {wnm_util:to_e164(Number), Inception}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec lookup_number(ne_binary()) ->
                           {'ok', ne_binary(), wh_proplist()} |
                           {'error', _}.
lookup_number(Number) ->
    Num = wnm_util:normalize_number(Number),
    case wh_cache:fetch_local(?DOODLE_CACHE, cache_key_number(Num)) of
        {'ok', {AccountId, Props}} ->
            lager:debug("cached number ~s is associated with account ~s", [Num, AccountId]),
            {'ok', AccountId, Props};
        {'error', 'not_found'} -> fetch_number(Num)
    end.

-spec fetch_number(ne_binary()) ->
                          {'ok', ne_binary(), wh_proplist()} |
                          {'error', _}.
fetch_number(Num) ->
    case wh_number_manager:lookup_account_by_number(Num) of
        {'ok', AccountId, Props} ->
            CacheProps = [{'origin', [{'db', wnm_util:number_to_db_name(Num), Num}, {'type', <<"number">>}]}],
            wh_cache:store_local(?DOODLE_CACHE, cache_key_number(Num), {AccountId, Props}, CacheProps),
            lager:debug("~s is associated with account ~s", [Num, AccountId]),
            {'ok', AccountId, Props};
        {'error', Reason}=E ->
            lager:debug("~s is not associated with any account, ~p", [Num, Reason]),
            E
    end.

-spec cache_key_number(ne_binary()) -> {'sms_number', ne_binary()}.
cache_key_number(Number) ->
    {'sms_number', Number}.

-spec lookup_mdn(ne_binary()) ->
                        {'ok', ne_binary(), api_binary()} |
                        {'error', _}.
lookup_mdn(Number) ->
    Num = wnm_util:normalize_number(Number),
    case wh_cache:fetch_local(?DOODLE_CACHE, cache_key_mdn(Num)) of
        {'ok', {Id, OwnerId}} ->
            lager:debug("cached number ~s is associated with ~s/~s", [Num, OwnerId, Id]),
            {'ok', Id, OwnerId};
        {'error', 'not_found'} -> fetch_mdn(Num)
    end.

-spec fetch_mdn(ne_binary()) ->
                       {'ok', ne_binary(), api_binary()} |
                       {'error', _}.
fetch_mdn(Num) ->
    case lookup_number(Num) of
        {'ok', AccountId, _Props} ->
            fetch_mdn_result(AccountId, Num);
        {'error', Reason}=E ->
            lager:debug("~s is not associated with any account, ~p", [Num, Reason]),
            E
    end.

-spec fetch_mdn_result(ne_binary(), ne_binary()) ->
                              {'ok', ne_binary(), api_binary()} |
                              {'error', 'not_found'}.
fetch_mdn_result(AccountId, Num) ->
    AccountDb = wh_util:format_account_db(AccountId),
    ViewOptions = [{'key', mdn_from_e164(Num)}],
    case couch_mgr:get_results(AccountDb, ?MDN_VIEW, ViewOptions) of
        {'ok', []} -> {'error', 'not_found'};
        {'ok', [JObj]} ->
            Id = wh_doc:id(JObj),
            OwnerId = wh_json:get_value([<<"value">>, <<"owner_id">>], JObj),
            lager:debug("~s is associated with mobile device ~s in account ~s", [Num, Id, AccountId]),
            cache_mdn_result(AccountDb, Id, OwnerId);
        {'error', _}=E -> E
    end.

-spec cache_mdn_result(ne_binary(), ne_binary(), api_binary()) ->
                              {'ok', ne_binary(), api_binary()}.
cache_mdn_result(AccountDb, Id, OwnerId) ->
    CacheProps = [{'origin', [{'db', AccountDb, Id}]}],
    wh_cache:store_local(?DOODLE_CACHE, cache_key_mdn(Id), {Id, OwnerId}, CacheProps),
    {'ok', Id, OwnerId}.

-spec cache_key_mdn(ne_binary()) -> {'sms_mdn', ne_binary()}.
cache_key_mdn(Number) ->
    {'sms_mdn', Number}.

%% TODO
%% MDN should be stored in e164 format
-spec mdn_from_e164(binary()) -> binary().
mdn_from_e164(<<"+1", Number/binary>>) -> Number;
mdn_from_e164(<<"1", Number/binary>>) -> Number;
mdn_from_e164(Number) -> Number.

-spec maybe_reschedule_sms(whapps_call:call()) -> 'ok'.
maybe_reschedule_sms(Call) ->
    maybe_reschedule_sms(<<>>, <<>>, Call).

-spec maybe_reschedule_sms(api_binary(), whapps_call:call()) -> 'ok'.
maybe_reschedule_sms(<<"sip:", Code/binary>>, Call) ->
    maybe_reschedule_sms(Code, <<>>, Call);
maybe_reschedule_sms(Code, Call) ->
    maybe_reschedule_sms(Code, <<>>, Call).

-spec maybe_reschedule_sms(api_binary(), api_binary(), whapps_call:call()) -> 'ok'.
maybe_reschedule_sms(Code, 'undefined', Call) ->
    maybe_reschedule_sms(Code, set_flow_error(<<"unknown error">>, Call));
maybe_reschedule_sms(<<"sip:", Code/binary>>, Message, Call) ->
    maybe_reschedule_sms(Code, Message, Call);
maybe_reschedule_sms(Code, Message, Call) ->
    maybe_reschedule_sms(Code, Message, whapps_call:account_id(Call), set_flow_error(Message, Call)).

-spec maybe_reschedule_sms(api_binary(), api_binary(), ne_binary(), whapps_call:call()) -> 'ok'.
maybe_reschedule_sms(Code, Message, AccountId, Call) ->
    put('call', Call),
    Rules = whapps_account_config:get_global(AccountId, ?CONFIG_CAT, <<"reschedule">>, wh_json:new()),
    Schedule = wh_json:set_values(
                 [{<<"code">>, Code}
                  ,{<<"reason">>, Message}
                 ]
                 ,whapps_call:kvs_fetch(<<"flow_schedule">>, wh_json:new(), Call)
                ),
    case apply_reschedule_logic(wh_json:get_values(Rules), Schedule) of
        'no_rule' ->
            lager:debug("no rules configured for accountid ~s", [AccountId]),
            doodle_exe:stop(set_flow_status(<<"error">>, Call));
        'end_rules' ->
            lager:debug("end rules configured for accountid ~s", [AccountId]),
            doodle_exe:stop(set_flow_status(<<"error">>,Call));
        NewSchedule ->
            doodle_exe:stop(whapps_call:kvs_store(<<"flow_schedule">>, NewSchedule, Call))
    end.

-spec inc_counters(wh_json:object(), list()) -> wh_json:object().
inc_counters(JObj, Counters) ->
    lists:foldl(fun inc_counter/2, JObj, Counters).

-spec inc_counter(binary(), wh_json:object()) -> wh_json:object().
inc_counter(Key, JObj) ->
    wh_json:set_value(Key, wh_json:get_integer_value(Key, JObj, 0) + 1, JObj).

-spec apply_reschedule_logic({wh_json:json_terms(), wh_json:keys()}, wh_json:object()) ->
                                    'no_rule' | 'end_rules' | wh_json:object().
apply_reschedule_logic({[], []}, _JObj) -> 'no_rule';
apply_reschedule_logic(Rules, JObj) ->
    Step = wh_json:get_integer_value(<<"rule">>, JObj, 1),
    apply_reschedule_logic(Rules, inc_counters(JObj, ?RESCHEDULE_COUNTERS), Step).

-spec apply_reschedule_logic({wh_json:json_terms(), wh_json:keys()}, wh_json:object(), integer()) ->
                                    'no_rule' | 'end_rules' | wh_json:object().
apply_reschedule_logic({_Vs, Ks}, _JObj, Step)
  when Step > length(Ks) -> 'end_rules';
apply_reschedule_logic({Vs, Ks}, JObj, Step) ->
    Rules = {lists:sublist(Vs, Step, length(Vs)), lists:sublist(Ks, Step, length(Ks))},
    apply_reschedule_rules(Rules, JObj, Step).

-spec apply_reschedule_rules({wh_json:json_terms(), wh_json:keys()}, wh_json:object(), integer()) ->
                                    wh_json:object() | 'end_rules'.
apply_reschedule_rules({[], _}, _JObj, _Step) -> 'end_rules';
apply_reschedule_rules({[Rule | Rules], [Key | Keys]}, JObj, Step) ->
    case apply_reschedule_step(wh_json:get_values(Rule), JObj) of
        'no_match' ->
            NewObj = wh_json:set_values(
                       [{<<"rule_start_time">>, wh_util:current_tstamp()}
                        ,{<<"attempts">>, 0}
                       ], JObj),
            apply_reschedule_rules({Rules, Keys}, NewObj, Step+1);
        Schedule -> wh_json:set_values(
                      [{<<"rule">>, Step}
                       ,{<<"rule_name">>, Key}
                      ], Schedule)
    end.

-spec apply_reschedule_step({wh_json:json_terms(), wh_json:keys()}, wh_json:object()) ->
                                   'no_match' | wh_json:object().
apply_reschedule_step({[], []}, JObj) -> JObj;
apply_reschedule_step({[Value | Values], [Key | Keys]}, JObj) ->
    case apply_reschedule_rule(Key, Value, JObj) of
        'no_match' -> 'no_match';
        Schedule -> apply_reschedule_step({Values, Keys}, Schedule)
    end.

-spec apply_reschedule_rule(ne_binary(), _, wh_json:object()) -> 'no_match' | wh_json:object().
apply_reschedule_rule(<<"error">>, ErrorObj, JObj) ->
    Codes = wh_json:get_value(<<"code">>, ErrorObj, []),
    XCodes = wh_json:get_value(<<"xcode">>, ErrorObj, []),
    Reasons = wh_json:get_value(<<"reason">>, ErrorObj, []),
    XReasons = wh_json:get_value(<<"xreason">>, ErrorObj, []),
    Code = wh_json:get_value(<<"code">>, JObj, <<>>),
    Reason = wh_json:get_value(<<"reason">>, JObj, <<>>),
    case    (lists:member(Code, Codes) orelse Codes =:= [])
    andalso (lists:member(Reason, Reasons) orelse Reasons =:= [])
    andalso ((not lists:member(Code, XCodes)) orelse XCodes =:= [])
    andalso ((not lists:member(Reason, XReasons)) orelse XReasons =:= [])
    of
        'true' -> JObj;
        'false' -> 'no_match'
    end;
apply_reschedule_rule(<<"number">>, Value, JObj) ->
    Attempts = wh_json:get_integer_value(<<"attempts">>, JObj, 0),
    case Attempts > Value of
        'true' -> 'no_match';
        'false' -> JObj
    end;
apply_reschedule_rule(<<"time">>, IntervalJObj, JObj) ->
    {[Value], [Key]} = wh_json:get_values(IntervalJObj),
    Start = wh_json:get_value(<<"rule_start_time">>, JObj),
    Until = time_rule(Key, Value, Start),
    Now = wh_util:current_tstamp(),
    case Until > Now of
        'true' -> JObj;
        'false' -> 'no_match'
    end;
apply_reschedule_rule(<<"interval">>, IntervalJObj, JObj) ->
    {[Value], [Key]} = wh_json:get_values(IntervalJObj),
    Next = time_rule(Key, Value, wh_util:current_tstamp()),
    wh_json:set_value(<<"start_time">>, Next, JObj);
apply_reschedule_rule(<<"report">>, V, JObj) ->
    Call = get('call'),
    Error = <<(wh_json:get_value(<<"code">>, JObj, <<>>))/binary, " "
              ,(wh_json:get_value(<<"reason">>, JObj, <<>>))/binary
            >>,
    Props = props:filter_undefined(
              [{<<"To">>, whapps_call:to_user(Call)}
               ,{<<"From">>, whapps_call:from_user(Call)}
               ,{<<"Error">>, wh_util:strip_binary(Error)}
               ,{<<"Attempts">>, wh_json:get_value(<<"attempts">>, JObj)}
               | safe_to_proplist(V)
              ]),
    Notify = [{<<"Subject">>, <<"sms_error">>}
              ,{<<"Message">>, <<"undelivered sms">>}
              ,{<<"Details">>, wh_json:set_values(Props, wh_json:new())}
              ,{<<"Account-ID">>, whapps_call:account_id(Call)}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ],
    wh_amqp_worker:cast(Notify, fun wapi_notifications:publish_system_alert/1),
    JObj;
apply_reschedule_rule(_, _, JObj) -> JObj.

-spec safe_to_proplist(_) -> wh_proplist().
safe_to_proplist(JObj) ->
    safe_to_proplist(wh_json:is_json_object(JObj), JObj).

-spec safe_to_proplist(boolean(), _) -> wh_proplist().
safe_to_proplist('true', JObj) ->
    wh_json:to_proplist(JObj);
safe_to_proplist(_, _) -> [].

-spec time_rule(ne_binary(), integer(), integer()) -> integer().
time_rule(<<"week">>, N, Base) -> Base + N * ?SECONDS_IN_WEEK;
time_rule(<<"day">>, N, Base) -> Base + N * ?SECONDS_IN_DAY;
time_rule(<<"hour">>, N, Base) -> Base + N * ?SECONDS_IN_HOUR;
time_rule(<<"minute">>, N, Base) -> Base + N * ?SECONDS_IN_MINUTE;
time_rule(<<"second">>, N, Base) -> Base + N * 1;
time_rule(_, _N, Base) -> Base.
