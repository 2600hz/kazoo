%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(doodle_util).

-include("doodle.hrl").

-define(SIP_ENDPOINT_ID_KEY(Realm, User), {?MODULE, 'sip_endpoint_id', Realm, User}).
-define(SIP_ENDPOINT_KEY(Realm, User), {?MODULE, 'sip_endpoint', Realm, User}).
-define(DEFAULT_INCEPTION, <<"off-net">>).
-define(MDN_VIEW, <<"mobile/listing_by_mdn">>).
-define(CONVERT_MDN, 'true').

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
-export([get_caller_id/2 , get_caller_id/3, set_caller_id/2]).
-export([lookup_number/1]).
-export([get_inbound_destination/1]).
-export([lookup_mdn/1]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec set_sms_body(ne_binary(), whapps_call:call()) -> whapps_call:call().
set_sms_body(Body, Call) ->
    whapps_call:kvs_store(<<"Body">>, Body, Call).

-spec get_sms_body(whapps_call:call()) -> ne_binary().
get_sms_body(Call) ->
    whapps_call:kvs_fetch(<<"Body">>, Call).

-spec set_flow_status(ne_binary(), whapps_call:call()) -> whapps_call:call().
set_flow_status(Status, Call) ->
    whapps_call:kvs_store(<<"flow_status">>, Status, Call).

-spec set_flow_status(ne_binary(), ne_binary(), whapps_call:call()) -> whapps_call:call().
set_flow_status(Status, Message, Call) ->
    Props = [{<<"flow_status">>, Status}
             ,{<<"flow_message">>, Message}
            ],
    whapps_call:kvs_store_proplist(Props, Call).

-spec set_flow_error(ne_binary(), whapps_call:call()) -> whapps_call:call().
set_flow_error(Error, Call) ->
    set_flow_error(<<"pending">>, Error, Call).

-spec set_flow_error(ne_binary(), ne_binary(), whapps_call:call()) -> whapps_call:call().
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
    save_sms(wh_json:new(), whapps_call:kvs_fetch('sms_docid', Call), Call).

-spec save_sms(wh_json:object(), whapps_call:call()) -> whapps_call:call().
save_sms(JObj, Call) ->
    save_sms(JObj, whapps_call:kvs_fetch('sms_docid', Call), Call).

-spec save_sms(wh_json:object(), api_binary(), whapps_call:call()) -> whapps_call:call().
save_sms(JObj, 'undefined', Call) ->
    {Year, Month, _} = erlang:date(),
    SmsDocId = wh_util:to_binary(
                 io_lib:format("~B~s-~s",
                               [Year
                                ,wh_util:pad_month(Month)
                                , whapps_call:call_id(Call)
                               ])),
    UpdatedCall = whapps_call:kvs_store('sms_docid', SmsDocId, Call),
    Doc = wh_json:set_value(<<"pvt_created">>, wh_util:current_tstamp(), wh_json:new()),
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
    To = whapps_call:to(Call),
    From = whapps_call:from(Call),
    Request = whapps_call:request(Call),
    [ToUser, ToRealm] = binary:split(To, <<"@">>),
    [FromUser, FromRealm] = binary:split(From, <<"@">>),
    [RequestUser, RequestRealm] = binary:split(Request, <<"@">>),
    MessageId = wh_json:get_value(<<"Message-ID">>, JObj),
    Status = whapps_call:kvs_fetch(<<"flow_status">>, <<"queued">>, Call),
    Rev = get_sms_revision(Call),
    Opts = props:filter_undefined([{'rev', Rev}]),
    Props = props:filter_empty(
              [{<<"_id">>, DocId}
               ,{<<"pvt_type">>, <<"sms">> }
               ,{<<"account_id">>, AccountId }
               ,{<<"pvt_account_id">>, AccountId }
               ,{<<"pvt_account_db">>, AccountDb }
               ,{<<"owner_id">>, OwnerId }
               ,{<<"pvt_owner_id">>, OwnerId }
               ,{<<"authorization_type">>, AuthType }
               ,{<<"authorization_id">>, AuthId }
               ,{<<"pvt_authorization_type">>, AuthType }
               ,{<<"pvt_authorization_id">>, AuthId }
               ,{<<"to">>, To }
               ,{<<"to_user">>, ToUser }
               ,{<<"to_realm">>, ToRealm }
               ,{<<"from">>, From }
               ,{<<"from_user">>, FromUser }
               ,{<<"from_realm">>, FromRealm }
               ,{<<"request">>, Request }
               ,{<<"request_user">>, RequestUser }
               ,{<<"request_realm">>, RequestRealm }
               ,{<<"body">>, Body }
               ,{<<"message_id">>, MessageId}
               ,{<<"pvt_modified">>, wh_util:current_tstamp()}
               ,{<<"pvt_status">>, Status}
               ,{<<"call_id">>, whapps_call:call_id_direct(Call)}
               ,{<<"pvt_call">>, whapps_call:to_json(whapps_call:kvs_erase(<<"_rev">>, Call))}
               ,{<<"_rev">>, Rev}
              ]),
    JObjDoc = wh_json:set_values(Props, Doc),
    kazoo_modb:create(AccountDb),
    case couch_mgr:save_doc(AccountDb, JObjDoc, Opts) of
        {'ok', Saved} ->
            whapps_call:kvs_store(<<"_rev">>, wh_json:get_value(<<"_rev">>, Saved), Call);
        {'error', E} ->
            lager:debug("error saving sms doc , wazzup ? ~p", [E]),
            Call
    end.

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
    ViewOptions = [{'key', [wh_util:to_lower_binary(Realm), wh_util:to_lower_binary(Username)]}],
    case couch_mgr:get_results(?WH_SIP_DB, <<"credentials/lookup">>, ViewOptions) of
        {'ok', [JObj]} ->
            EndpointId = wh_json:get_value(<<"id">>, JObj),
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
    ViewOptions = [{'key', [wh_util:to_lower_binary(Realm), wh_util:to_lower_binary(Username)]}
                   ,'include_docs'
                  ],
    case couch_mgr:get_results(?WH_SIP_DB, <<"credentials/lookup">>, ViewOptions) of
        {'ok', [JObj]} ->
            EndpointId = wh_json:get_value(<<"id">>, JObj),
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

-spec replay_sms(ne_binary(), ne_binary()) -> any().
replay_sms(AccountId, DocId) ->
    lager:debug("trying to replay sms ~s for account ~s",[DocId, AccountId]),
    {'ok', Doc} = kazoo_modb:open_doc(AccountId, DocId),
    Flow = wh_json:get_value(<<"pvt_call">>, Doc),
    Rev = wh_json:get_value(<<"_rev">>, Doc),
    replay_sms_flow(AccountId, DocId, Rev, Flow).

-spec replay_sms_flow(ne_binary(), ne_binary(), ne_binary(), api_object()) -> any().
replay_sms_flow(_AccountId, _DocId, _Rev, 'undefined') -> 'ok';
replay_sms_flow(AccountId, <<_:7/binary, CallId/binary>> = DocId, Rev, JObj) ->
    lager:debug("replaying sms ~s for account ~s",[DocId, AccountId]),
    Routines = [{fun whapps_call:set_call_id/2, CallId}
                ,fun(C) -> set_sms_revision(Rev, C) end
                ,fun(C) -> set_flow_status(<<"resumed">>, C) end
                ,fun save_sms/1
               ],

    Call = whapps_call:exec(Routines, whapps_call:from_json(JObj)),
    whapps_call:put_callid(Call),

    lager:info("doodle received sms resume for ~s of account ~s, taking control", [DocId, AccountId]),
    doodle_route_win:maybe_restrict_call(JObj, Call).

-spec sms_status(api_object()) -> binary().
sms_status('undefined') -> <<"pending">>;
sms_status(JObj) ->
    DeliveryCode = wh_json:get_value(<<"Delivery-Result-Code">>, JObj),
    Status = wh_json:get_value(<<"Status">>, JObj),
    sms_status(DeliveryCode, Status).

-spec sms_status(api_binary(), api_binary()) -> binary().
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

-spec set_caller_id(wh_json:object(), whapps_call:call()) -> whapps_call:call().
set_caller_id(Data, Call) ->
    {CIDNumber, CIDName} = get_caller_id(Data, Call),
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
                           {'error', term()}.
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
                          {'error', term()}.
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
                        {'error', term()}.
lookup_mdn(Number) ->
    Num = wnm_util:normalize_number(Number),
    case wh_cache:fetch_local(?DOODLE_CACHE, cache_key_mdn(Num)) of
        {'ok', Id, OwnerId} ->
            lager:debug("cached number ~s is associated with ~s/~s", [Num, OwnerId, Id]),
            {'ok', Id, OwnerId};
        {'error', 'not_found'} -> fetch_mdn(Num)
    end.

-spec fetch_mdn(ne_binary()) ->
                       {'ok', ne_binary(), api_binary()} |
                       {'error', term()}.
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
            Id = wh_json:get_value(<<"id">>, JObj),
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
