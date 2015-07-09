%%%-------------------------------------------------------------------
%%% @copyright
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   OnNet (Kirill Sysoev github.com/onnet)
%%%-------------------------------------------------------------------
-module(cccp_util).

-export([relay_amqp/2
         ,authorize/2
         ,handle_disconnect/2
         ,get_number/1
         ,store_last_dialed/2
         ,build_bridge_request/6
        ]).

-include("cccp.hrl").

-define(DEFAULT_CALLEE_REGEX, <<"^\\+?\\d{7,}$">>).

-spec relay_amqp(wh_json:object(), wh_proplist()) -> 'ok'.
relay_amqp(JObj, _Props) ->
    case whapps_call:retrieve(wh_json:get_value(<<"Call-ID">>, JObj), ?APP_NAME) of
        {'ok', Call} -> relay_event(JObj, Call);
        _ -> 'ok'
    end.

-spec relay_event(wh_json:object(), whapps_call:call()) -> 'ok'.
relay_event(JObj, Call) ->
    case whapps_call:kvs_fetch('consumer_pid', Call) of
        Pid when is_pid(Pid) -> whapps_call_command:relay_event(Pid, JObj);
        _ -> 'ok'
    end.

-spec handle_disconnect(wh_json:object(), wh_proplist()) -> 'ok'.
handle_disconnect(JObj, _Props) ->
    case (<<"CHANNEL_EXECUTE_COMPLETE">> =:= wh_json:get_value(<<"Event-Name">>, JObj))
        andalso is_binary(wh_json:get_value(<<"Hangup-Code">>, JObj))
    of
        'true' -> handle_disconnect_cause(JObj);
        'false' -> 'ok'
    end.

-spec handle_disconnect_cause(wh_json:object()) -> 'ok'.
handle_disconnect_cause(JObj) ->
    {'ok', Call} = whapps_call:retrieve(wh_json:get_value(<<"Call-ID">>, JObj), ?APP_NAME),
    case wh_json:get_value(<<"Disposition">>, JObj) of
        'undefined' -> 'ok';
        <<"UNALLOCATED_NUMBER">> ->
            whapps_call_command:prompt(<<"hotdesk-invalid_entry">>, Call),  %%% don't feel if it is needed
            whapps_call_command:queued_hangup(Call);                        %%% we can setup different prompt
        <<"INVALID_NUMBER_FORMAT">> ->                                      %%% for different hangup cause
            whapps_call_command:prompt(<<"hotdesk-invalid_entry">>, Call),
            whapps_call_command:queued_hangup(Call);
        <<"CALL_REJECTED">> ->
            whapps_call_command:prompt(<<"hotdesk-invalid_entry">>, Call),
            whapps_call_command:queued_hangup(Call);
        <<"USER_BUSY">> ->
            whapps_call_command:prompt(<<"hotdesk-invalid_entry">>, Call),
            whapps_call_command:queued_hangup(Call);
        UnhandledCause ->
            lager:debug("Unhandled disconnect cause: ~p", [UnhandledCause]),
            whapps_call_command:queued_hangup(Call)
    end.

-spec authorize(ne_binary(), ne_binary()) ->
                       {'ok', ne_binaries()} |
                       'empty' |
                       'error'.
authorize(Value, View) ->
    ViewOptions = [{'key', Value}],
    case couch_mgr:get_results(?KZ_CCCPS_DB, View, ViewOptions) of
        {'ok',[]} ->
            lager:info("Auth by ~p failed for: ~p. No such value in Db.", [Value, View]),
            'empty';   %%% don't change. used in cb_cccps.erl
        {'ok', [JObj]} ->
            AccountId = wh_json:get_value([<<"value">>,<<"account_id">>], JObj),
            OutboundCID = wh_json:get_value([<<"value">>,<<"outbound_cid">>], JObj),
            [AccountId
             ,legalize_outbound_cid(OutboundCID, AccountId)
             ,wh_json:get_value([<<"value">>,<<"id">>], JObj)
            ];
        _E ->
            lager:info("Auth failed for ~p. Error occurred: ~p.", [Value, _E]),
            'error'
    end.

-spec legalize_outbound_cid(ne_binary(), ne_binary()) -> ne_binary().
legalize_outbound_cid(OutboundCID, AccountId) ->
    case whapps_config:get_is_true(?CCCP_CONFIG_CAT, <<"ensure_valid_caller_id">>, 'true') of
        'true' -> ensure_valid_caller_id(OutboundCID, AccountId);
        'false' -> OutboundCID
    end.

-spec ensure_valid_caller_id(ne_binary(), ne_binary()) -> ne_binary().
ensure_valid_caller_id(OutboundCID, AccountId) ->
    {'ok', AccountPhoneNumbersList} =
        couch_mgr:open_cache_doc(wh_util:format_account_id(AccountId, 'encoded')
                                 ,?WNM_PHONE_NUMBER_DOC
                                ),
    case lists:member(wnm_util:normalize_number(OutboundCID)
                      ,wh_json:get_keys(AccountPhoneNumbersList)
                     )
    of
        'true' ->
            OutboundCID;
        'false' ->
            DefaultCID = whapps_config:get(?CCCP_CONFIG_CAT, <<"default_caller_id_number">>, <<"00000000000">>),
            lager:debug("OutboundCID ~p is out of account's list; changing to application's default: ~p", [OutboundCID, DefaultCID]),
            DefaultCID
    end.

-spec get_number(whapps_call:call()) ->
                        {'num_to_dial', ne_binary()} |
                        'ok'.
get_number(Call) ->
    RedialCode = whapps_config:get(?CCCP_CONFIG_CAT, <<"last_number_redial_code">>, <<"*0">>),
    case whapps_call_command:b_prompt_and_collect_digits(2, 17, <<"cf-enter_number">>, 3, Call) of
        {'ok', RedialCode} ->
            get_last_dialed_number(Call);
        {'ok', EnteredNumber} ->
            verify_entered_number(EnteredNumber, Call);
        _Err ->
            lager:info("No Phone number obtained: ~p", [_Err]),
            whapps_call_command:prompt(<<"hotdesk-invalid_entry">>, Call),
            whapps_call_command:queued_hangup(Call)
    end.

-spec verify_entered_number(ne_binary(), whapps_call:call()) -> 'ok'.
verify_entered_number(EnteredNumber, Call) ->
    Number = wnm_util:to_e164(re:replace(EnteredNumber, "[^0-9]", "", ['global', {'return', 'binary'}])),
    case cccp_allowed_callee(Number) of
        'true' ->
            check_restrictions(Number, Call);
        _ ->
            lager:debug("Wrong number entered: ~p", [EnteredNumber]),
            whapps_call_command:prompt(<<"hotdesk-invalid_entry">>, Call),
            whapps_call_command:queued_hangup(Call)
    end.

-spec get_last_dialed_number(whapps_call:call()) ->
                                    {'num_to_dial', ne_binary()} |
                                    'ok'.
get_last_dialed_number(Call) ->
    {'ok', CachedCall} = whapps_call:retrieve(whapps_call:call_id(Call), ?APP_NAME),
    DocId = whapps_call:kvs_fetch('auth_doc_id', CachedCall),
    {'ok', Doc} = couch_mgr:open_doc(<<"cccps">>, DocId),
    LastDialed = wh_json:get_value(<<"pvt_last_dialed">>, Doc),
    case cccp_allowed_callee(LastDialed) of
       'false' ->
            whapps_call_command:prompt(<<"hotdesk-invalid_entry">>, Call),
            whapps_call_command:queued_hangup(Call);
        'true' ->
            check_restrictions(LastDialed, Call)
    end.

-spec store_last_dialed(ne_binary(), ne_binary()) -> 'ok'.
store_last_dialed(Number, DocId) ->
    {'ok', Doc} = couch_mgr:update_doc(<<"cccps">>, DocId, [{<<"pvt_last_dialed">>, Number}]),
    _ = couch_mgr:update_doc(wh_doc:account_db(Doc), DocId, [{<<"pvt_last_dialed">>, Number}]),
    'ok'.

-spec check_restrictions(ne_binary(), whapps_call:call()) ->
                                {'num_to_dial', ne_binary()} |
                                'ok'.
check_restrictions(Number, Call) ->
    {'ok', CachedCall} = whapps_call:retrieve(whapps_call:call_id(Call), ?APP_NAME),
    DocId = whapps_call:kvs_fetch('auth_doc_id', CachedCall),
    {'ok', Doc} = couch_mgr:open_doc(<<"cccps">>, DocId),
    AccountId = wh_doc:account_id(Doc),
    AccountDb = wh_doc:account_db(Doc),
    case is_number_restricted(Number, AccountId, AccountDb) of
       'true' ->
            lager:debug("Number ~p is restricted", [Number]),
            hangup_unauthorized_call(Call);
       'false' ->
            is_user_restricted(Number, wh_json:get_value(<<"user_id">>, Doc), AccountDb, Call)
    end.

-spec is_number_restricted(ne_binary(), ne_binary(), ne_binary()) -> boolean().
is_number_restricted(Number, DocId, AccountDb) ->
    case couch_mgr:open_cache_doc(AccountDb, DocId) of
        {'error', _} -> 'false';
        {'ok', JObj} ->
            Classification = wnm_util:classify_number(Number),
            wh_json:get_value([<<"call_restriction">>, Classification, <<"action">>], JObj) =:= <<"deny">>
    end.

-spec is_user_restricted(ne_binary(), ne_binary(), ne_binary(), whapps_call:call()) ->
                                {'num_to_dial', ne_binary()} |
                                'ok'.
is_user_restricted(Number, UserId, AccountDb, Call) ->
    case is_number_restricted(Number, UserId, AccountDb) of
        'true' ->
            lager:debug("Number ~p is restricted", [Number]),
            hangup_unauthorized_call(Call);
        'false' ->
            {'num_to_dial', Number}
    end.

-spec hangup_unauthorized_call(whapps_call:call()) -> 'ok'.
hangup_unauthorized_call(Call) ->
    whapps_call_command:prompt(<<"cf-unauthorized_call">>, Call),
    whapps_call_command:queued_hangup(Call).

-spec build_bridge_request(ne_binary(), ne_binary(), binary(), ne_binary(), ne_binary(), ne_binary()) ->
                                  wh_proplist().
build_bridge_request(CallId, ToDID, Q, CtrlQ, AccountId, OutboundCID) ->
    props:filter_undefined(
      [{<<"Resource-Type">>, <<"audio">>}
       ,{<<"Application-Name">>, <<"bridge">>}
       ,{<<"Existing-Call-ID">>, CallId}
       ,{<<"Call-ID">>, CallId}
       ,{<<"Control-Queue">>, CtrlQ}
       ,{<<"To-DID">>, ToDID}
       ,{<<"Resource-Type">>, <<"originate">>}
       ,{<<"Outbound-Caller-ID-Number">>, OutboundCID}
       ,{<<"Outbound-Caller-ID-Name">>, OutboundCID}
       ,{<<"Originate-Immediate">>, 'true'}
       ,{<<"Msg-ID">>, wh_util:rand_hex_binary(6)}
       ,{<<"Account-ID">>, AccountId}
       | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
      ]).

-spec cccp_allowed_callee(ne_binary()) -> boolean().
cccp_allowed_callee(Number) ->
    Regex = whapps_config:get_binary(?CCCP_CONFIG_CAT, <<"allowed_callee_regex">>, ?DEFAULT_CALLEE_REGEX),
    case re:run(Number, Regex) of
        'nomatch' ->
            lager:debug("number '~s' is not allowed to call through cccp", [Number]),
            'false';
        _ ->
            lager:debug("number '~s' is allowed to call through cccp, proceeding", [Number]),
            'true'
    end.
