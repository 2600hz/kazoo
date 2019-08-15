%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author OnNet (Kirill Sysoev github.com/onnet)
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cccp_util).

-export([relay_amqp/2
        ,authorize/2
        ,handle_disconnect/2
        ,get_number/1
        ,store_last_dialed/2
        ,build_request/10
        ,bridge/8
        ,verify_entered_number/3
        ,get_last_dialed_number/1
        ,current_account_outbound_directions/1
        ,count_user_legs/2

        ,register_views/0
        ,init_db/0
        ]).

-include("cccp.hrl").

-define(DEFAULT_CALLEE_REGEX, <<"^\\+?\\d{7,}$">>).

-spec relay_amqp(kz_json:object(), kz_term:proplist()) -> 'ok'.
relay_amqp(JObj, Props) ->
    case kapps_call:kvs_fetch('consumer_pid', props:get_value('call', Props)) of
        Pid when is_pid(Pid) -> kapps_call_command:relay_event(Pid, JObj);
        _ -> 'ok'
    end.

-spec handle_disconnect(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_disconnect(JObj, Props) ->
    case (<<"CHANNEL_EXECUTE_COMPLETE">> =:= kz_json:get_value(<<"Event-Name">>, JObj))
        andalso is_binary(kz_json:get_value(<<"Hangup-Code">>, JObj))
    of
        'true' -> handle_disconnect_cause(JObj, props:get_value('call', Props));
        'false' -> 'ok'
    end.

-spec handle_disconnect_cause(kz_json:object(), kapps_call:call()) -> 'ok'.
handle_disconnect_cause(JObj, Call) ->
    case kz_json:get_value(<<"Disposition">>, JObj) of
        'undefined' -> 'ok';
        <<"UNALLOCATED_NUMBER">> ->
            _ = kapps_call_command:prompt(<<"hotdesk-invalid_entry">>, Call),  %%% don't feel if it is needed
            kapps_call_command:queued_hangup(Call);                        %%% we can setup different prompt
        <<"INVALID_NUMBER_FORMAT">> ->                                      %%% for different hangup cause
            _ = kapps_call_command:prompt(<<"hotdesk-invalid_entry">>, Call),
            kapps_call_command:queued_hangup(Call);
        <<"CALL_REJECTED">> ->
            _ = kapps_call_command:prompt(<<"hotdesk-invalid_entry">>, Call),
            kapps_call_command:queued_hangup(Call);
        <<"USER_BUSY">> ->
            _ = kapps_call_command:prompt(<<"hotdesk-invalid_entry">>, Call),
            kapps_call_command:queued_hangup(Call);
        UnhandledCause ->
            lager:debug("unhandled disconnect cause: ~p", [UnhandledCause]),
            kapps_call_command:queued_hangup(Call)
    end.

-spec authorize(kz_term:ne_binary(), kz_term:ne_binary()) ->
                       {'ok', kz_json:object()} |
                       'empty' |
                       'error'.
authorize(Value, View) ->
    ViewOptions = [{'key', Value}],
    case kz_datamgr:get_results(?KZ_CCCPS_DB, View, ViewOptions) of
        {'ok', [JObj]} ->
            {'ok', kz_json:get_value(<<"value">>,JObj)};
        Reply ->
            lager:info("auth failed for ~p with reply: ~p.", [Value, Reply]),
            Reply
    end.

-spec get_number(kapps_call:call()) ->
                        {'num_to_dial', kz_term:ne_binary()} |
                        'ok'.
get_number(Call) ->
    get_number(Call, 3).

-spec get_number(kapps_call:call(), integer()) ->
                        {'num_to_dial', kz_term:ne_binary()} |
                        'ok'.
get_number(Call, 0) ->
    lager:info("run out of attempts amount... hanging up"),
    _ = kapps_call_command:prompt(<<"hotdesk-invalid_entry">>, Call),
    kapps_call_command:queued_hangup(Call);
get_number(Call, Retries) ->
    RedialCode = kapps_config:get_ne_binary(?CCCP_CONFIG_CAT, <<"last_number_redial_code">>, <<"*0">>),
    case kapps_call_command:b_prompt_and_collect_digits(2, 17, <<"cf-enter_number">>, 3, Call) of
        {'ok', RedialCode} ->
            get_last_dialed_number(Call);
        {'ok', EnteredNumber} ->
            verify_entered_number(EnteredNumber, Call, Retries);
        _Err ->
            lager:info("no phone number obtained: ~p", [_Err]),
            _ = kapps_call_command:prompt(<<"hotdesk-invalid_entry">>, Call),
            get_number(Call, Retries - 1)
    end.

-spec verify_entered_number(kz_term:ne_binary(), kapps_call:call(), integer()) -> 'ok'.
verify_entered_number(EnteredNumber, Call, Retries) ->
    Number = knm_converters:normalize(re:replace(EnteredNumber, "[^0-9]", "", ['global', {'return', 'binary'}])),
    case cccp_allowed_callee(Number) of
        'true' ->
            check_restrictions(Number, Call);
        _ ->
            lager:debug("wrong number entered: ~p", [EnteredNumber]),
            _ = kapps_call_command:prompt(<<"hotdesk-invalid_entry">>, Call),
            get_number(Call, Retries - 1)
    end.

-spec get_last_dialed_number(kapps_call:call()) ->
                                    {'num_to_dial', kz_term:ne_binary()} |
                                    'ok'.
get_last_dialed_number(Call) ->
    DocId = kapps_call:kvs_fetch('auth_doc_id', Call),
    {'ok', Doc} = kz_datamgr:open_doc(?KZ_CCCPS_DB, DocId),
    LastDialed = kz_json:get_value(<<"pvt_last_dialed">>, Doc),
    case cccp_allowed_callee(LastDialed) of
        'false' ->
            _ = kapps_call_command:prompt(<<"hotdesk-invalid_entry">>, Call),
            kapps_call_command:queued_hangup(Call);
        'true' ->
            check_restrictions(LastDialed, Call)
    end.

-spec store_last_dialed(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
store_last_dialed(Number, DocId) ->
    Updates = [{<<"pvt_last_dialed">>, Number}],
    UpdateOptions = [{'update', Updates}],

    {'ok', Doc} = kz_datamgr:update_doc(?KZ_CCCPS_DB, DocId, UpdateOptions),
    _ = kz_datamgr:update_doc(kz_doc:account_db(Doc), DocId, UpdateOptions),

    'ok'.

-spec check_restrictions(kz_term:ne_binary(), kapps_call:call()) ->
                                {'num_to_dial', kz_term:ne_binary()} |
                                'ok'.
check_restrictions(Number, Call) ->
    DocId = kapps_call:kvs_fetch('auth_doc_id', Call),
    {'ok', Doc} = kz_datamgr:open_doc(?KZ_CCCPS_DB, DocId),
    AccountId = kz_doc:account_id(Doc),
    AccountDb = kz_doc:account_db(Doc),
    case is_number_restricted(Number, AccountId, AccountDb) of
        'true' ->
            lager:debug("number ~p is restricted", [Number]),
            hangup_unauthorized_call(Call);
        'false' ->
            is_user_restricted(Number, kz_json:get_value(<<"user_id">>, Doc), AccountDb, Call)
    end.

-spec is_number_restricted(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
is_number_restricted(Number, DocId, AccountDb) ->
    case kz_datamgr:open_cache_doc(AccountDb, DocId) of
        {'error', _} -> 'false';
        {'ok', JObj} ->
            Classification = knm_converters:classify(Number),
            kz_json:get_value([<<"call_restriction">>, Classification, <<"action">>], JObj) =:= <<"deny">>
    end.

-spec is_user_restricted(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kapps_call:call()) ->
                                {'num_to_dial', kz_term:ne_binary()} |
                                'ok'.
is_user_restricted(Number, UserId, AccountDb, Call) ->
    case is_number_restricted(Number, UserId, AccountDb) of
        'true' ->
            lager:debug("number ~p is restricted", [Number]),
            hangup_unauthorized_call(Call);
        'false' ->
            {'num_to_dial', Number}
    end.

-spec hangup_unauthorized_call(kapps_call:call()) -> 'ok'.
hangup_unauthorized_call(Call) ->
    _ = kapps_call_command:prompt(<<"cf-unauthorized_call">>, Call),
    kapps_call_command:queued_hangup(Call).

-spec cccp_allowed_callee(kz_term:ne_binary()) -> boolean().
cccp_allowed_callee(Number) ->
    Regex = kapps_config:get_binary(?CCCP_CONFIG_CAT, <<"allowed_callee_regex">>, ?DEFAULT_CALLEE_REGEX),
    case re:run(Number, Regex) of
        'nomatch' ->
            lager:debug("number '~s' is not allowed to call through cccp", [Number]),
            'false';
        _ ->
            lager:debug("number '~s' is allowed to call through cccp, proceeding", [Number]),
            'true'
    end.

-spec build_request(kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary(), kz_term:ne_binary(),binary(),binary()) -> kz_term:proplist().
build_request(CallId, ToDID, AuthorizingId, Q, CtrlQ, AccountId, Action, RetainCID, RetainName, RetainNumber) ->
    Realm = kzd_accounts:fetch_realm(AccountId),
    CCVs = props:filter_undefined([{<<"Account-ID">>, AccountId}
                                  ,{<<"Authorizing-ID">>, AuthorizingId}
                                  ,{<<"Authorizing-Type">>, <<"user">>}
                                  ,{<<"Retain-CID">>, RetainCID}
                                  ,{<<"Presence-ID">>, build_presence(ToDID, Realm)}
                                  ]),
    Diversions = case RetainCID of
                     <<"true">> ->
                         AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
                         {AccountNumber,_} = kz_attributes:maybe_get_assigned_number('undefined', 'undefined', AccountDb),
                         [{<<"Diversions">>, [<<"<sip:", AccountNumber/binary, "@", Realm/binary, ">;reason=unconditional">>]}];
                     <<"false">> -> []
                 end,
    Endpoint = [{<<"Invite-Format">>, <<"loopback">>}
               ,{<<"Route">>,  ToDID}
               ,{<<"To-DID">>, ToDID}
               ,{<<"Custom-Channel-Vars">>, kz_json:from_list(CCVs)}
               ,{<<"Custom-SIP-Headers">>,  kz_json:from_list(Diversions)}
               ],
    {CIDNumber, CIDName} = compose_cid(ToDID, RetainCID, RetainNumber, RetainName, AccountId),
    props:filter_undefined(
      [{<<"Resource-Type">>, <<"audio">>}
      ,{<<"Caller-ID-Name">>, maybe_cid_name(CIDName, CIDNumber)}
      ,{<<"Caller-ID-Number">>, CIDNumber}
      ,{<<"Application-Name">>, Action}
      ,{<<"Endpoints">>, [kz_json:from_list(Endpoint)]}
      ,{<<"Resource-Type">>, <<"originate">>}
      ,{<<"Control-Queue">>, CtrlQ}
      ,{<<"Existing-Call-ID">>, CallId}
      ,{<<"Originate-Immediate">>, <<"true">>}
      ,{<<"Msg-ID">>, kz_binary:rand_hex(8)}
      ,{<<"Account-ID">>, AccountId}
      ,{<<"Dial-Endpoint-Method">>, <<"single">>}
      ,{<<"Continue-On-Fail">>, <<"true">>}
      ,{<<"Custom-Channel-Vars">>, kz_json:from_list(CCVs)}
      ,{<<"Custom-SIP-Headers">>,  kz_json:from_list(Diversions)}
      ,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>, <<"Retain-CID">>, <<"Authorizing-ID">>, <<"Authorizing-Type">>]}
       | kz_api:default_headers(Q, <<"resource">>, <<"originate_req">>, ?APP_NAME, ?APP_VERSION)
      ]).

-spec bridge(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), binary(), binary()) -> 'ok'.
bridge(CallId, ToDID, AuthorizingId, CtrlQ, AccountId, RetainCID, RetainName, RetainNumber) ->
    Req = build_request(CallId, ToDID, AuthorizingId, 'undefined', CtrlQ, AccountId, <<"bridge">>, RetainCID, RetainName, RetainNumber),
    kapi_resource:publish_originate_req(Req).

-spec compose_cid(kz_term:ne_binary(), kz_term:ne_binary(), binary(), binary(), kz_term:ne_binary()) ->
                         {kz_term:api_binary(), kz_term:api_binary()}.
compose_cid(ToDID, RetainCID, RetainNumber, RetainName, AccountId) ->
    case RetainCID of
        <<"true">> ->
            maybe_outbound_call(ToDID, RetainNumber, RetainName, AccountId);
        _ ->
            {'undefined','undefined'}
    end.

-spec maybe_outbound_call(kz_term:ne_binary(), binary(), binary(), kz_term:ne_binary()) ->
                                 {binary(), binary()}.
maybe_outbound_call(ToDID, RetainNumber, RetainName, AccountId) ->
    case knm_converters:is_reconcilable(ToDID) of
        'false' -> {RetainNumber, RetainName};
        'true' ->
            case knm_converters:is_reconcilable(RetainNumber) of
                'true' ->
                    {knm_converters:normalize(RetainNumber), RetainName};
                'false' ->
                    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
                    kz_attributes:maybe_get_assigned_number('undefined', RetainName, AccountDb)
            end
    end.

-spec maybe_cid_name(kz_term:api_ne_binary(), kz_term:api_ne_binary()) -> kz_term:api_ne_binary().
maybe_cid_name(<<Name/binary>>, _) -> Name;
maybe_cid_name(_, Number) -> Number.

-spec build_presence(kz_term:api_ne_binary(), kz_term:ne_binary()) -> kz_term:api_ne_binary().
build_presence(<<Number/binary>>, Realm) -> <<Number/binary, "@", Realm/binary>>;
build_presence(_, _) -> 'undefined'.

-spec current_account_outbound_directions(kz_term:ne_binary()) -> kz_term:ne_binaries().
current_account_outbound_directions(AccountId) ->
    [kz_json:get_ne_binary_value(<<"destination">>, Channel)
     || Channel <- current_account_channels(AccountId),
        <<"outbound">> =:= kz_json:get_ne_binary_value(<<"direction">>, Channel)
    ].

-spec count_user_legs(kz_term:ne_binary(), kz_term:ne_binary()) -> integer().
count_user_legs(UserId, AccountId) ->
    lists:foldl(fun(Channel, Acc) ->
                        is_user_channel(Channel, UserId) + Acc
                end
               ,0
               ,current_account_channels(AccountId)
               ).

-spec is_user_channel(kz_json:object(), kz_term:ne_binary()) -> integer().
is_user_channel(Channel, UserId) ->
    case kz_json:get_ne_binary_value(<<"authorizing_id">>, Channel) of
        UserId -> 1;
        _ -> 0
    end.

-spec current_account_channels(kz_term:ne_binary()) -> kz_json:objects().
current_account_channels(AccountId) ->
    Req = [{<<"Realm">>, kzd_accounts:fetch_realm(AccountId)}
          ,{<<"Usernames">>, []}
          ,{<<"Account-ID">>, AccountId}
          ,{<<"Active-Only">>, 'false'}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case kz_amqp_worker:call_collect(Req
                                    ,fun kapi_call:publish_query_account_channels_req/1
                                    ,{'ecallmgr', 'true'}
                                    )
    of
        {'error', _R} ->
            lager:info("cccp could not reach ecallmgr channels: ~p", [_R]),
            [];
        {_OK, [Resp|_]} ->
            kz_json:get_list_value(<<"Channels">>, Resp, [])
    end.

-spec register_views() -> 'ok'.
register_views() ->
    kz_datamgr:register_views_from_folder('cccp').

-spec init_db() -> 'ok'.
init_db() ->
    case kz_datamgr:db_exists(<<"cccps">>) of
        'true' ->
            _ = kapps_maintenance:refresh(?KZ_CCCPS_DB),
            'ok';
        'false' ->
            kz_datamgr:db_create(<<"cccps">>),
            _ = kapps_maintenance:refresh(?KZ_CCCPS_DB),
            'ok'
    end.
