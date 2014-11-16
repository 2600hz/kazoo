%%%-------------------------------------------------------------------
%%% @copyright 
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   OnNet (Kirill Sysoev github.com/onnet)
%%%-------------------------------------------------------------------
-module(cccp_util).

-export([handle_callinfo/2
        ,relay_amqp/2
        ,authorize/2
        ,handle_disconnect/2
        ,get_number/1
        ,store_last_dialed/2
        ]).

-include("cccp.hrl").

-spec handle_callinfo(wh_json:object(), wh_proplist()) -> 'ok'.
handle_callinfo(JObj, Props) ->
    relay_amqp(JObj, Props).

-spec relay_amqp(wh_json:object(), wh_proplist()) -> 'ok'.
relay_amqp(JObj, _Props) ->
    case whapps_call:retrieve(wh_json:get_value(<<"Call-ID">>, JObj), ?APP_NAME) of
        {'ok', Call} ->
            relay_event(JObj, Call);
        _ ->
            'ok'
    end.

-spec relay_event(wh_json:object(), wh_proplist()) -> 'ok'.
relay_event(JObj, Call) ->
    RouteWinPid = whapps_call:kvs_fetch('consumer_pid', Call),
    case is_pid(RouteWinPid) of
        true ->
            whapps_call_command:relay_event(RouteWinPid, JObj);
        _ ->
            'ok'
    end.

-spec handle_disconnect(wh_json:object(), wh_proplist()) -> 'ok'.
handle_disconnect(JObj, _Props) ->
    {'ok', Call} = whapps_call:retrieve(wh_json:get_value(<<"Call-ID">>, JObj), ?APP_NAME),
    case (<<"CHANNEL_EXECUTE_COMPLETE">> =:= wh_json:get_value(<<"Event-Name">>, JObj)) 
             andalso 
         is_binary(wh_json:get_value(<<"Hangup-Code">>, JObj)) of
        'true' ->
             case wh_json:get_value(<<"Disposition">>, JObj) of
                'undefined' ->
                    'ok';
                <<"UNALLOCATED_NUMBER">> ->
                    whapps_call_command:prompt(<<"hotdesk-invalid_entry">>, Call),
                    whapps_call_command:queued_hangup(Call);
                <<"INVALID_NUMBER_FORMAT">> ->
                    whapps_call_command:prompt(<<"hotdesk-invalid_entry">>, Call),
                    whapps_call_command:queued_hangup(Call);
                <<"CALL_REJECTED">> ->
                    whapps_call_command:prompt(<<"hotdesk-invalid_entry">>, Call),
                    whapps_call_command:queued_hangup(Call);
                _ ->
                    whapps_call_command:queued_hangup(Call)
             end;
         _ -> 'ok'
    end.

-spec authorize(ne_binary(), ne_binary()) -> {ok, list()} | 'empty' | 'error'.
authorize(Value, View) ->
    ViewOptions = [{'key', Value}],
    case couch_mgr:get_results(?CCCPS_DB, View, ViewOptions) of
        {ok,[]} ->
            lager:info("Auth by ~p failed for: ~p. No such value in Db.", [Value, View]),
            'empty';   %%% don't change. used in cb_cccps.erl
        {ok, [JObj]} ->
            lager:info("Value ~p is authorized.", [JObj]),
            AccountId = wh_json:get_value([<<"value">>,<<"account_id">>], JObj),
            OutboundCID = wh_json:get_value([<<"value">>,<<"outbound_cid">>], JObj),
            [
             AccountId,
             legalize_outbound_cid(OutboundCID, AccountId),
             wh_json:get_value([<<"value">>,<<"id">>], JObj)
            ];
        E ->
            lager:info("Auth failed for ~p. Error occurred: ~p.", [Value, E]),
            'error'
    end.

-spec legalize_outbound_cid(ne_binary(), ne_binary()) -> ne_binary().
legalize_outbound_cid(OutboundCID, AccountId) ->
    case whapps_config:get_is_true(?CCCP_CONFIG_CAT, <<"ensure_valid_caller_id">>, 'true') of
        'true' ->
            {ok, AccountPhoneNumbersList} = couch_mgr:open_cache_doc(wh_util:format_account_id(AccountId, 'encoded'), <<"phone_numbers">>),
            case lists:member(wnm_util:normalize_number(OutboundCID), wh_json:get_keys(AccountPhoneNumbersList)) of
                'true' ->
                   OutboundCID;
                'false' ->
                   DefaultCID = whapps_config:get(?CCCP_CONFIG_CAT, <<"default_caller_id_number">>, <<"00000000000">>),
                   lager:debug("OutboundCID ~p is out of account's list; changing to application's default: ~p", [OutboundCID, DefaultCID]),
                   DefaultCID
            end;
        'false' ->
            OutboundCID
    end.

-spec get_number(whapps_call:call()) -> {'num_to_dial', ne_binary()} | 'ok'.
get_number(Call) ->
    RedialCode = whapps_config:get(?CCCP_CONFIG_CAT, <<"last_number_redial_code">>, <<"*0">>), 
    case whapps_call_command:b_prompt_and_collect_digits(2, 12, <<"cf-enter_number">>, 3, Call) of
       {ok, RedialCode} ->
           lager:debug("Last dialed number requested"),
           get_last_dialed_number(Call);
       {ok, EnteredNumber} ->
           CleanedNumber = re:replace(EnteredNumber, "[^0-9]", "", [global, {return, 'binary'}]),
           Number = re:replace(wnm_util:to_e164(CleanedNumber), "[^0-9]", "", [global, {return, 'binary'}]),
           case wnm_util:is_reconcilable(Number) of
               'true' ->
                   lager:debug("Phone number entered: ~p. Normalized number: ~p", [EnteredNumber, Number]),
                   check_restrictions(Number, Call);
               _ ->
                   lager:debug("Wrong number entered: ~p", [EnteredNumber]),
                   whapps_call_command:prompt(<<"hotdesk-invalid_entry">>, Call),
                   whapps_call_command:queued_hangup(Call)
           end;
       Err ->
           lager:info("No Phone number obtained: ~p", [Err]),
           whapps_call_command:prompt(<<"hotdesk-invalid_entry">>, Call),
           whapps_call_command:queued_hangup(Call)
    end.

-spec get_last_dialed_number(whapps_call:call()) -> {'num_to_dial', ne_binary()} | 'ok'.
get_last_dialed_number(Call) ->
    {'ok', CachedCall} = whapps_call:retrieve(whapps_call:call_id(Call), ?APP_NAME),
    DocId = whapps_call:kvs_fetch('auth_doc_id', CachedCall),
    {'ok', Doc} = couch_mgr:open_doc(<<"cccps">>, DocId),
    LastDialed = wh_json:get_value(<<"pvt_last_dialed">>, Doc),
    case wnm_util:is_reconcilable(LastDialed) of
       'false' -> 
            whapps_call_command:prompt(<<"hotdesk-invalid_entry">>, Call),
            whapps_call_command:queued_hangup(Call);
        'true' ->
            check_restrictions(LastDialed, Call)
    end.

-spec store_last_dialed(ne_binary(), ne_binary()) -> 'ok'.
store_last_dialed(Number, DocId) ->
    {'ok', Doc} = couch_mgr:update_doc(<<"cccps">>, DocId, [{<<"pvt_last_dialed">>, Number}]),
    _ = couch_mgr:update_doc(wh_json:get_value(<<"pvt_account_db">>, Doc), DocId, [{<<"pvt_last_dialed">>, Number}]),
    'ok'.

-spec check_restrictions(ne_binary(), whapps_call:call()) -> {'num_to_dial', ne_binary()} | 'ok'. 
check_restrictions(Number, Call) ->
    {'ok', CachedCall} = whapps_call:retrieve(whapps_call:call_id(Call), ?APP_NAME),
    DocId = whapps_call:kvs_fetch('auth_doc_id', CachedCall),
    {'ok', Doc} = couch_mgr:open_doc(<<"cccps">>, DocId),
    AccountId = wh_json:get_value(<<"pvt_account_id">>, Doc),
    AccountDb = wh_json:get_value(<<"pvt_account_db">>, Doc),
    case check_doc_for_restriction(Number, AccountId, AccountDb) of
       'true' ->
            lager:debug("Number ~p is restricted", [Number]),
            whapps_call_command:prompt(<<"cf-unauthorized_call">>, Call),
            whapps_call_command:queued_hangup(Call);
       'false' ->
            UserId = wh_json:get_value(<<"user_id">>, Doc),
            case check_doc_for_restriction(Number, UserId, AccountDb) of
                'true' ->
                    lager:debug("Number ~p is restricted", [Number]),
                    whapps_call_command:prompt(<<"cf-unauthorized_call">>, Call),
                    whapps_call_command:queued_hangup(Call);
                'false' ->
                    {'num_to_dial', Number}
            end
    end.

-spec check_doc_for_restriction(ne_binary(), ne_binary(), ne_binary()) -> boolean().
check_doc_for_restriction(Number, DocId, AccountDb) ->
    case couch_mgr:open_cache_doc(AccountDb, DocId) of
        {'error', _} -> 'false';
        {'ok', JObj} ->
            Classification = wnm_util:classify_number(Number),
            lager:debug("classified number as ~p", [Classification]),
            wh_json:get_value([<<"call_restriction">>, Classification, <<"action">>], JObj) =:= <<"deny">>
    end.

