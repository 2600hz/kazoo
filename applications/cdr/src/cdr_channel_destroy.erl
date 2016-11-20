%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2016, 2600Hz
%%% @doc
%%% Listen for CDR events and record them to the database
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Edouard Swiac
%%%   Ben Wann
%%%   KAZOO-3596: Sponsored by GTNetwork LLC, implemented by SIPLABS LLC
%%%-------------------------------------------------------------------
-module(cdr_channel_destroy).

-export([handle_req/2]).

-include("cdr.hrl").

-define(IGNORED_APP, kapps_config:get(?CONFIG_CAT, <<"ignore_apps">>, [<<"milliwatt">>])).

-define(LOOPBACK_KEY, <<"ignore_loopback_bowout">>).
-define(IGNORE_LOOPBACK(AccountId),
        case AccountId of
            'undefined' -> kapps_config:get(?CONFIG_CAT, ?LOOPBACK_KEY, 'true');
            _ -> kapps_account_config:get_global(AccountId, ?CONFIG_CAT, ?LOOPBACK_KEY, 'true')
        end).

-define(CHANNEL_VARS, <<"Custom-Channel-Vars">>).
-define(CCV(Key), [?CHANNEL_VARS, Key]).

-spec handle_req(kz_json:object(), kz_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = kapi_call:event_v(JObj),
    _ = kz_util:put_callid(JObj),
    kz_datamgr:suppress_change_notice(),
    Routines = [fun maybe_ignore_app/1
               ,fun maybe_ignore_loopback/1
               ],
    case lists:foldl(fun maybe_ignore_cdr/2, {JObj, []}, Routines) of
        {_, []} -> handle_req(JObj);
        {_, List} ->
            lists:foreach(fun (M) -> lager:debug("~s", [M]) end, List)
    end.

-spec maybe_ignore_cdr(fun(), {kz_json:object(), list()}) -> {kz_json:object(), list()}.
maybe_ignore_cdr(Fun, {JObj, Acc}) ->
    case Fun(JObj) of
        {'true', M} -> {JObj, [M | Acc]};
        _ -> {JObj, Acc}
    end.

-spec maybe_ignore_app(kz_json:object()) -> {boolean(), binary()}.
maybe_ignore_app(JObj) ->
    AppName = kz_util:to_binary(kz_call_event:application_name(JObj)),
    {lists:member(AppName, ?IGNORED_APP)
    ,<<"ignoring cdr request from ", AppName/binary>>
    }.

-spec maybe_ignore_loopback(kz_json:object()) -> {boolean(), binary()}.
maybe_ignore_loopback(JObj) ->
    {kz_util:is_true(?IGNORE_LOOPBACK(kz_call_event:account_id(JObj)))
     andalso kz_json:is_true(<<"Channel-Is-Loopback">>, JObj)
     andalso kz_json:is_true(<<"Channel-Loopback-Bowout">>, JObj)
     andalso kz_json:is_true(<<"Channel-Loopback-Bowout-Execute">>, JObj)
     andalso (is_normal_hangup_cause(kz_call_event:hangup_cause(JObj))
              orelse kz_json:get_ne_binary_value(<<"Channel-Loopback-Leg">>, JObj) =/= <<"B">>
             ),
     <<"ignoring cdr request for loopback channel">>
    }.

-spec is_normal_hangup_cause(api_binary()) -> boolean().
is_normal_hangup_cause('undefined') -> 'true';
is_normal_hangup_cause(<<"NORMAL", _/binary>>) -> 'true';
is_normal_hangup_cause(_) -> 'false'.

-spec handle_req(kz_json:object()) -> 'ok'.
handle_req(JObj) ->
    AccountId = kz_call_event:account_id(JObj),
    Timestamp = kz_call_event:timestamp(JObj),
    prepare_and_save(AccountId, Timestamp, JObj).

-spec prepare_and_save(account_id(), gregorian_seconds(), kz_json:object()) -> 'ok'.
prepare_and_save(AccountId, Timestamp, JObj) ->
    Routines = [fun update_pvt_parameters/3
               ,fun update_ccvs/3
               ,fun set_doc_id/3
               ,fun set_recording_url/3
               ,fun set_call_priority/3
               ,fun maybe_set_e164_destination/3
               ,fun is_conference/3
               ,fun set_interaction/3
               ,fun save_cdr/3
               ],
    _ = lists:foldl(fun(F, J) -> F(AccountId, Timestamp, J) end
                   ,JObj
                   ,Routines
                   ),
    'ok'.

-spec update_pvt_parameters(api_binary(), gregorian_seconds(), kz_json:object()) -> kz_json:object().
update_pvt_parameters('undefined', _, JObj) ->
    Props = [{'type', 'cdr'}
            ,{'crossbar_doc_vsn', 2}
            ],
    kz_doc:update_pvt_parameters(JObj, ?KZ_ANONYMOUS_CDR_DB, Props);
update_pvt_parameters(AccountId, Timestamp, JObj) ->
    AccountMODb = kz_util:format_account_id(AccountId, Timestamp),
    Props = [{'type', 'cdr'}
            ,{'crossbar_doc_vsn', 2}
            ,{'account_id', AccountId}
            ],
    kz_doc:update_pvt_parameters(JObj, AccountMODb, Props).

-spec update_ccvs(api_binary(), gregorian_seconds(), kz_json:object()) -> kz_json:object().
update_ccvs(_, _, JObj) ->
    CCVs = kz_call_event:custom_channel_vars(JObj, kz_json:new()),
    {UpdatedJobj, UpdatedCCVs} =
        kz_json:foldl(fun update_ccvs_foldl/3
                     ,{JObj, CCVs}
                     ,CCVs
                     ),
    kz_json:set_value(?CHANNEL_VARS, UpdatedCCVs, UpdatedJobj).

-spec update_ccvs_foldl(kz_json:path(), kz_json:json_term(), {kz_json:object(), kz_json:object()}) ->
                               {kz_json:object(), kz_json:object()}.
update_ccvs_foldl(Key, Value,  {JObj, CCVs}=Acc) ->
    case kz_json:is_private_key(Key) of
        'false' -> Acc;
        'true' ->
            {kz_json:set_value(Key, Value, JObj)
            ,kz_json:delete_key(Key, CCVs)
            }
    end.

-spec set_doc_id(api_binary(), gregorian_seconds(), kz_json:object()) -> kz_json:object().
set_doc_id(_, Timestamp, JObj) ->
    CallId = kz_call_event:call_id(JObj),
    %% we should consider this because there is a lost channel in case of
    %% nightmare transfers
    %%    CallId = kz_util:rand_hex_binary(16),
    DocId = cdr_util:get_cdr_doc_id(Timestamp, CallId),
    kz_doc:set_id(JObj, DocId).

-spec set_call_priority(api_binary(), gregorian_seconds(), kz_json:object()) -> kz_json:object().
set_call_priority(_AccountId, _Timestamp, JObj) ->
    maybe_leak_ccv(JObj, <<"Call-Priority">>).

-spec set_recording_url(api_binary(), gregorian_seconds(), kz_json:object()) -> kz_json:object().
set_recording_url(_AccountId, _Timestamp, JObj) ->
    maybe_leak_ccv(JObj, <<"Recording-Url">>).

-spec maybe_set_e164_destination(api_binary(), gregorian_seconds(), kz_json:object()) -> kz_json:object().
maybe_set_e164_destination(_AccountId, _Timestamp, JObj) ->
    maybe_leak_ccv(JObj, <<"E164-Destination">>).

-spec is_conference(api_binary(), gregorian_seconds(), kz_json:object()) -> kz_json:object().
is_conference(_AccountId, _Timestamp, JObj) ->
    maybe_leak_ccv(JObj, <<"Is-Conference">>, {fun kz_json:is_true/3, 'false'}).

-spec maybe_leak_ccv(kz_json:object(), kz_json:path()) -> kz_json:object().
-spec maybe_leak_ccv(kz_json:object(), kz_json:path(), {fun(), any()}) -> kz_json:object().
maybe_leak_ccv(JObj, Key) ->
    maybe_leak_ccv(JObj, Key, {fun kz_json:get_value/3, 'undefined'}).

maybe_leak_ccv(JObj, Key, {GetFun, Default}) ->
    case GetFun(?CCV(Key), JObj, Default) of
        'undefined' -> JObj;
        Default -> JObj;
        Value -> kz_json:set_value(Key
                                  ,Value
                                  ,kz_json:delete_key(?CCV(Key), JObj)
                                  )
    end.

-spec set_interaction(api_binary(), gregorian_seconds(), kz_json:object()) ->
                             kz_json:object().
set_interaction(_AccountId, _Timestamp, JObj) ->
    Interaction = kz_call_event:custom_channel_var(JObj, <<?CALL_INTERACTION_ID>>, ?CALL_INTERACTION_DEFAULT),
    <<Time:11/binary, "-", Key/binary>> = Interaction,
    Timestamp = kz_util:to_integer(Time),
    CallId = kz_call_event:call_id(JObj),
    DocId = cdr_util:get_cdr_doc_id(Timestamp, CallId),

    kz_json:set_values([{<<"Interaction-Time">>, Timestamp}
                       ,{<<"Interaction-Key">>, Key}
                       ,{<<"Interaction-Id">>, Interaction}
                       ]
                      ,kz_json:delete_key(?CCV(<<?CALL_INTERACTION_ID>>), kz_doc:set_id(JObj, DocId))
                      ).

-spec save_cdr(api_binary(), gregorian_seconds(), kz_json:object()) -> kz_json:object().
save_cdr(_, _, JObj) ->
    CDRDb = kz_doc:account_db(JObj),
    case cdr_util:save_cdr(CDRDb, kz_json:normalize_jobj(JObj)) of
        {'error', 'max_retries'} ->
            lager:error("write failed to ~s, too many retries", [CDRDb]);
        'ok' -> 'ok'
    end.
