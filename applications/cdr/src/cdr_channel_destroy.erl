%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Listen for CDR events and record them to the database
%%% @author James Aimonetti
%%% @author Edouard Swiac
%%% @author Ben Wann
%%% @author Sponsored by GTNetwork LLC, Implemented by SIPLABS LLC
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cdr_channel_destroy).

-export([handle_req/2]).

-include("cdr.hrl").

-define(IGNORED_APP, kapps_config:get(?CONFIG_CAT, <<"ignore_apps">>, [<<"milliwatt">>])).

-define(LOOPBACK_KEY, <<"ignore_loopback_bowout">>).
-define(IGNORE_LOOPBACK(AccountId),
        case AccountId of
            'undefined' -> kapps_config:get_is_true(?CONFIG_CAT, ?LOOPBACK_KEY, 'true');
            _ -> kapps_account_config:get_global(AccountId, ?CONFIG_CAT, ?LOOPBACK_KEY, 'true')
        end).

-define(CHANNEL_VARS, <<"Custom-Channel-Vars">>).
-define(CCV(Key), [?CHANNEL_VARS, Key]).

-spec handle_req(kz_call_event:doc(), kz_term:proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = kapi_call:event_v(JObj),
    _ = kz_log:put_callid(JObj),
    Routines = [fun maybe_ignore_app/1
               ,fun maybe_ignore_loopback/1
               ],
    case lists:foldl(fun maybe_ignore_cdr/2, {JObj, []}, Routines) of
        {_, []} -> handle_req(JObj);
        {_, List} ->
            lists:foreach(fun(M) -> lager:debug("~s", [M]) end, List)
    end.

-spec maybe_ignore_cdr(fun(), {kz_call_event:doc(), list()}) -> {kz_call_event:doc(), list()}.
maybe_ignore_cdr(Fun, {JObj, Acc}) ->
    case Fun(JObj) of
        {'true', M} -> {JObj, [M | Acc]};
        _ -> {JObj, Acc}
    end.

-spec maybe_ignore_app(kz_call_event:doc()) -> {boolean(), binary()}.
maybe_ignore_app(JObj) ->
    AppName = kz_term:to_binary(kz_call_event:application_name(JObj)),
    {lists:member(AppName, ?IGNORED_APP)
    ,<<"ignoring cdr request from ", AppName/binary>>
    }.

-spec maybe_ignore_loopback(kz_call_event:doc()) -> {boolean(), binary()}.
maybe_ignore_loopback(JObj) ->
    {kz_term:is_true(?IGNORE_LOOPBACK(kz_call_event:account_id(JObj)))
     andalso kz_json:is_true(<<"Channel-Is-Loopback">>, JObj)
     andalso kz_json:is_true(<<"Channel-Loopback-Bowout">>, JObj)
     andalso kz_json:is_true(<<"Channel-Loopback-Bowout-Execute">>, JObj)
     andalso (is_normal_hangup_cause(kz_call_event:hangup_cause(JObj))
              orelse kz_json:get_ne_binary_value(<<"Channel-Loopback-Leg">>, JObj) =/= <<"B">>
             ),
     <<"ignoring cdr request for loopback channel">>
    }.

-spec is_normal_hangup_cause(kz_term:api_ne_binary()) -> boolean().
is_normal_hangup_cause('undefined') -> 'true';
is_normal_hangup_cause(<<"NORMAL", _/binary>>) -> 'true';
is_normal_hangup_cause(_) -> 'false'.

-spec handle_req(kz_call_event:doc()) -> 'ok'.
handle_req(JObj) ->
    AccountId = kz_call_event:account_id(JObj),
    Timestamp = kz_call_event:timestamp(JObj),
    prepare_and_save(AccountId, Timestamp, JObj).

-spec prepare_and_save(account_id(), kz_time:gregorian_seconds(), kz_call_event:doc()) -> 'ok'.
prepare_and_save(AccountId, Timestamp, JObj) ->
    %% Caution: The Timestamp is ahead of interaction-timestamp.
    %%          {@link set_interaction/3} is setting Id based on interaction-timestamp.
    %%          So there is an edge case for the midnight between two months. If interaction time is less than midnight
    %%          and Timestamp is falling to after midnight (which is next month) the document ends up in the next month MODB
    %%          but the document ID is for previous month. Which breaks cb_cdrs and interaction view.

    Routines = [fun update_ccvs/3
               ,fun set_doc_id/3
               ,fun set_recording_url/3
               ,fun set_call_priority/3
               ,fun maybe_set_e164_destination/3
               ,fun maybe_set_e164_origination/3
               ,fun maybe_set_did_classifier/3
               ,fun is_conference/3
               ,fun set_interaction/3
               ,fun update_pvt_parameters/3 %% due to interaction-timestamp MUST be called LAST
               ,fun save_cdr/3
               ],
    _ = lists:foldl(fun(F, J) -> F(AccountId, Timestamp, J) end
                   ,JObj
                   ,Routines
                   ),
    'ok'.

-spec update_pvt_parameters(kz_term:api_ne_binary(), kz_time:gregorian_seconds(), kz_call_event:doc()) ->
                                   kz_call_event:doc().
update_pvt_parameters('undefined', _, JObj) ->
    Props = [{'type', 'cdr'}
            ,{'crossbar_doc_vsn', 2}
            ],
    kz_doc:update_pvt_parameters(JObj, ?KZ_ANONYMOUS_CDR_DB, Props);
update_pvt_parameters(AccountId, Timestamp, JObj) ->
    CorrectTimestamp = kz_json:get_integer_value(<<"Interaction-Time">>, JObj, Timestamp),
    AccountMODb = kz_util:format_account_id(AccountId, CorrectTimestamp),
    Props = [{'type', 'cdr'}
            ,{'crossbar_doc_vsn', 2}
            ,{'account_id', AccountId}
            ],
    kz_doc:update_pvt_parameters(JObj, AccountMODb, Props).

-spec update_ccvs(kz_term:api_ne_binary(), kz_time:gregorian_seconds(), kz_call_event:doc()) ->
                         kz_call_event:doc().
update_ccvs(_, _, JObj) ->
    CCVs = kz_call_event:custom_channel_vars(JObj, kz_json:new()),
    {UpdatedJobj, UpdatedCCVs} =
        kz_json:foldl(fun update_ccvs_foldl/3
                     ,{JObj, CCVs}
                     ,CCVs
                     ),
    kz_json:set_value(?CHANNEL_VARS, UpdatedCCVs, UpdatedJobj).

-spec update_ccvs_foldl(kz_json:get_key(), kz_json:json_term(), {kz_call_event:doc(), kz_json:object()}) ->
                               {kz_call_event:doc(), kz_json:object()}.
update_ccvs_foldl(Key, Value,  {JObj, CCVs}=Acc) ->
    case kz_doc:is_private_key(Key) of
        'false' -> Acc;
        'true' ->
            {kz_json:set_value(Key, Value, JObj)
            ,kz_json:delete_key(Key, CCVs)
            }
    end.

-spec set_doc_id(kz_term:api_ne_binary(), kz_time:gregorian_seconds(), kz_call_event:doc()) ->
                        kz_call_event:doc().
set_doc_id(_AcctId, Timestamp, JObj) ->
    CallId = kz_call_event:call_id(JObj),
    %% we should consider this because there is a lost channel in case of
    %% nightmare transfers
    %%    CallId = kz_binary:rand_hex(16),
    %%
    %% Caution: The Timestamp is ahead of interaction-timestamp.
    %%          {@link set_interaction/3} is setting Id based on interaction-timestamp.
    %%          So there is an edge case for the midnight between two months. If interaction time is less than midnight
    %%          and Timestamp is falling to after midnight (which is next month) the document ends up in the next month MODB
    %%          but the document ID is for previous month. Which breaks cb_cdrs and interaction view.
    DocId = cdr_util:get_cdr_doc_id(Timestamp, CallId),
    kz_doc:set_id(JObj, DocId).

-spec set_call_priority(kz_term:api_ne_binary(), kz_time:gregorian_seconds(), kz_call_event:doc()) -> kz_call_event:doc().
set_call_priority(_AccountId, _Timestamp, JObj) ->
    maybe_leak_ccv(JObj, <<"Call-Priority">>).

-spec set_recording_url(kz_term:api_ne_binary(), kz_time:gregorian_seconds(), kz_call_event:doc()) -> kz_call_event:doc().
set_recording_url(_AccountId, _Timestamp, JObj) ->
    maybe_leak_ccv(JObj, <<"Recording-Url">>).

-spec maybe_set_e164_destination(kz_term:api_ne_binary(), kz_time:gregorian_seconds(), kz_call_event:doc()) -> kz_call_event:doc().
maybe_set_e164_destination(_AccountId, _Timestamp, JObj) ->
    maybe_leak_ccv(JObj, <<"E164-Destination">>).

-spec maybe_set_e164_origination(kz_term:api_ne_binary(), kz_time:gregorian_seconds(), kz_call_event:doc()) -> kz_call_event:doc().
maybe_set_e164_origination(_AccountId, _Timestamp, JObj) ->
    maybe_leak_ccv(JObj, <<"E164-Origination">>).

-spec maybe_set_did_classifier(kz_term:api_ne_binary(), kz_time:gregorian_seconds(), kz_call_event:doc()) -> kz_call_event:doc().
maybe_set_did_classifier(_AccountId, _Timestamp, JObj) ->
    maybe_leak_ccv(JObj, <<"DID-Classifier">>).

-spec is_conference(kz_term:api_ne_binary(), kz_time:gregorian_seconds(), kz_call_event:doc()) -> kz_call_event:doc().
is_conference(_AccountId, _Timestamp, JObj) ->
    maybe_leak_ccv(JObj, <<"Is-Conference">>, {fun kz_json:is_true/3, 'false'}).

-spec maybe_leak_ccv(kz_call_event:doc(), kz_json:get_key()) -> kz_call_event:doc().
maybe_leak_ccv(JObj, Key) ->
    maybe_leak_ccv(JObj, Key, {fun kz_json:get_value/3, 'undefined'}).

-spec maybe_leak_ccv(kz_call_event:doc(), kz_json:get_key(), {fun(), any()}) -> kz_call_event:doc().
maybe_leak_ccv(JObj, Key, {GetFun, Default}) ->
    case GetFun(?CCV(Key), JObj, Default) of
        'undefined' -> JObj;
        Default -> JObj;
        Value -> kz_json:set_value(Key
                                  ,Value
                                  ,kz_json:delete_key(?CCV(Key), JObj)
                                  )
    end.

-spec set_interaction(kz_term:api_ne_binary(), kz_time:gregorian_seconds(), kz_call_event:doc()) ->
                             kz_call_event:doc().
set_interaction(_AccountId, _Timestamp, JObj) ->
    %% See {@link prepare_and_save/3} for an edge case for Timestamp
    Interaction = kz_call_event:custom_channel_var(JObj, <<?CALL_INTERACTION_ID>>, ?CALL_INTERACTION_DEFAULT),
    <<Time:11/binary, "-", Key/binary>> = Interaction,
    Timestamp = kz_term:to_integer(Time),
    CallId = kz_call_event:call_id(JObj),
    DocId = cdr_util:get_cdr_doc_id(Timestamp, CallId),

    kz_json:set_values([{<<"Interaction-Time">>, Timestamp}
                       ,{<<"Interaction-Key">>, Key}
                       ,{<<"Interaction-Id">>, Interaction}
                       ]
                      ,kz_json:delete_key(?CCV(<<?CALL_INTERACTION_ID>>), kz_doc:set_id(JObj, DocId))
                      ).

-spec save_cdr(kz_term:api_ne_binary(), kz_time:gregorian_seconds(), kz_call_event:doc()) -> 'ok'.
save_cdr(_AcctId, _Timestamp, JObj) ->
    CDRDb = kz_doc:account_db(JObj),
    case cdr_util:save_cdr(CDRDb, kz_json:normalize_jobj(JObj)) of
        {'error', 'max_save_retries'} ->
            lager:error("write failed to ~s, too many retries", [CDRDb]);
        'ok' -> 'ok'
    end.
