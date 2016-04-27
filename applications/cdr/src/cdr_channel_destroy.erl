%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
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

-include("cdr.hrl").

-define(IGNORED_APP, whapps_config:get(?CONFIG_CAT, <<"ignore_apps">>, [<<"milliwatt">>])).

-define(LOOPBACK_KEY, <<"ignore_loopback_bowout">>).
-define(IGNORE_LOOPBACK(AccountId), whapps_account_config:get_global(AccountId, ?CONFIG_CAT, ?LOOPBACK_KEY, 'true')).

-define(CHANNEL_VARS, <<"Custom-Channel-Vars">>).
-define(CCV(Key), [?CHANNEL_VARS, Key]).

-export([handle_req/2]).

handle_req(JObj, _Props) ->
    'true' = wapi_call:event_v(JObj),
    _ = wh_util:put_callid(JObj),
    kz_datamgr:suppress_change_notice(),
    Routines = [fun maybe_ignore_app/1
                ,fun maybe_ignore_loopback/1
               ],
    case lists:foldl(fun maybe_ignore_cdr/2, {JObj, []}, Routines) of
        {_, []} -> handle_req(JObj);
        {_, List} -> [lager:debug("~s", [M]) || M <- List]
    end.

-spec maybe_ignore_cdr(fun(), {wh_json:object(), list()}) -> {wh_json:object(), list()}.
maybe_ignore_cdr(Fun, {JObj, Acc}) ->
    case Fun(JObj) of
        {'true', M} -> {JObj, [M | Acc]};
        _ -> {JObj, Acc}
    end.

-spec maybe_ignore_app(wh_json:object()) -> {boolean(), binary()}.
maybe_ignore_app(JObj) ->
    AppName = wh_util:to_binary(kz_call_event:application_name(JObj)),
    {lists:member(AppName, ?IGNORED_APP),
     <<"ignoring cdr request from ", AppName/binary>>
    }.

-spec maybe_ignore_loopback(wh_json:object()) -> {boolean(), binary()}.
maybe_ignore_loopback(JObj) ->
    {wh_util:is_true(?IGNORE_LOOPBACK(kz_call_event:account_id(JObj))) andalso
         wh_json:is_true(<<"Channel-Is-Loopback">>, JObj) andalso
         wh_json:is_true(<<"Channel-Loopback-Bowout">>, JObj) andalso
         wh_json:is_true(<<"Channel-Loopback-Bowout-Execute">>, JObj) andalso
         (is_normal_hangup_cause(kz_call_event:hangup_cause(JObj)) orelse
              wh_json:get_ne_binary_value(<<"Channel-Loopback-Leg">>, JObj) =/= <<"B">>),
     <<"ignoring cdr request for loopback channel">>
    }.

-spec is_normal_hangup_cause(api_binary()) -> boolean().
is_normal_hangup_cause('undefined') -> 'true';
is_normal_hangup_cause(<<"NORMAL", _/binary>>) -> 'true';
is_normal_hangup_cause(_) -> 'false'.

-spec handle_req(wh_json:object()) -> 'ok'.
handle_req(JObj) ->
    AccountId = kz_call_event:account_id(JObj),
    Timestamp = kz_call_event:timestamp(JObj),
    prepare_and_save(AccountId, Timestamp, JObj).

-spec prepare_and_save(account_id(), gregorian_seconds(), wh_json:object()) -> 'ok'.
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

    lists:foldl(fun(F, J) ->
                        F(AccountId, Timestamp, J)
                end
                ,JObj
                ,Routines
               ),
    'ok'.

-spec update_pvt_parameters(api_binary(), gregorian_seconds(), wh_json:object()) -> wh_json:object().
update_pvt_parameters('undefined', _, JObj) ->
    Props = [{'type', 'cdr'}
             ,{'crossbar_doc_vsn', 2}
            ],
    wh_doc:update_pvt_parameters(JObj, ?WH_ANONYMOUS_CDR_DB, Props);
update_pvt_parameters(AccountId, Timestamp, JObj) ->
    AccountMODb = wh_util:format_account_id(AccountId, Timestamp),
    Props = [{'type', 'cdr'}
             ,{'crossbar_doc_vsn', 2}
             ,{'account_id', AccountId}
            ],
    wh_doc:update_pvt_parameters(JObj, AccountMODb, Props).

-spec update_ccvs(api_binary(), gregorian_seconds(), wh_json:object()) -> wh_json:object().
update_ccvs(_, _, JObj) ->
    CCVs = kz_call_event:custom_channel_vars(JObj, wh_json:new()),
    {UpdatedJobj, UpdatedCCVs} =
        wh_json:foldl(
            fun update_ccvs_foldl/3
            ,{JObj, CCVs}
            ,CCVs
        ),
    wh_json:set_value(?CHANNEL_VARS, UpdatedCCVs, UpdatedJobj).

-spec update_ccvs_foldl(wh_json:key(), wh_json:json_term(), {wh_json:object(), wh_json:object()}) ->
                               {wh_json:object(), wh_json:object()}.
update_ccvs_foldl(Key, Value,  {JObj, CCVs}=Acc) ->
    case wh_json:is_private_key(Key) of
        'false' -> Acc;
        'true' ->
            {wh_json:set_value(Key, Value, JObj)
             ,wh_json:delete_key(Key, CCVs)
            }
    end.

-spec set_doc_id(api_binary(), gregorian_seconds(), wh_json:object()) -> wh_json:object().
set_doc_id(_, Timestamp, JObj) ->
    CallId = kz_call_event:call_id(JObj),
%% we should consider this because there is a lost channel in case of
%% nightmare transfers
%%    CallId = wh_util:rand_hex_binary(16),
    DocId = cdr_util:get_cdr_doc_id(Timestamp, CallId),
    wh_doc:set_id(JObj, DocId).

-spec set_call_priority(api_binary(), gregorian_seconds(), wh_json:object()) -> wh_json:object().
set_call_priority(_AccountId, _Timestamp, JObj) ->
    maybe_leak_ccv(JObj, <<"Call-Priority">>).

-spec set_recording_url(api_binary(), gregorian_seconds(), wh_json:object()) -> wh_json:object().
set_recording_url(_AccountId, _Timestamp, JObj) ->
    maybe_leak_ccv(JObj, <<"Recording-Url">>).

-spec maybe_set_e164_destination(api_binary(), gregorian_seconds(), wh_json:object()) -> wh_json:object().
maybe_set_e164_destination(_AccountId, _Timestamp, JObj) ->
    maybe_leak_ccv(JObj, <<"E164-Destination">>).

-spec is_conference(api_binary(), gregorian_seconds(), wh_json:object()) -> wh_json:object().
is_conference(_AccountId, _Timestamp, JObj) ->
    maybe_leak_ccv(JObj, <<"Is-Conference">>, {fun wh_json:is_true/3, 'false'}).

-spec maybe_leak_ccv(wh_json:object(), wh_json:key()) -> wh_json:object().
-spec maybe_leak_ccv(wh_json:object(), wh_json:key(), {fun(), any()}) -> wh_json:object().
maybe_leak_ccv(JObj, Key) ->
    maybe_leak_ccv(JObj, Key, {fun wh_json:get_value/3, 'undefined'}).

maybe_leak_ccv(JObj, Key, {GetFun, Default}) ->
    case GetFun(?CCV(Key), JObj, Default) of
        'undefined' -> JObj;
        Default -> JObj;
        Value -> wh_json:set_value(Key
                                   ,Value
                                   ,wh_json:delete_key(?CCV(Key), JObj)
                                  )
    end.

-spec set_interaction(api_binary(), gregorian_seconds(), wh_json:object()) ->
                       wh_json:object().
set_interaction(_AccountId, _Timestamp, JObj) ->
    <<Time:11/binary, "-", Key/binary>> = Interaction = kz_call_event:custom_channel_var(JObj, <<?CALL_INTERACTION_ID>>),
    Timestamp = wh_util:to_integer(Time),
    CallId = kz_call_event:call_id(JObj),
    DocId = cdr_util:get_cdr_doc_id(Timestamp, CallId),

    wh_json:set_values(
      [{<<"Interaction-Time">>, Timestamp}
       ,{<<"Interaction-Key">>, Key}
       ,{<<"Interaction-Id">>, Interaction}
      ]
      ,wh_json:delete_key(?CCV(<<?CALL_INTERACTION_ID>>), wh_doc:set_id(JObj, DocId))
     ).

-spec save_cdr(api_binary(), gregorian_seconds(), wh_json:object()) -> wh_json:object().
save_cdr(_, _, JObj) ->
    CDRDb = wh_doc:account_db(JObj),
    case cdr_util:save_cdr(CDRDb, wh_json:normalize_jobj(JObj)) of
        {'error', 'max_retries'} ->
            lager:error("write failed to ~s, too many retries", [CDRDb]);
        'ok' -> 'ok'
    end.
