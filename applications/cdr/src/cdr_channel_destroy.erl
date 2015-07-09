%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2014, 2600Hz
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

-export([handle_req/2]).

handle_req(JObj, _Props) ->
    'true' = wapi_call:event_v(JObj),
    _ = wh_util:put_callid(JObj),
    couch_mgr:suppress_change_notice(),
    maybe_ignore(JObj).

-spec maybe_ignore(wh_json:object()) -> 'ok'.
maybe_ignore(JObj) ->
    AppName = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Application-Name">>], JObj),
    case lists:member(AppName, ?IGNORED_APP) of
        'false'-> handle_req(JObj);
        'true' ->
            lager:debug("ignoring cdr request from ~s", [AppName])
    end.

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
                ,fun save_cdr/3
               ],

    lists:foldl(fun(F, J) ->
                        F(AccountId, Timestamp, J)
                end
                ,wh_json:normalize_jobj(JObj)
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
    CCVs = wh_json:get_value(<<"custom_channel_vars">>, JObj, wh_json:new()),
    {UpdatedJobj, UpdatedCCVs} =
        wh_json:foldl(
            fun update_ccvs_foldl/3
            ,{JObj, CCVs}
            ,CCVs
        ),
    wh_json:set_value(<<"custom_channel_vars">>, UpdatedCCVs, UpdatedJobj).

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
    CallId = wh_json:get_value(<<"call_id">>, JObj),
    DocId = cdr_util:get_cdr_doc_id(Timestamp, CallId),
    wh_doc:set_id(JObj, DocId).

-spec set_call_priority(api_binary(), gregorian_seconds(), wh_json:object()) -> wh_json:object().
set_call_priority(_AccountId, _Timestamp, JObj) ->
    maybe_leak_ccv(JObj, <<"call_priority">>).

-spec set_recording_url(api_binary(), gregorian_seconds(), wh_json:object()) -> wh_json:object().
set_recording_url(_AccountId, _Timestamp, JObj) ->
    maybe_leak_ccv(JObj, <<"recording_url">>).

-spec maybe_set_e164_destination(api_binary(), gregorian_seconds(), wh_json:object()) -> wh_json:object().
maybe_set_e164_destination(_AccountId, _Timestamp, JObj) ->
    maybe_leak_ccv(JObj, <<"e164_destination">>).

-spec is_conference(api_binary(), gregorian_seconds(), wh_json:object()) -> wh_json:object().
is_conference(_AccountId, _Timestamp, JObj) ->
    maybe_leak_ccv(JObj, <<"is_conference">>, {fun wh_json:is_true/3, 'false'}).

-spec maybe_leak_ccv(wh_json:object(), wh_json:key()) -> wh_json:object().
-spec maybe_leak_ccv(wh_json:object(), wh_json:key(), {fun(), _}) -> wh_json:object().
maybe_leak_ccv(JObj, Key) ->
    maybe_leak_ccv(JObj, Key, {fun wh_json:get_value/3, 'undefined'}).

maybe_leak_ccv(JObj, Key, {GetFun, Default}) ->
    CCVKey = [<<"custom_channel_vars">>, Key],
    case GetFun(CCVKey, JObj, Default) of
        'undefined' -> JObj;
        Default -> JObj;
        Value -> wh_json:set_value(Key
                                   ,Value
                                   ,wh_json:delete_key(CCVKey, JObj)
                                  )
    end.

-spec save_cdr(api_binary(), gregorian_seconds(), wh_json:object()) -> wh_json:object().
save_cdr(_, _, JObj) ->
    CDRDb = wh_doc:account_db(JObj),
    case cdr_util:save_cdr(CDRDb, JObj) of
        {'error', 'max_retries'} ->
            lager:error("write failed to ~s, too many retries", [CDRDb]);
        'ok' -> 'ok'
    end.
