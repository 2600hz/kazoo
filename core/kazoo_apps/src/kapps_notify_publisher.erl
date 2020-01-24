%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2017-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapps_notify_publisher).

-export([call_collect/2
        ,cast/2

        ,collecting/1
        ,is_completed/1
        ]).

-include_lib("kazoo_apps.hrl").

-define(NOTIFY_CAT, <<"notify">>).

-define(DEFAULT_TIMEOUT, 30 * ?MILLISECONDS_IN_SECOND).
-define(TIMEOUT, kapps_config:get_pos_integer(?NOTIFY_CAT, <<"notify_publisher_timeout_ms">>, ?DEFAULT_TIMEOUT)).

-define(DEFAULT_PUBLISHER_ENABLED
       ,kapps_config:get_is_true(?NOTIFY_CAT, <<"notify_persist_enabled">>, 'true')
       ).

-define(ACCOUNT_SHOULD_PERSIST(AccountId)
       ,kapps_account_config:get_global(AccountId, ?NOTIFY_CAT, <<"should_persist_for_retry">>, 'true')
       ).

-define(DEFAULT_TYPE_EXCEPTION, [<<"system_alert">>
                                ,<<"voicemail_save">>
                                ,<<"register">>
                                ,<<"webhook">>
                                ]).
-define(GLOBAL_FORCE_NOTIFY_TYPE_EXCEPTION
       ,kapps_config:get_ne_binaries(?NOTIFY_CAT, <<"notify_persist_temporary_force_exceptions">>, [])
       ).

-define(NOTIFY_TYPE_EXCEPTION(AccountId)
       ,kapps_account_config:get_global(AccountId, ?NOTIFY_CAT, <<"notify_persist_exceptions">>, ?DEFAULT_TYPE_EXCEPTION)
       ).

-define(DEFAULT_RETRY_PERIOD
       ,kapps_config:get_integer(<<"tasks.notify_resend">>, <<"retry_after_fudge_s">>, 10 * ?SECONDS_IN_MINUTE)
       ).

-type failure_reason() :: {kz_term:ne_binary(), kz_term:api_object()}.

%%------------------------------------------------------------------------------
%% @doc Publish notification and collect notify update messages from
%% teletype. Useful if you want to make sure teletype processed
%% the notification completely (e.g. new voicemail).
%% @end
%%------------------------------------------------------------------------------
-spec call_collect(kz_term:api_terms(), kz_amqp_worker:publish_fun()) -> kz_amqp_worker:request_return().
call_collect(Req, PublishFun) ->
    NotifyType = notify_type(PublishFun),
    CallResp = kz_amqp_worker:call_collect(Req, PublishFun, fun collecting/1, ?TIMEOUT),
    handle_resp(NotifyType, Req, CallResp),
    CallResp.

%%------------------------------------------------------------------------------
%% @doc Publish notification asynchronous, and save the payload to db
%% if it failed.
%% @end
%%------------------------------------------------------------------------------
-spec cast(kz_term:api_terms(), kz_amqp_worker:publish_fun()) -> 'ok'.
cast(Req, PublishFun) ->
    CallId = kz_log:get_callid(),
    Fun = fun() ->
                  _ = case kz_term:is_ne_binary(CallId) of
                          'true' -> kz_log:put_callid(CallId);
                          'false' -> kz_log:put_callid(Req)
                      end,
                  call_collect(Req, PublishFun)
          end,
    _ = erlang:spawn(Fun),
    'ok'.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Handle AMQP worker responses.
%% @end
%%------------------------------------------------------------------------------
-spec handle_resp(kz_term:api_ne_binary(), kz_term:api_terms(), kz_amqp_worker:request_return()) -> 'ok'.
handle_resp(NotifyType, Req, {'ok', _}=Resp) ->
    check_for_failure(NotifyType, Req, Resp);

handle_resp(NotifyType, Req, {'error', Error}) ->
    maybe_handle_error(NotifyType, Req, handle_amqp_worker_error(Error));

handle_resp(NotifyType, Req, {'returned', _, Resp}) ->
    check_for_failure(NotifyType, Req, {'returned', [Resp]});

handle_resp(NotifyType, Req, {'timeout', _}=Resp) ->
    check_for_failure(NotifyType, Req, Resp).

%%------------------------------------------------------------------------------
%% @doc Check for notify update messages from teletype/notify applications.
%% @end
%%------------------------------------------------------------------------------
-spec check_for_failure(kz_term:api_ne_binary(), kz_term:api_terms(), {'ok' | 'returned' | 'timeout', kz_json:object() | kz_json:objects()}) -> 'ok'.
check_for_failure(NotifyType, Req, {_ErrorType, Responses}=Resp) ->
    Reason = json_to_reason(Resp),
    case is_completed(Responses) of
        'true' -> maybe_log_metadata(NotifyType, Reason);
        'false' -> maybe_handle_error(NotifyType, Req, Reason)
    end.

-spec maybe_log_metadata(kz_term:api_ne_binary(), failure_reason()) -> 'ok'.
maybe_log_metadata('undefined', _) -> 'ok';
maybe_log_metadata(_, {<<"completed">>, 'undefined'}) -> 'ok';
maybe_log_metadata(NotifyType, {<<"completed">>=Reason, Metadata}) ->
    lager:debug("publishing ~s resulted in ~s. full result: ~p", [NotifyType, Reason, Metadata]);
maybe_log_metadata(_, _) -> 'ok'.


%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_handle_error(kz_term:api_binary(), kz_term:api_terms(), failure_reason()) -> 'ok'.
maybe_handle_error('undefined', _, _) ->
    lager:warning("not saving unknown notification type");
maybe_handle_error(NotifyType, Req, Reason) ->
    AccountId = kapi_notifications:account_id(Req),
    should_persist_notify(AccountId)
        andalso should_handle_notify_type(NotifyType, AccountId)
        andalso handle_error(NotifyType, Req, Reason).

%%------------------------------------------------------------------------------
%% @doc create document with notification payload to save in db.
%% @end
%%------------------------------------------------------------------------------
-spec handle_error(kz_term:ne_binary(), kz_term:api_terms(), failure_reason()) -> 'ok'.
handle_error(NotifyType, Req, {Reason, Metadata}) ->
    lager:warning("failed to publish notification ~s: ~p  , saving the payload...", [NotifyType, Reason]),
    Props = props:filter_undefined(
              [{<<"description">>, <<"failed to publish notification">>}
              ,{<<"failure_reason">>, Reason}
              ,{<<"metadata">>, Metadata}
              ,{<<"notification_type">>, NotifyType}
              ,{<<"payload">>, Req}
              ,{<<"attempts">>, 1}
              ,{<<"retry_after">>, ?DEFAULT_RETRY_PERIOD}
              ]),
    PvtOptions = [{'type', <<"failed_notify">>}
                 ,{'account_id', kapi_notifications:account_id(Req)}
                 ,{'account_db', ?KZ_PENDING_NOTIFY_DB}
                 ],
    JObj = kz_doc:update_pvt_parameters(kz_json:from_list_recursive(Props), 'undefined', PvtOptions),
    save_pending_notification(NotifyType, JObj, 2).

-spec save_pending_notification(kz_term:ne_binary(), kz_json:object(), integer()) -> 'ok'.
save_pending_notification(_NotifyType, _JObj, Loop) when Loop < 0 ->
    lager:error("max try to save payload for notification ~s", [_NotifyType]);
save_pending_notification(NotifyType, JObj, Loop) ->
    case kz_datamgr:save_doc(?KZ_PENDING_NOTIFY_DB, JObj) of
        {'ok', _SavedJObj} ->
            lager:warning("payload for notification ~s is saved to ~s", [NotifyType, kz_doc:id(_SavedJObj)]);
        {'error', 'timeout'} ->
            save_pending_notification(NotifyType, JObj, Loop - 1);
        {'error', 'conflict'} ->
            save_pending_notification(NotifyType, JObj, Loop - 1);
        {'error', _E} ->
            lager:error("failed to save payload for notification ~s: ~p", [NotifyType, _E])
    end.

%%------------------------------------------------------------------------------
%% @doc Collect responses until failed or completed messages are received.
%% @end
%%------------------------------------------------------------------------------
-spec collecting(kz_json:objects()) -> boolean().
collecting([JObj|_]) ->
    case kapi_notifications:notify_update_v(JObj)
        andalso kz_json:get_ne_binary_value(<<"Status">>, JObj)
    of
        <<"completed">> -> 'true';
        <<"disabled">> -> 'true';
        <<"failed">> -> 'true';
        <<"ignored">> -> 'true';
        _ -> 'false'
    end.

%%------------------------------------------------------------------------------
%% @doc Check responses from teletype and see if request is completed or not.
%% If it failed check the reason to see should it be handled.
%% @end
%%------------------------------------------------------------------------------
-spec is_completed(kz_json:object() | kz_json:objects()) -> boolean().
is_completed([]) -> 'false';
is_completed([JObj|_]) ->
    case kapi_notifications:notify_update_v(JObj)
        andalso kz_json:get_ne_binary_value(<<"Status">>, JObj)
    of
        <<"completed">> ->
            'true';
        <<"disabled">> ->
            lager:debug("notification is disabled"),
            'true';
        <<"failed">> ->
            FailureMsg = kz_json:get_ne_binary_value(<<"Failure-Message">>, JObj),
            ShouldIgnore = should_ignore_failure(FailureMsg),
            lager:debug("teletype failed with reason ~s, ignoring: ~s", [FailureMsg, ShouldIgnore]),
            ShouldIgnore;
        <<"ignored">> ->
            lager:debug("teletype has ignored the notification"),
            'true';
        _ ->
            'false'
    end;
is_completed(JObj) ->
    is_completed([JObj]).

%%------------------------------------------------------------------------------
%% @doc Check the reason to see if this failure should be saved or not.
%% @end
%%------------------------------------------------------------------------------
-spec should_ignore_failure(kz_term:api_ne_binary()) -> boolean().
should_ignore_failure(<<"missing_from">>) -> 'true';
should_ignore_failure(<<"invalid_to_addresses">>) -> 'true';
should_ignore_failure(<<"no_to_addresses">>) -> 'true';
should_ignore_failure(<<"email_encoding_failed">>) -> 'true';
should_ignore_failure(<<"validation_failed">>) -> 'true';
should_ignore_failure(<<"missing_data:", _/binary>>) -> 'true';
should_ignore_failure(<<"failed_template:", _/binary>>) -> 'true'; %% rendering problems
should_ignore_failure(<<"template_error:", _/binary>>) -> 'true'; %% rendering problems
should_ignore_failure(<<"no teletype template modules responded">>) -> 'true'; %% no module is bound?
should_ignore_failure(<<"unknown error throw-ed">>) -> 'true';

%% explicitly not ignoring these below:
should_ignore_failure(<<"unknown_template_error">>) -> 'false'; %% maybe something went wrong with template, trying later?
should_ignore_failure(<<"no_attachment">>) -> 'false'; %% probably fax or voicemail is not stored in storage yet, retry later
should_ignore_failure(<<"badmatch">>) -> 'false'; %% not ignoring it yet (voicemail_new)
should_ignore_failure(_) -> 'false'.

%%------------------------------------------------------------------------------
%% @doc Convert `kz_amqp_worker' errors to friendly string.
%% @end
%%------------------------------------------------------------------------------
-spec handle_amqp_worker_error(any()) -> failure_reason().
handle_amqp_worker_error({'badmatch', {'error', BadMatch}}) ->
    %% maybe it's validation error
    handle_amqp_worker_error(BadMatch);
handle_amqp_worker_error({'function_clause', _}) ->
    <<"sending the amqp resulted in failure: function clause error">>;
handle_amqp_worker_error({'badarg', _}) ->
    <<"sending the amqp resulted in failure: badarg error">>;
handle_amqp_worker_error(Error) ->
    case kz_json:is_json_object(Error) of
        'true' -> json_to_reason(Error);
        'false' ->
            {<<"sending the amqp resulted in failure: ", (cast_to_binary(Error))/binary>>, 'undefined'}
    end.

%%------------------------------------------------------------------------------
%% @doc Convert JObj errors to friendly string (responses from teletype or
%% `kz_amqp_worker' errors in JObj).
%%
%% For now we just only get the first failed response.
%% @end
%%------------------------------------------------------------------------------
-spec json_to_reason(any()) -> failure_reason().
json_to_reason({'returned', JObjs}) when is_list(JObjs) ->
    kz_json:find(<<"message">>, JObjs, <<"unknown broker error">>);

json_to_reason({ErrorType, JObjs}) when is_list(JObjs) ->
    Reasons = [<<"failed">>, <<"pending">>, <<"completed">>],
    Fun = fun(Reason, Acc) -> find_reason_from_jsons(Reason, JObjs, Acc) end,

    case lists:foldl(Fun, maps:new(), Reasons) of
        #{<<"completed">> := [JObj|_]} ->
            {<<"completed">>, kz_json:get_value(<<"Metadata">>, JObj)};
        #{<<"failed">> := [JObj|_]} ->
            FailureMsg = kz_json:get_ne_binary_value(<<"Failure-Message">>, JObj, <<"unknown_reason">>),
            {<<"teletype failed with reason ", FailureMsg/binary>>, kz_json:get_value(<<"Metadata">>, JObj)};
        #{<<"pending">> := [JObj|_]} ->
            FailureMsg = <<"timeout during publishing, last message from teletype is 'pending'">>,
            {FailureMsg, kz_json:get_value(<<"Metadata">>, JObj)};
        _ ->
            {<<"received ", (cast_to_binary(ErrorType))/binary, " without any response from teletype">>, 'undefined'}
    end;

json_to_reason({ErrorType, JObj}) ->
    json_to_reason({ErrorType, [JObj]});

json_to_reason(JObjs) when is_list(JObjs) ->
    json_to_reason({'error', JObjs});

json_to_reason(JObj) ->
    json_to_reason({'error', [JObj]}).

%%------------------------------------------------------------------------------
%% @doc Categorize Responses based on status.
%% @end
%%------------------------------------------------------------------------------
find_reason_from_jsons(Reason, JObjs, Map) ->
    case kz_json:find_value(<<"Status">>, Reason, JObjs, 'undefined') of
        'undefined' -> Map;
        Val -> maps:update_with(Reason, fun(List) -> [Val|List] end, [Val], Map)
    end.

-spec cast_to_binary(any()) -> kz_term:ne_binary().
cast_to_binary(Error) ->
    try kz_term:to_binary(Error)
    catch
        _:_ ->
            kz_term:to_binary(io_lib:format("~p", [Error]))
    end.

%%------------------------------------------------------------------------------
%% @doc Find notification type from the publish function.
%% @end
%%------------------------------------------------------------------------------
-spec notify_type(kz_amqp_worker:publish_fun() | kz_term:ne_binary()) -> kz_term:api_ne_binary().
notify_type(<<"publish_", NotifyType/binary>>) ->
    NotifyType;
notify_type(<<NotifyType/binary>>) ->
    lager:error("unknown notification publish function ~s", [NotifyType]),
    'undefined';
notify_type(PublishFun) ->
    case catch erlang:fun_info_mfa(PublishFun) of
        {'kapi_notifications', Fun, 1} -> notify_type(cast_to_binary(Fun));
        _Other ->
            lager:error("unknown notification publish function: ~p", [_Other]),
            'undefined'
    end.

-spec should_persist_notify(kz_term:api_binary()) -> boolean().
should_persist_notify(AccountId) ->
    ?DEFAULT_PUBLISHER_ENABLED
        andalso ?ACCOUNT_SHOULD_PERSIST(AccountId).

-spec should_handle_notify_type(kz_term:ne_binary(), kz_term:api_binary()) -> boolean().
should_handle_notify_type(NotifyType, AccountId) ->
    not lists:member(NotifyType, ?GLOBAL_FORCE_NOTIFY_TYPE_EXCEPTION)
        andalso not lists:member(NotifyType, ?NOTIFY_TYPE_EXCEPTION(AccountId)).
