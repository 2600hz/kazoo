%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
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

-define(DEFAULT_PUBLISHER_ENABLED,
        kapps_config:get_is_true(?NOTIFY_CAT, <<"notify_presist_enabled">>, true)
       ).
-define(ACCOUNT_SHOULD_PRESIST(AccountId),
        kapps_account_config:get_global(AccountId, ?NOTIFY_CAT, <<"should_presist_for_retry">>, true)
       ).

-define(DEFAULT_TYPE_EXCEPTION, [<<"system_alert">>
                                ,<<"voicemail_save">>
                                ,<<"register">>
                                ]).
-define(GLOBAL_FORCE_NOTIFY_TYPE_EXCEPTION,
        kapps_config:get_ne_binaries(?NOTIFY_CAT, <<"notify_presist_temprorary_force_exceptions">>, [])
       ).
-define(NOTIFY_TYPE_EXCEPTION(AccountId),
        kapps_account_config:get_global(AccountId, ?NOTIFY_CAT, <<"notify_presist_exceptions">>, ?DEFAULT_TYPE_EXCEPTION)
       ).

%%--------------------------------------------------------------------
%% @doc Publish notification and collect notify update messages from
%%      teletype, useful if you want to make sure teletype processed
%%      the notification completely (e.g. new voicemail)
%%--------------------------------------------------------------------
-spec call_collect(api_terms(), kz_amqp_worker:publish_fun()) -> kz_amqp_worker:request_return().
call_collect(Req, PublishFun) ->
    NotifyType = notify_type(PublishFun),
    CallResp = kz_amqp_worker:call_collect(Req, PublishFun, fun collecting/1, ?TIMEOUT),
    handle_resp(NotifyType, Req, CallResp),
    CallResp.

%%--------------------------------------------------------------------
%% @doc Publish notification asynchronous, and save it to db if
%%      it failed to retry later.
%%--------------------------------------------------------------------
-spec cast(api_terms(), kz_amqp_worker:publish_fun()) -> 'ok'.
cast(Req, PublishFun) ->
    CallId = kz_util:get_callid(),
    Fun = fun() ->
                  _ = case kz_term:is_ne_binary(CallId) of
                          'true' -> kz_util:put_callid(CallId);
                          'false' -> kz_util:put_callid(Req)
                      end,
                  call_collect(Req, PublishFun)
          end,
    _ = erlang:spawn(Fun),
    'ok'.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc handle amqp worker responses
-spec handle_resp(api_ne_binary(), api_terms(), kz_amqp_worker:request_return()) -> 'ok'.
handle_resp(NotifyType, Req, {'ok', _}=Resp) -> check_for_failure(NotifyType, Req, Resp);
handle_resp(NotifyType, Req, {'error', Error}) -> maybe_handle_error(NotifyType, Req, error_to_failure_reason(Error));
handle_resp(NotifyType, Req, {'returned', _, Resp}) -> check_for_failure(NotifyType, Req, {'returned', [Resp]});
handle_resp(NotifyType, Req, {'timeout', _}=Resp) -> check_for_failure(NotifyType, Req, Resp).

%% @private
%% @doc check for notify update messages from teletype/notify apps
-spec check_for_failure(api_ne_binary(), api_terms(), {'ok' | 'returned' | 'timeout', kz_json:objects()}) -> 'ok'.
check_for_failure(NotifyType, Req, {_ErrorType, Responses}=Resp) ->
    case is_completed(Responses) of
        'true' -> 'ok';
        'false' -> maybe_handle_error(NotifyType, Req, json_to_failure_reason(Resp))
    end.

-spec maybe_handle_error(api_binary(), api_terms(), any()) -> 'ok'.
maybe_handle_error('undefined', _Req, _Error) ->
    lager:warning("not saving undefined notification");
maybe_handle_error(NotifyType, Req, Error) ->
    AccountId = find_account_id(Req),
    should_presist_notify(AccountId)
        andalso should_handle_notify_type(NotifyType, AccountId)
        andalso handle_error(NotifyType, Req, Error).

%% @private
%% @doc save pay load to db to retry later
-spec handle_error(ne_binary(), api_terms(), any()) -> 'ok'.
handle_error(NotifyType, Req, Error) ->
    lager:warning("attempt for publishing notification ~s was unsuccessful: ~p", [NotifyType, Error]),
    Props = props:filter_undefined(
              [{<<"description">>, <<"failed to publish notification">>}
              ,{<<"failure_reason">>, error_to_failure_reason(Error)}
              ,{<<"notification_type">>, NotifyType}
              ,{<<"payload">>, Req}
              ,{<<"attempts">>, 1}
              ]),
    JObj = kz_doc:update_pvt_parameters(kz_json:from_list_recursive(Props)
                                       ,'undefined'
                                       , [{'type', <<"failed_notify">>}
                                         ,{'account_id', find_account_id(Req)}
                                         ,{'account_db', ?KZ_PENDING_NOTIFY_DB}
                                         ]
                                       ),
    save_pending_notification(NotifyType, JObj, 2).

-spec save_pending_notification(ne_binary(), kz_json:object(), integer()) -> 'ok'.
save_pending_notification(_NotifyType, _JObj, Loop) when Loop < 0 ->
    lager:error("max try to save payload for notification ~s publish attempt", [_NotifyType]);
save_pending_notification(NotifyType, JObj, Loop) ->
    case kz_datamgr:save_doc(?KZ_PENDING_NOTIFY_DB, JObj) of
        {'ok', Saved} ->
            lager:warning("payload for failed notification ~s publish attempt was saved to ~s", [NotifyType, kz_doc:id(Saved)]);
        {'error', 'not_found'} ->
            kapps_maintenance:refresh(?KZ_PENDING_NOTIFY_DB),
            save_pending_notification(NotifyType, JObj, Loop - 1);
        {'error', 'timeout'} ->
            save_pending_notification(NotifyType, JObj, Loop - 1);
        {'error', 'conflict'} ->
            save_pending_notification(NotifyType, JObj, Loop - 1);
        {'error', _E} ->
            lager:error("failed to save payload for notification ~s publish attempt: ~p", [NotifyType, _E])
    end.

%% @private
%% @doc only stop if failed or completed messages are received
-spec collecting(kz_json:objects()) -> boolean().
collecting([JObj|_]) ->
    case kapi_notifications:notify_update_v(JObj)
        andalso kz_json:get_ne_binary_value(<<"Status">>, JObj)
    of
        <<"completed">> -> 'true';
        <<"disabled">> -> 'true';
        <<"ignored">> -> 'true';
        <<"failed">> -> 'true';
        _ -> 'false'
    end.

%% @private
%% @doc whether or not the publish is completed by teletype
-spec is_completed(kz_json:object() | kz_json:objects()) -> boolean().
is_completed([]) -> 'false';
is_completed([JObj|_]) ->
    case kapi_notifications:notify_update_v(JObj)
        andalso kz_json:get_ne_binary_value(<<"Status">>, JObj)
    of
        <<"completed">> -> 'true';
        <<"disabled">> ->
            Reason = kz_json:get_ne_binary_value(<<"Failure-Message">>, JObj, <<>>),
            lager:debug("notification is disabled ~p", [Reason]),
            'true';
        <<"ignored">> ->
            Reason = kz_json:get_ne_binary_value(<<"Failure-Message">>, JObj, <<>>),
            lager:debug("teletype has ignored the notification ~p", [Reason]),
            'true';
        <<"failed">> ->
            FailureMsg = kz_json:get_ne_binary_value(<<"Failure-Message">>, JObj),
            ShouldIgnore = should_ignore_failure(FailureMsg),
            lager:debug("teletype failed with reason ~s, ignoring: ~s", [FailureMsg, ShouldIgnore]),
            ShouldIgnore;
        %% FIXME: Is pending enough to consider publish was successful? at least teletype received the notification!
        %% <<"pending">> -> 'true';
        _ -> 'false'
    end;
is_completed(JObj) ->
    is_completed([JObj]).

-spec should_ignore_failure(api_ne_binary()) -> boolean().
should_ignore_failure(<<"missing_from">>) -> 'true';
should_ignore_failure(<<"invalid_to_addresses">>) -> 'true';
should_ignore_failure(<<"no_to_addresses">>) -> 'true';
should_ignore_failure(<<"email_encoding_failed">>) -> 'true';
should_ignore_failure(<<"validation_failed">>) -> 'true';
should_ignore_failure(<<"missing_data:", _/binary>>) -> 'true';
should_ignore_failure(<<"failed_template:", _/binary>>) -> 'true'; %% rendering problems
should_ignore_failure(<<"template_error:", _/binary>>) -> 'true'; %% rendering problems
should_ignore_failure(<<"no teletype template modules responded">>) -> 'true'; %% no module is binded?
should_ignore_failure(<<"unknown error throw-ed">>) -> 'true';

%% explicitly not ignoring these below:
should_ignore_failure(<<"unknown_template_error">>) -> 'false'; %% maybe something went wrong with template, trying later?
should_ignore_failure(<<"no_attachment">>) -> 'false'; %% probably fax or voicemail is not stored in storage yet, retry later
should_ignore_failure(<<"badmatch">>) -> 'false'; %% not ignoring it yet (voicemail_new)
should_ignore_failure(_) -> 'false'.

%% @private
%% @doc try to find account id in different part of payload(copied from teletype_util)
-spec find_account_id(api_terms()) -> api_ne_binary().
find_account_id(Req) when is_list(Req) ->
    find_account_id(Req, fun props:get_first_defined/2);
find_account_id(Req) ->
    find_account_id(Req, fun kz_json:get_first_defined/2).

find_account_id(Req, Get) ->
    Get([<<"account_id">>
        ,[<<"account">>, <<"_id">>]
        ,<<"pvt_account_id">>
        ,<<"_id">>, <<"id">>
        ,<<"Account-ID">>
        ,[<<"details">>, <<"account_id">>]
        ,[<<"Details">>, <<"Account-ID">>]
        ,[<<"details">>, <<"custom_channel_vars">>, <<"account_id">>]
        ,[<<"Details">>, <<"Custom-Channel-Vars">>, <<"Account-ID">>]
        ]
       ,Req
       ).

%% @private
%% @doc convert error to human understandable string
-spec error_to_failure_reason(any()) -> ne_binary().
error_to_failure_reason({'badmatch', {'error', BadMatch}}) ->
    %% maybe it's validation error
    error_to_failure_reason(BadMatch);
error_to_failure_reason({'function_clause', _}) ->
    <<"function clause error">>;
error_to_failure_reason({'badarg', _}) ->
    <<"badarg error">>;
error_to_failure_reason(Error) ->
    case kz_json:is_json_object(Error) of
        'true' -> json_to_failure_reason(Error);
        'false' -> cast_to_binary(Error)
    end.

%% @private
%% @doc same as above for json (response from teletype)
-spec json_to_failure_reason(any()) -> ne_binary().
json_to_failure_reason({ErrorType, JObjs}) when is_list(JObjs) ->
    case kz_json:find(<<"Status">>, JObjs) of
        <<"failed">> -> <<"teletype failed with reason "
                          ,(kz_json:get_ne_binary_value(<<"Failure-Message">>, hd(JObjs), <<"unknown_reason">>))/binary
                        >>;
        <<"pending">> -> <<"timeout during publishing, last message from teletype is 'pending'">>;
        <<"completed">> -> <<"it shouldn't be here">>;
        _ -> <<"received ", (cast_to_binary(ErrorType))/binary, " without any response from teletype">>
    end;
json_to_failure_reason({'error', JObj}) ->
    json_to_failure_reason({'error', [JObj]});
json_to_failure_reason(JObjs) when is_list(JObjs) ->
    json_to_failure_reason({'error', JObjs});
json_to_failure_reason(JObj) ->
    json_to_failure_reason({'error', [JObj]}).

%% @private
-spec cast_to_binary(any()) -> ne_binary().
cast_to_binary(Error) ->
    try kz_term:to_binary(Error)
    catch
        _:_ ->
            lager:debug("failed to convert notification failure reason to binary: ~p", [Error]),
            kz_term:to_binary(io_lib:format("~p", [Error]))
    end.

%% @private
%% @doc detect notification type from publish function
%%      clever way for lazy developer who wants to type/remember less
-spec notify_type(kz_amqp_worker:publish_fun() | ne_binary()) -> api_ne_binary().
notify_type(<<"publish_", NotifyType/binary>>) ->
    NotifyType;
notify_type(NotifyType) when is_binary(NotifyType) ->
    lager:error("unknown notification publish function ~s", [NotifyType]),
    'undefined';
notify_type(PublishFun) ->
    case catch erlang:fun_info_mfa(PublishFun) of
        {kapi_notifications, Fun, 1} -> notify_type(cast_to_binary(Fun));
        _Other ->
            lager:error("unknown notification publish function: ~p", [_Other]),
            'undefined'
    end.

-spec should_presist_notify(api_binary()) -> boolean().
should_presist_notify(AccountId) ->
    ?DEFAULT_PUBLISHER_ENABLED
        andalso ?ACCOUNT_SHOULD_PRESIST(AccountId).

-spec should_handle_notify_type(ne_binary(), api_binary()) -> boolean().
should_handle_notify_type(NotifyType, AccountId) ->
    not lists:member(NotifyType, ?GLOBAL_FORCE_NOTIFY_TYPE_EXCEPTION)
        andalso not lists:member(NotifyType, ?NOTIFY_TYPE_EXCEPTION(AccountId)).
