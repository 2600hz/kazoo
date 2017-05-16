%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kapi_notify_publisher).

-export([call_collect/2
        ,cast/2
        ]).

-include_lib("amqp_util.hrl").

-define(FAILED_NOTIFY_DB, <<"pending_notifications">>).
-define(DEFAULT_TIMEOUT, 10 * ?MILLISECONDS_IN_SECOND).
-define(TIMEOUT, kapps_config:get_integer(<<"notify">>, <<"notify_publisher_timeout">>, ?DEFAULT_TIMEOUT)).

%%--------------------------------------------------------------------
%% @doc Publish notification and collect notify update messages from
%%      teletype, useful if you want to make sure teletype proccessed
%%      the notifaction compeletly (e.g. new voicemail)
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
    _ = kz_util:spawn(fun() -> call_collect(Req, PublishFun) end),
    'ok'.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc handle amqp worker responses
-spec handle_resp(api_ne_binary(), api_terms(), kz_amqp_worker:request_return()) -> 'ok'.
handle_resp(_NotifyType, _Req, 'ok') -> 'ok';
handle_resp(NotifyType, Req, {'ok', _}=Resp) -> check_for_failure(NotifyType, Req, Resp);
handle_resp(NotifyType, Req, {'error', Error}) -> handle_error(NotifyType, Req, error_to_failure_reason(Error));
handle_resp(NotifyType, Req, {'returned', _, Resp}) -> check_for_failure(NotifyType, Req, {'returned', Resp});
handle_resp(NotifyType, Req, {'timeout', _}=Resp) -> check_for_failure(NotifyType, Req, Resp).

%% @private
%% @doc check for notify update messages from teeltype/notify apps
-spec check_for_failure(api_ne_binary(), api_terms(), {'ok' | 'returned' | 'timeout', kz_json:objects()}) -> 'ok'.
check_for_failure(NotifyType, Req, {_ErrorType, Responses}=Resp) ->
    case is_completed(Responses) of
        'true' -> 'ok';
        'false' -> handle_error(NotifyType, Req, json_to_failure_reason(Resp))
    end.
%% @private
%% @doc save pay load to db to retry later
-spec handle_error(api_ne_binary(), api_terms(), any()) -> 'ok'.
handle_error('undefined', _Req, _Error) ->
    lager:error("not saving undefined notification");
handle_error(NotifyType, Req, Error) ->
    lager:error("attempt for publishing notifcation ~s was unsuccessful: ~p", [NotifyType, Error]),
    Props = props:filter_undefined(
              [{<<"description">>, <<"failed to publish notification">>}
              ,{<<"failure_reason">>, error_to_failure_reason(Error)}
              ,{<<"notification_type">>, NotifyType}
              ,{<<"payload">>, Req}
              ,{<<"attempted">>, 1}
              ]),
    JObj = kz_doc:update_pvt_parameters(
             kz_json:from_list_recursive(Props), 'undefined', [{'type', <<"failed_notify">>}
                                                              ,{'account_id', find_account_id(Req)}
                                                              ,{'account_db', ?FAILED_NOTIFY_DB}
                                                              ]
            ),
    save_pending_notification(NotifyType, JObj, 2).

-spec handle_error(ne_binary(), kz_json:object(), integer()) -> 'ok'.
save_pending_notification(_NotifyType, _JObj, Loop) when Loop < 0 ->
    lager:error("max try to save payload for notification ~s publish attempt", [NotifyType]);
save_pending_notification(NotifyType, JObj, Loop) ->
    case kz_datamgr:save_doc(?FAILED_NOTIFY_DB, JObj) of
        {'ok', _} ->
            lager:error("payload for failed notification ~s publish attempt was saved", [NotifyType]);
        {'error', 'not_found'} ->
            _ = kz_datamgr:db_create(?FAILED_NOTIFY_DB),
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
        andalso kz_json:get_value(<<"Status">>, JObj)
    of
        <<"completed">> -> 'true';
        <<"failed">> -> 'true';
        _ -> 'false'
    end.

%% @private
%% @doc whether or not the publish is completed by teletype
-spec is_completed(kz_json:objects()) -> boolean().
is_completed([]) -> 'false';
is_completed([JObj|_]) ->
    case kapi_notifications:notify_update_v(JObj)
        andalso kz_json:get_value(<<"Status">>, JObj)
    of
        <<"completed">> -> 'true';
        %% FIXME: Is pending enough to consider publish was successful? at least teletype recieved the notification!
        %% <<"pending">> -> 'true';
        _ -> 'false'
    end.

%% @private
%% @doc try to find account id in different part of payload(copied from teletype_util)
-spec find_account_id(api_terms()) -> api_binary().
find_account_id(Req) ->
    props:get_first_defined([<<"account_id">>
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
        _ -> <<"recieved ", (cast_to_binary(ErrorType))/binary, " without any response from teletype">>
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
        _:_ -> <<"unknown_reason">>
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
