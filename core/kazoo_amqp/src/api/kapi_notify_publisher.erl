%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kapi_notify_publisher).

-export([call/4
        ,call_collect/3
        ,cast/3
        ]).

-include_lib("amqp_util.hrl").

-define(FAILED_NOTIFY_DB, <<"pending_notifications">>).
-define(DEFAULT_TIMEOUT, 5 * ?MILLISECONDS_IN_SECOND).
-define(TIMEOUT, kapps_config:get_integer(<<"notify">>, <<"notify_publisher_timeout">>, ?DEFAULT_TIMEOUT)).

-type publish_fun() :: fun((api_terms()) -> any()).
-type request_return() :: {'ok', kz_json:object() | kz_json:objects()} |
                          {'returned', kz_json:object(), kz_json:object()} |
                          {'timeout', kz_json:objects()} |
                          {'error', any()}.

-spec call(api_terms(), publish_fun(), any(), ne_binary()) -> any().
call(Req, PublishFun, VFun, Type) ->
    CallResp = kz_amqp_worker:call(Req, PublishFun, VFun, 4100),
    handle_resp(Type, Req, CallResp),
    CallResp.

-spec call_collect(api_terms(), publish_fun(), ne_binary()) -> any().
call_collect(Req, PublishFun, Type) ->
    CallResp = amqp_call_collect(Req, PublishFun),
    handle_resp(Type, Req, CallResp),
    CallResp.

-spec cast(api_terms(), publish_fun(), ne_binary()) -> 'ok'.
cast(Req, PublishFun, Type) ->
    _ = kz_util:spawn(fun() -> handle_resp(Type, Req , amqp_call_collect(Req, PublishFun)) end),
    'ok'.

%%%===================================================================
%%% Internal functions
%%%===================================================================
amqp_call_collect(Req, PublishFun) ->
    kz_amqp_worker:call_collect(Req, PublishFun, fun collecting/1, ?TIMEOUT).

-spec handle_resp(ne_binary(), api_terms(), request_return()) -> 'ok'.
handle_resp(_Type, _Req, 'ok') -> 'ok';
handle_resp(Type, Req, {'ok', _}=Resp) -> check_for_failure(Type, Req, Resp);
handle_resp(Type, Req, {'error', Error}) -> handle_error(Type, Req, error_to_failure_reason(Error));
handle_resp(Type, Req, {'returned', _, Resp}) -> check_for_failure(Type, Req, {'returned', Resp});
handle_resp(Type, Req, {'timeout', _}=Resp) -> check_for_failure(Type, Req, Resp).

-spec check_for_failure(ne_binary(), api_terms(), {'ok' | 'returned' | 'timeout', kz_json:objects()}) -> 'ok'.
check_for_failure(Type, Req, {_ErrorType, Responses}=Resp) ->
    case is_completed(Responses) of
        'true' -> 'ok';
        'false' -> handle_error(Type, Req, json_to_failure_reason(Resp))
    end.

-spec handle_error(ne_binary(), api_terms(), any()) -> 'ok'.
handle_error(Type, Req, Error) ->
    lager:error("attempt for publishing notifcation ~s was unsuccessful: ~p", [Type, Error]),
    Props = props:filter_undefined(
              [{<<"description">>, <<"failed to publish notification">>}
              ,{<<"failure_reason">>, error_to_failure_reason(Error)}
              ,{<<"notification_type">>, Type}
              ,{<<"payload">>, Req}
              ,{<<"attempted">>, 1}
              ]),
    JObj = kz_doc:update_pvt_parameters(
             kz_json:from_list_recursive(Props), 'undefined', [{'type', <<"failed_notify">>}
                                                              ,{'account_id', find_account_id(Req)}
                                                              ,{'account_db', ?FAILED_NOTIFY_DB}
                                                              ]
            ),
    case kz_datamgr:save_doc(?FAILED_NOTIFY_DB, JObj) of
        {'ok', _} ->
            lager:error("payload for failed notification ~s publish attempt was saved", [Type]);
        {'error', _E} ->
            lager:error("failed to save payload for notification ~s publish attempt: ~p", [Type, _E])
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec collecting(kz_json:objects()) -> boolean().
collecting([JObj|_]) ->
    %% processing only the last message
    case kapi_notifications:notify_update_v(JObj)
        andalso kz_json:get_value(<<"Status">>, JObj)
    of
        <<"completed">> -> 'true';
        <<"failed">> -> 'true';
        _ -> 'false'
    end.

-spec is_completed(kz_json:objects()) -> boolean().
is_completed([]) -> 'false';
is_completed([JObj|_]) ->
    %% processing only the last message
    case kapi_notifications:notify_update_v(JObj)
        andalso kz_json:get_value(<<"Status">>, JObj)
    of
        <<"completed">> -> 'true';
        _ -> 'false'
    end.

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

-spec error_to_failure_reason(any()) -> ne_binary().
error_to_failure_reason({'badmatch', {'error', BadMatch}}) ->
    %% maybe it's validation error
    error_to_failure_reason(BadMatch);
error_to_failure_reason(Error) ->
    case kz_json:is_json_object(Error) of
        'true' -> json_to_failure_reason(Error);
        'false' -> cast_error_to_binary(Error)
    end.

-spec json_to_failure_reason(any()) -> ne_binary().
json_to_failure_reason({ErrorType, JObjs}) ->
    case kz_json:find(<<"Status">>, JObjs) of
        <<"failed">> -> <<"teletype failed with reason "
                        ,(kz_json:get_ne_binary_value(<<"Failure-Message">>, hd(JObjs), <<"unknown_reason">>))/binary
                        >>;
        <<"pending">> -> <<"timeout during publishing and the last teletype processing status was pending">>;
        <<"completed">> -> <<"it shouldn't be here">>;
        _ -> <<"recieved ", (cast_error_to_binary(ErrorType))/binary, " without any response from teletype">>
    end;
json_to_failure_reason(JObjs) when is_list(JObjs) ->
    json_to_failure_reason({'error', JObjs});
json_to_failure_reason(JObj) ->
    json_to_failure_reason({'error', [JObj]}).

-spec cast_error_to_binary(any()) -> ne_binary().
cast_error_to_binary(Error) ->
    try kz_term:to_binary(Error)
    catch
        _:_ -> <<"unknown_reason">>
    end.
