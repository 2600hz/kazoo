%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
%%% @doc
%%%  The module is a set of different utility functions
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Vladimir Potapev)
%%%-------------------------------------------------------------------
-module(cm_util).

-export([network_address_to_ip_tuple/1
         ,parent_account_id/1
         ,maybe_translate_kv_into_avps/3
         ,maybe_translate_avps_into_kv/3
         ,determine_aaa_request_type/1
         ,determine_channel_type/1
         ,put_session_timeout/2
         ,get_session_timeout/1
         ,put_interim_update/2
         ,get_interim_update/1
         ,clean_session_timeout/1
         ,clean_interim_update/1
         ,hangup_call/1
         ,get_resource_name/2
         ,append_resource_name_to_request/1
         ,insert_device_info_if_needed/2]).

-include("circlemaker.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%%  Converts a binary string to a IP address tuple
%% @end
%%--------------------------------------------------------------------
-spec network_address_to_ip_tuple(ne_binary()) -> {'ok', {integer(), integer(), integer(), integer()}} |
                                                  {'error', 'einval'}.
network_address_to_ip_tuple(Address) when is_binary(Address) ->
    ResolvedAddressList = wh_network_utils:resolve(Address),
    FirstResolvedAddress = get_first(ResolvedAddressList),
    parse_address(FirstResolvedAddress).

get_first([]) -> {'error', 'unresolved'};
get_first([Address | _Addresses]) -> Address.

parse_address({'error', _} = Err) -> Err;
parse_address(Address) ->
    case inet_parse:ipv4_address(binary_to_list(Address)) of
        {'ok', Res} -> Res;
        Err -> Err
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%  Returns parent account ID. Any Reseller IDs will be skipped.
%% @end
%%--------------------------------------------------------------------
-spec parent_account_id(wh_json:object()) -> api_binary().
parent_account_id(JObj) ->
    case kz_account:parent_account_id(JObj) of
        'undefined' -> <<"system_config">>;
        AccountId -> AccountId
    end.

-spec maybe_translate_kv_into_avps(wh_json:object(), wh_proplist(), atom()) -> wh_json:object().
maybe_translate_kv_into_avps(WholeRequest, AAAProps, RequestType) ->
    TranslationConfigEntry = case RequestType of
                                 'authz' -> <<"authz_avp_translation">>;
                                 'authn' -> <<"authn_avp_translation">>;
                                 'accounting' -> <<"accounting_avp_translation">>
                             end,
    Attrs = case props:get_value(TranslationConfigEntry, AAAProps) of
                        'undefined' -> WholeRequest;
                        TranslationList ->
                            lists:map(
                                fun(TranslationItem) ->
                                    maybe_translate_kv_into_avps_item(TranslationItem, WholeRequest)
                                end
                                ,TranslationList)
                    end,
    props:filter(fun(T) -> T =/= {'undefined', 'undefined'} end, Attrs).

maybe_translate_kv_into_avps_item(TranslationItem, WholeRequest) ->
    Cast = props:get_value(<<"cast">>, TranslationItem),
    Attr = props:get_value(<<"attribute">>, TranslationItem),
    RequestKey = props:get_value(<<"request_key">>, TranslationItem),
    RequestRegexp = props:get_value(<<"request_value_regexp">>, TranslationItem),
    case wh_json:get_value(RequestKey, WholeRequest) of
        'undefined' ->
            {'undefined', 'undefined'};
        Value when is_binary(Value) ->
            BinValue = binary_to_list(Value),
            BinRequestRegexp = binary_to_list(RequestRegexp),
            case re:run(BinValue, BinRequestRegexp) of
                'nomatch' -> {Attr, <<"">>};
                {'match', Groups} ->
                    {Pos, Len} = lists:nth(2, Groups),
                    NewValue = lists:sublist(BinValue, Pos + 1, Len),
                    MaybeCasted = case Cast of
                                      <<"string_to_integer">> ->
                                          list_to_integer(NewValue);
                                      _ ->
                                          list_to_binary(NewValue)
                                  end,
                    {Attr, MaybeCasted}
            end;
        Value when is_integer(Value) ->
            {Attr, Value}
    end.

-spec maybe_translate_avps_into_kv(wh_proplist(), wh_json:object(), atom()) -> wh_proplist().
maybe_translate_avps_into_kv(AVPsResponse, AAAJObj, RequestType) ->
    TranslationConfigEntry = case RequestType of
                                 'authz' -> <<"authz_avp_translation">>;
                                 'authn' -> <<"authn_avp_translation">>;
                                 'accounting' -> <<"accounting_avp_translation">>
                             end,
    TranslationList = wh_json:get_value(TranslationConfigEntry, AAAJObj),
    Props = lists:map(
                fun(TranslationItem) ->
                    maybe_translate_avps_into_kv_item(TranslationItem, AVPsResponse)
                end
                ,TranslationList),
    props:filter(fun(T) -> T =/= {'undefined', 'undefined'} end, Props).

maybe_translate_avps_into_kv_item(TranslationItem, AVPsResponse) ->
    Cast = wh_json:get_value(<<"cast">>, TranslationItem),
    Attr = wh_json:get_string_value(<<"attribute">>, TranslationItem),
    RequestKey = wh_json:get_value(<<"request_key">>, TranslationItem),
    AttrRegexp = wh_json:get_value(<<"attr_value_regexp">>, TranslationItem),
    case props:get_value(Attr, AVPsResponse) of
        'undefined' ->
            {'undefined', 'undefined'};
        Value when is_binary(Value) ->
            BinValue = binary_to_list(Value),
            BinAttrRegexp = binary_to_list(AttrRegexp),
            case re:run(BinValue, BinAttrRegexp) of
                'nomatch' -> {RequestKey, <<"">>};
                {'match', Groups} ->
                    {Pos, Len} = lists:nth(2, Groups),
                    NewValue = lists:sublist(BinValue, Pos + 1, Len),
                    {RequestKey, list_to_binary(NewValue)}
            end;
        Value when is_integer(Value) ->
            MaybeCasted = case Cast of
                              <<"string_to_integer">> ->
                                  integer_to_list(Value);
                              _ ->
                                  Value
                          end,
            {RequestKey, MaybeCasted}
    end.

-spec determine_aaa_request_type(wh_json:object()) -> 'authn' | 'authz' | 'accounting'.
determine_aaa_request_type(JObj) ->
    case {wh_json:get_value(<<"Event-Category">>, JObj), wh_json:get_value(<<"Event-Name">>, JObj)} of
        {<<"aaa">>, <<"aaa_authn_req">>} -> 'authn';
        {<<"authn">>, _} -> 'authn';
        {<<"authz">>, _} -> 'authz';
        {<<"call_event">>, <<"CHANNEL_CREATE">>} -> 'accounting';
        {<<"call_event">>, <<"CHANNEL_DESTROY">>} -> 'accounting';
        {<<"call_event">>, <<"channel_fs_status_resp">>} -> 'accounting'
    end.

-spec determine_channel_type(wh_json:object()) -> {ne_binaries(), 'normal'|'loopback'}.
determine_channel_type(JObj) ->
    From = wh_json:get_value(<<"From">>, JObj),
    To = wh_json:get_value(<<"To">>, JObj),
    CallDirection = wh_json:get_value(<<"Call-Direction">>, JObj),
    CalleeIdName = wh_json:get_value(<<"Callee-ID-Name">>, JObj),
    ResourceId = wh_json:get_value(<<"Resource-ID">>, JObj),
    Result = case ResourceId of
                  'undefined' ->
                      [<<"internal">>, CallDirection];
                  Val when is_binary(Val) ->
                      [<<"external">>, CallDirection]
              end,
    Type = case {From, To} of
               {'undefined', 'undefined'} ->
                   % special case for Interim-Update, because there are no "From" and "To" fields
                   % and because this channel is already authorized, it should be 'normal' type
                   'normal';
               _ ->
                   FromPart = binary:part(From, {byte_size(From), -2}),
                   ToPart = binary:part(To, {byte_size(To), -2}),
                   case {FromPart, ToPart, CalleeIdName} of
                       % Special type of FreeSwitch channel (see FreeSwitch sources: src/switch_ivr_originate.c:2651)
                       {_, _, <<"Outbound Call">>} -> 'loopback';
                       % Loopback patterns
                       {<<"-a">>, _, _} -> 'loopback';
                       {<<"-b">>, _, _} -> 'loopback';
                       {_, <<"-a">>, _} -> 'loopback';
                       {_, <<"-b">>, _} -> 'loopback';
                       _ -> 'normal'
                   end
           end,
    lager:debug("Channel type is ~p", [{Result, Type}]),
    {Result, Type}.

-spec put_session_timeout(pos_integer(), ne_binary()) -> any().
put_session_timeout(SessionTimeout, AccountId) ->
    lager:debug("Storing session timeout value ~p into account ~p", [SessionTimeout, AccountId]),
    DbName = wh_util:format_account_id(AccountId, 'encoded'),
    {'ok', AaaDoc} = couch_mgr:open_cache_doc(DbName, <<"aaa">>),
    case wh_json:get_value(<<"session_timeout">>, AaaDoc) of
        SessionTimeout ->
            lager:debug("This value already exists in the account");
        _ ->
            lager:debug("This value isn't exist in the accountm so it's updated"),
            NewAaaDoc = wh_json:set_value(<<"session_timeout">>, SessionTimeout, AaaDoc),
            couch_mgr:save_doc(DbName, NewAaaDoc)
    end.

-spec get_session_timeout(ne_binary()) -> pos_integer() | 'undefined'.
get_session_timeout(AccountId) ->
    lager:debug("Retrieve session timeout value from account ~p", [AccountId]),
    DbName = wh_util:format_account_id(AccountId, 'encoded'),
    {'ok', AaaDoc} = couch_mgr:open_cache_doc(DbName, <<"aaa">>),
    case wh_json:get_value(<<"session_timeout">>, AaaDoc) of
        'undefined' ->
            lager:debug("Session timeout value isn't set"),
            'undefined';
        Timeout ->
            lager:debug("Session timeout value is ~p", [Timeout]),
            Timeout
    end.

-spec clean_session_timeout(ne_binary()) -> any().
clean_session_timeout(AccountId) ->
    lager:debug("Clean session timeout value of the account ~p", [AccountId]),
    DbName = wh_util:format_account_id(AccountId, 'encoded'),
    {'ok', AaaDoc} = couch_mgr:open_cache_doc(DbName, <<"aaa">>),
    case wh_json:get_value(<<"session_timeout">>, AaaDoc) of
        'undefined' ->
            lager:debug("Session timeout value isn't set");
        Timeout ->
            lager:debug("Session timeout value is ~p", [Timeout]),
            NewAaaDoc = wh_json:delete_key(<<"session_timeout">>, AaaDoc),
            couch_mgr:save_doc(DbName, NewAaaDoc)
    end.

-spec put_interim_update(pos_integer(), ne_binary()) -> any().
put_interim_update(InterimUpdate, AccountId) ->
    lager:debug("Storing interim update interval value ~p into account ~p", [InterimUpdate, AccountId]),
    DbName = wh_util:format_account_id(AccountId, 'encoded'),
    {'ok', AaaDoc} = couch_mgr:open_cache_doc(DbName, <<"aaa">>),
    case wh_json:get_value(<<"interim_update_interval">>, AaaDoc) of
        InterimUpdate ->
            lager:debug("This value already exists in the account");
        'undefined' ->
            lager:debug("This value isn't exist in the account so it's updated"),
            NewAaaDoc = wh_json:set_value(<<"interim_update_interval">>, InterimUpdate, AaaDoc),
            couch_mgr:save_doc(DbName, NewAaaDoc)
    end.

-spec get_interim_update(ne_binary()) -> pos_integer() | 'undefined'.
get_interim_update(AccountId) ->
    lager:debug("Retrieve interim update interval value from account ~p", [AccountId]),
    DbName = wh_util:format_account_id(AccountId, 'encoded'),
    {'ok', AaaDoc} = couch_mgr:open_cache_doc(DbName, <<"aaa">>),
    case {wh_json:get_value(<<"local_interim_update_interval">>, AaaDoc)
          ,wh_json:get_value(<<"interim_update_interval">>, AaaDoc)} of
        {'undefined', 'undefined'} ->
            lager:debug("Interim update interval value isn't set"),
            'undefined';
        {'undefined', Interval} ->
            lager:debug("Interim update interval value ~p got from the interim_update_interval value", [Interval]),
            Interval;
        {Interval, _} ->
            lager:debug("Interim update interval value ~p got from the local_interim_update_interval value", [Interval]),
            Interval
    end.

-spec clean_interim_update(ne_binary()) -> any().
clean_interim_update(AccountId) ->
    lager:debug("Clean interim update interval value of the account ~p", [AccountId]),
    DbName = wh_util:format_account_id(AccountId, 'encoded'),
    {'ok', AaaDoc} = couch_mgr:open_cache_doc(DbName, <<"aaa">>),
    case wh_json:get_value(<<"interim_update_interval">>, AaaDoc) of
        'undefined' ->
            lager:debug("Interim update interval value isn't set");
        Interval ->
            lager:debug("Interim update interval value is ~p", [Interval]),
            NewAaaDoc = wh_json:delete_key(<<"interim_update_interval">>, AaaDoc),
            couch_mgr:save_doc(DbName, NewAaaDoc)
    end.

-spec hangup_call(ne_binary()) -> 'ok'.
hangup_call(CallId) ->
    lager:debug("Hangup channel with CallID ~p", [CallId]),
    HangupCallEvt = [{<<"Call-ID">>, CallId}
                     ,{<<"Reason">>, <<"hangup">>}
                     ,{<<"Fetch-ID">>, wh_util:rand_hex_binary(4)}
                     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                    ],
    wapi_call:publish_hangup_call(CallId, HangupCallEvt).

-spec get_resource_name(ne_binary(), ne_binary() | 'undefined') -> ne_binary() | 'not_found'.
get_resource_name(ResourceId, 'undefined') ->
    case couch_mgr:open_cache_doc(?WH_OFFNET_DB, ResourceId) of
        {'ok', Doc} ->
            lager:debug("Found system resource ~s in ~s", [ResourceId, ?WH_OFFNET_DB]),
            wh_json:get_value(<<"name">>, Doc);
        {'error', _E} ->
            lager:debug("Resource ~s is not a system resource (~p)", [ResourceId, _E]),
            'not_found'
    end;
get_resource_name(ResourceId, AccountId) ->
    DbName = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_cache_doc(DbName, ResourceId) of
        {'ok', Doc} ->
            lager:debug("Found local resource ~s in ~s", [ResourceId, AccountId]),
            wh_json:get_value(<<"name">>, Doc);
        {'error', _E} ->
            lager:debug("Resource ~s is not a local resource (~p)", [ResourceId, _E]),
            'not_found'
    end.

-spec append_resource_name_to_request(wh_json:object()) -> wh_json:object().
append_resource_name_to_request(Request) ->
    lager:debug("Appending Resource-Name header to the request"),
    case wh_json:get_value([?CCV, <<"Resource-ID">>], Request) of
        'undefined' ->
            lager:debug("Resource-ID value not found"),
            Request;
        ResourceId ->
            append_resource_name_to_request(Request, ResourceId)
    end.

-spec append_resource_name_to_request(wh_json:object(), ne_binary()) -> wh_json:object().
append_resource_name_to_request(Request, ResourceId) ->
    AccountId = wh_json:get_value([?CCV, <<"Account-ID">>], Request),
    Result = case wh_json:is_true([?CCV, <<"Global-Resource">>], Request, 'false') of
                 'true' ->
                     get_resource_name(ResourceId, 'undefined');
                 'false' ->
                     get_resource_name(ResourceId, AccountId)
             end,
    case Result of
        'not_found' ->
            Request;
        ResourceName ->
            wh_json:set_value([?CCV, <<"Resource-Name">>], ResourceName, Request)
    end.

get_sip_device_info(JObj) ->
    AccountId = wh_json:get_value([?CCV, <<"Account-ID">>], JObj),
    AuthID = wh_json:get_value([?CCV, <<"Authorizing-ID">>], JObj),
    case AccountId of
        'undefined' ->
            lager:debug("get_sip_device_info()1"),
            {'error', 'not_found'};
        _ ->
            lager:debug("get_sip_device_info()2"),
            AuthType = wh_json:get_value([?CCV, <<"Authorizing-Type">>], JObj),
            DeviceRes = couch_mgr:open_cache_doc(wh_util:format_account_id(AccountId, 'encoded'), AuthID),
            AuthType1 = case AuthType of
                            'undefined' ->
                                lager:debug("get_sip_device_info()3"),
                                case DeviceRes of
                                    {'ok', _} ->
                                        lager:debug("get_sip_device_info()4"),
                                        <<"device">>;
                                    _ ->
                                        lager:debug("get_sip_device_info()5"),
                                        'undefined'
                                end;
                            _ ->
                                lager:debug("get_sip_device_info()6"),
                                AuthType
                        end,
            case AuthType1 of
                <<"device">> ->
                    lager:debug("get_sip_device_info()7"),
                    {'ok', DeviceDoc} = DeviceRes,
                    case wh_json:get_value(<<"device_type">>, DeviceDoc) of
                        <<"sip_device">> ->
                            lager:debug("get_sip_device_info()8"),
                            DeviceName = wh_json:get_value(<<"name">>, DeviceDoc),
                            SipUserName = wh_json:get_value([<<"sip">>, <<"username">>], DeviceDoc),
                            {'ok', {DeviceName, SipUserName}};
                        _ ->
                            lager:debug("get_sip_device_info()9"),
                            DeviceName = wh_json:get_value(<<"name">>, DeviceDoc),
                            {'ok', {DeviceName, <<"">>}}
                    end;
                _ ->
                    lager:debug("get_sip_device_info()10"),
                    {'ok', {<<"">>, <<"">>}}
            end
    end.

insert_device_info_if_needed(JObj, _Type) ->
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    lager:debug("get_sip_device_info()11"),
    case ets:lookup(?ETS_DEVICE_INFO, CallId) of
        [] ->
            lager:debug("get_sip_device_info()12"),
            case get_sip_device_info(JObj) of
                {'ok', {DeviceName, SipUserName}} ->
                    % store SIP Device Info in ETS to use it in an authz
                    lager:debug("get_sip_device_info()13"),
                    ets:insert(?ETS_DEVICE_INFO, {CallId, DeviceName, SipUserName}),
                    wh_json:set_values([{[?CCV, <<"Device-Name">>], DeviceName}
                                        ,{[?CCV, <<"Device-SIP-User-Name">>], SipUserName}]
                                        ,JObj);
                {'error', _} ->
                    lager:debug("get_sip_device_info()14"),
                    JObj
            end;
        [{CallId, DeviceName, SipUserName}] ->
            lager:debug("get_sip_device_info()15"),
            wh_json:set_values([{[?CCV, <<"Device-Name">>], DeviceName}
                                ,{[?CCV, <<"Device-SIP-User-Name">>], SipUserName}]
                                ,JObj)
    end.
