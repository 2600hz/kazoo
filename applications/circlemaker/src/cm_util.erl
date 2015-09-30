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
         ,store_leg_kvs/2
         ,transform_leg_kvs/3
         ,cleanup_leg_kvs/1
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
         ,insert_device_info_if_needed/2
         ,mark_channel_as_loopback/1
         ,unmark_channel_as_loopback/1
         ,is_channel_loopback/1
         ,get_channel_fs_status/1
         ,cleanup_channel_fs_status/1
         ,store_channel_type/2
         ,read_channel_type/1
         ,cleanup_channel_type/1
         ,ets_add_inbound_originate_leg/2
         ,ets_add_other_leg_of_inbound_originate_leg/3
         ,ets_update_leg_jobj_originator_type/2
         ,ets_cleanup_other_and_orig_legs/2
         ,get_actual_aaa_document/2]).

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
                                 'accounting' -> <<"accounting_avp_translation">>;
                                 'custom' -> <<"custom_avp_translation">>
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
            lager:debug("kv to avp: ~p by ~p", [BinValue, BinRequestRegexp]),
            case re:run(BinValue, BinRequestRegexp) of
                'nomatch' ->
                    lager:debug("nomatch"),
                    {Attr, <<"">>};
                {'match', Groups} ->
                    lager:debug("match: ~p", [Groups]),
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
                                 'accounting' -> <<"accounting_avp_translation">>;
                                 'custom' -> <<"custom_avp_translation">>
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

validate_leg_kvs_cfg_sections(AAADoc) ->
    RequestKV = wh_json:get_value([<<"leg_fields_transformation">>, <<"request_fields">>], AAADoc),
    ResultKV = wh_json:get_value([<<"leg_fields_transformation">>, <<"result_fields">>], AAADoc),
    case (RequestKV =/= 'undefined') and (ResultKV =/= 'undefined') of
        'true' -> 
            'true';
        'false' ->
            lager:debug("No any sections to configure a leg state transformation were found in AAA-document"),
            'false'
    end.

-spec store_leg_kvs(wh_json:object(), wh_json:object()) -> any().
store_leg_kvs(JObj, AAADoc) ->
    lager:debug("Store leg state"),
    case validate_leg_kvs_cfg_sections(AAADoc) of
        'true' ->
            {ChannelProps, ChannelType, ChannelOrig} = determine_channel_type(JObj),
            CallId = wh_json:get_value(<<"Call-ID">>, JObj),
            lager:debug("Channel type is ~p on store leg ~p state", [{ChannelProps, ChannelType, ChannelOrig}, CallId]),
            LegType = case {ChannelProps, ChannelType, ChannelOrig} of
                          {_, _, 'orig'} ->
                              % originate inbound leg
                              'orig_leg';
                          {[<<"external">>, <<"outbound">>], 'normal', _} ->
                              % external outbound leg (dest. leg)
                              'dest_leg';
                          _ ->
                              'other_leg'
                      end,
            EventType = case {wh_json:get_value(<<"Event-Category">>, JObj), wh_json:get_value(<<"Event-Name">>, JObj)} of
                            {<<"authz">>, _} ->
                                'authz';
                            {<<"call_event">>, <<"CHANNEL_CREATE">>} ->
                                'channel_create';
                            {<<"call_event">>, <<"CHANNEL_DESTROY">>} ->
                                'channel_destroy';
                            _Evt ->
                                lager:error("Other event ~p", [_Evt]),
                                'other_event'
                        end,
            RequestKV = wh_json:get_value([<<"leg_fields_transformation">>, <<"request_fields">>], AAADoc),
            lists:foreach(fun(Item) ->
                ItemLegType = list_to_atom(binary_to_list(wh_json:get_value(<<"source">>, Item))),
                ItemEventType = list_to_atom(binary_to_list(wh_json:get_value(<<"event">>, Item, <<"undefined">>))),
                ItemKey = wh_json:get_value(<<"key">>, Item),
                ItemAlias = wh_json:get_value(<<"alias">>, Item),
                ItemRegexp = wh_json:get_value(<<"value_regexp">>, Item),
                Val = wh_json:get_value(ItemKey, JObj),
                lager:debug("wh_json:get_value(~p, JObj) returns ~p", [ItemKey, Val]),
                lager:debug("Legs type comparsion: ~p =:= ~p", [{LegType, EventType}, {ItemLegType, ItemEventType}]),
                (Val =/= 'undefined') andalso ({LegType, EventType} =:= {ItemLegType, ItemEventType}) andalso
                    begin
                        lager:debug("Trying to store state for the leg ~p", [{ChannelProps, ChannelType, ChannelOrig}]),
                        % this request KV should be stored for later use in corresponding ETS
                        case re:run(binary_to_list(Val), binary_to_list(ItemRegexp)) of
                            'nomatch' ->
                                lager:error("No match: ~p:~p", [binary_to_list(Val), binary_to_list(ItemRegexp)]);
                            {'match', Groups} ->
                                lager:debug("Match: ~p", [Groups]),
                                {Pos, Len} = lists:nth(2, Groups),
                                NewValue = lists:sublist(binary_to_list(Val), Pos + 1, Len),
                                Rec = {{CallId, ItemLegType, ItemEventType, ItemAlias}, list_to_binary(NewValue)},
                                lager:debug("Store leg state in ETS: ~p", [Rec]),
                                ets:insert(?ETS_LEGS_STATE_STORAGE, Rec)
                        end
                    end
            end, RequestKV);
        'false' ->
            'ok'
    end.

-spec transform_leg_kvs(wh_json:object(), wh_json:object(), wh_json:object()) -> wh_json:object().
transform_leg_kvs(JObj, CustomData, AAADoc) ->
    lager:debug("Transform leg state"),
    case validate_leg_kvs_cfg_sections(AAADoc) of
        'true' ->
            {ChannelProps, ChannelType, ChannelOrig} = determine_channel_type(JObj),
            LegType = case {ChannelProps, ChannelType, ChannelOrig} of
                          {_, _, 'orig'} ->
                              % originate inbound leg
                              'orig_leg';
                          {[<<"external">>, <<"outbound">>], 'normal', _} ->
                              % external outbound leg (dest. leg)
                              'dest_leg'
                      end,
            EventType = case {wh_json:get_value(<<"Event-Category">>, JObj), wh_json:get_value(<<"Event-Name">>, JObj)} of
                            {<<"authz">>, _} ->
                                lager:debug("Authz JObj: ~p", [JObj]),
                                'authz';
                            {<<"call_event">>, <<"CHANNEL_CREATE">>} ->
                                'channel_create';
                            {<<"call_event">>, <<"CHANNEL_DESTROY">>} ->
                                'channel_destroy';
                            _Evt ->
                                lager:debug("Other event ~p. Shouldn't be processed.", [_Evt]),
                                'unrecognized_event'
                        end,
            ResponseKV = wh_json:get_value([<<"leg_fields_transformation">>, <<"result_fields">>], AAADoc),
            lists:foldl(
                fun(Item, CurrJObj) ->
                    ItemLegType = list_to_atom(binary_to_list(wh_json:get_value(<<"dest">>, Item))),
                    ItemEventType = list_to_atom(binary_to_list(wh_json:get_value(<<"event">>, Item))),
                    ItemKey = wh_json:get_value(<<"key">>, Item),
                    ItemValuePattern = wh_json:get_value(<<"value">>, Item),
                    case ({LegType, EventType} =:= {ItemLegType, ItemEventType}) of
                        'true' ->
                            lager:debug("Trying to transform leg state ~p", [{ChannelProps, ChannelType, ChannelOrig}]),
                            NewValue = process_tokens(JObj, ItemValuePattern, CustomData, AAADoc, ItemLegType, ItemEventType),
                            wh_json:set_value(ItemKey, NewValue, CurrJObj);
                        'false' ->
                            CurrJObj
                    end
                end, JObj, ResponseKV);
        'false' ->
            JObj
    end.

process_tokens(JObj, ItemValuePattern, CustomData, AAADoc, ItemLegType, ItemEventType) ->
    % find tokens and search for its values
    case re:run(binary_to_list(ItemValuePattern), "\\$([a-zA-Z]+[0-9]*)") of
        'nomatch' ->
            % all tokens processed
            lager:debug("All tokens processed. Resulting value is ~p", [ItemValuePattern]),
            ItemValuePattern;
        {'match', Groups} ->
            lager:debug("match: ~p", [Groups]),
            {Pos, Len} = lists:nth(2, Groups),
            Alias = lists:sublist(binary_to_list(ItemValuePattern), Pos + 1, Len),
            lager:debug("Alias found: ~p", [Alias]),
            % find this item in ETS
            OtherLegCallId = wh_json:get_value(<<"Other-Leg-Call-ID">>, JObj),
            lager:debug("CallID of other leg is ~p", [OtherLegCallId]),
            % get leg type and event_type of orig leg
            RequestKV = wh_json:get_value([<<"leg_fields_transformation">>, <<"request_fields">>], AAADoc),
            [FoundItem] = [Item || Item <- RequestKV, wh_json:get_value(<<"alias">>, Item) =:= list_to_binary(Alias)],
            lager:debug("qwe1: ~p", [FoundItem]),
            {ChannelType, ChannelOrig} = case wh_json:get_value(<<"source">>, FoundItem) of
                                             <<"custom">> ->
                                                 lager:debug("It's custom alias ~p", [FoundItem]),
                                                 {'other', 'other'};
                                             Source ->
                                                 lager:error("It's not custom alias"),
                                                 % stay as is
                                                 {list_to_atom(binary_to_list(Source)), list_to_atom(binary_to_list(wh_json:get_value(<<"event">>, FoundItem)))}
                                         end,
            lager:debug("qwe3: ~p", [{OtherLegCallId, ChannelType, ChannelOrig}]),
            ResultValue = case ets:lookup(?ETS_LEGS_STATE_STORAGE, {OtherLegCallId, ChannelType, ChannelOrig, list_to_binary(Alias)}) of
                              [] ->
                                  lager:debug("No data in ETS for this alias. It could be a custom data."),
                                  % no data in ETS, so it could be an "custom" data source. Trying to find it
                                  RequestKV = wh_json:get_value([<<"leg_fields_transformation">>, <<"request_fields">>], AAADoc),
                                  [FoundItem] = [Item || Item <- RequestKV, wh_json:get_value(<<"alias">>, Item) =:= list_to_binary(Alias)],
                                  lager:debug("qwe2: ~p", [FoundItem]),
                                  case wh_json:get_value(<<"source">>, FoundItem) of
                                      <<"custom">> ->
                                          lager:debug("Found custom alias ~p", [FoundItem]),
                                          Key = wh_json:get_value(<<"key">>, FoundItem),
                                          Value = wh_json:get_value(Key, CustomData),
                                          re:replace(binary_to_list(ItemValuePattern), "\\$[a-zA-Z0-9]+", binary_to_list(Value),[{return,list}]);
                                      _ ->
                                          lager:error("No alias was found"),
                                          % stay as is
                                          re:replace(binary_to_list(ItemValuePattern), "\\$[a-zA-Z0-9]+", "",[{return,list}])
                                  end;
                              ItemsList ->
                                  lager:debug("Found data ~p in ETS for this alias", [ItemsList]),
                                  % filter data for needed token (alias)
                                  [{_, Value}] = [E || E = {{_, _, _, ItemAlias}, _} <- ItemsList, ItemAlias =:= list_to_binary(Alias)],
                                  re:replace(binary_to_list(ItemValuePattern), "\\$[a-zA-Z0-9]+", binary_to_list(Value),[{return,list}])
                          end,
            ResultValueBin = list_to_binary(ResultValue),
            lager:debug("Intermediate value is ~p", [ResultValueBin]),
            process_tokens(JObj, ResultValueBin, CustomData, AAADoc, ItemLegType, ItemEventType)
    end.

-spec cleanup_leg_kvs(api_binary()) -> any().
cleanup_leg_kvs(CallId) ->
    lager:debug("cleanup_leg_kvs/1: ~p", [ets:match_object(?ETS_LEGS_STATE_STORAGE, {{CallId,'$1','$2', '$3'}, '_'})]),
    case ets:match_object(?ETS_LEGS_STATE_STORAGE, {{CallId,'$1','$2', '$3'}, '_'}) of
        [] ->
            lager:info("No objects in legs state storage to cleanup for the leg ~p", [CallId]);
        _ItemsList ->
            lager:info("Cleanup cached record in legs state storage for the leg ~p", [CallId])
%%             ,
%%             [ets:delete(?ETS_LEGS_STATE_STORAGE, {CallId1, ItemLegType, ItemEventType, Alias}) || {{CallId1, ItemLegType, ItemEventType, Alias}, _} <- ItemsList]
    end.

-spec determine_aaa_request_type(wh_json:object()) -> 'authn' | 'authz' | 'accounting' | 'custom'.
determine_aaa_request_type(JObj) ->
    case {wh_json:get_value(<<"Event-Category">>, JObj), wh_json:get_value(<<"Event-Name">>, JObj)} of
        {<<"aaa">>, <<"aaa_authn_req">>} -> 'authn';
        {<<"aaa">>, <<"aaa_custom_req">>} -> 'custom';
        {<<"authn">>, _} -> 'authn';
        {<<"authz">>, _} -> 'authz';
        {<<"call_event">>, <<"CHANNEL_CREATE">>} -> 'accounting';
        {<<"call_event">>, <<"CHANNEL_DESTROY">>} -> 'accounting';
        {<<"call_event">>, <<"channel_fs_status_resp">>} -> 'accounting'
    end.

-spec determine_channel_type(wh_json:object()) -> {ne_binaries(), 'normal'|'loopback'}.
determine_channel_type(JObj) ->
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    lager:debug("Determine type of channel ~p", [CallId]),
    case cm_util:read_channel_type(CallId) of
        {'error', 'not_found'} ->
            From = wh_json:get_value(<<"From">>, JObj),
            To = wh_json:get_value(<<"To">>, JObj),
            CallDirection = wh_json:get_value(<<"Call-Direction">>, JObj),
            CalleeIdName = wh_json:get_value(<<"Callee-ID-Name">>, JObj),
            CallerIdName = wh_json:get_value(<<"Caller-ID-Name">>, JObj),
            ResourceId = wh_json:get_value([?CCV, <<"Resource-ID">>], JObj),
            AccountId = wh_json:get_value([?CCV, <<"Account-ID">>], JObj),
            IsInboundExternal = wh_json:is_true([?CCV, <<"Is-Inbound-External">>], JObj),
            Result = case {ResourceId, AccountId, IsInboundExternal} of
                         {_, _, 'true'} ->
                             [<<"external">>, <<"inbound">>];
                         {'undefined', 'undefined', _} ->
                             [<<"external">>, <<"inbound">>];
                         {'undefined', _, _} ->
                             [<<"internal">>, CallDirection];
                         {Val, _, _} when is_binary(Val) ->
                             [<<"external">>, CallDirection]
                     end,
            Type = case Result of
                       [<<"external">>, <<"outbound">>] ->
                           'normal';
                       _ ->
                           case {From, To} of
                               {'undefined', 'undefined'} ->
                                   % special case for Interim-Update, because there are no "From" and "To" fields
                                   % and because this channel is already authorized, it should be 'normal' type
                                   'normal';
                               _ ->
                                   FromPart = binary:part(From, {byte_size(From), -2}),
                                   ToPart = binary:part(To, {byte_size(To), -2}),
                                   case {FromPart, ToPart, CallerIdName, CalleeIdName} of
                                   % Special type of FreeSwitch channel (see FreeSwitch sources: src/switch_ivr_originate.c:2651)
                                       {_, _, <<"Outbound Call">>, _} -> 'loopback';
                                       {_, _, _, <<"Outbound Call">>} -> 'loopback';
                                   % Loopback patterns
                                       {<<"-a">>, _, _, _} -> 'loopback';
                                       {<<"-b">>, _, _, _} -> 'loopback';
                                       {_, <<"-a">>, _, _} -> 'loopback';
                                       {_, <<"-b">>, _, _} -> 'loopback';
                                       _ -> 'normal'
                                   end
                           end
                   end,
            % additional check if this leg is inboung originate
            IsInboundLeg = case {Result, Type} of
                               {_, 'loopback'} ->
                                   'false';
                               {[_, <<"inbound">>], _} ->
                                   'true';
                               _ ->
                                   'false'
                           end,
            CallId = wh_json:get_value(<<"Call-ID">>, JObj),
            IsOriginateLeg = (wh_json:get_value(<<"Other-Leg-Call-ID">>, JObj, CallId) =:= CallId),
            InboundOriginate = case (IsInboundLeg and IsOriginateLeg) of
                                   'true' -> 'orig';
                                   'false' -> 'nonorig'
                               end,
            lager:debug("Channel type is ~p", [{Result, Type, InboundOriginate}]),
            % store in cache
            IsChannelCreate = whapps_util:get_event_type(JObj) =:= {<<"call_event">>, <<"CHANNEL_CREATE">>},
            IsChannelCreate andalso cm_util:store_channel_type(CallId, {Result, Type, InboundOriginate}),
            {Result, Type, InboundOriginate};
        {ChannelProps, ChannelType, ChannelOrig} ->
            {ChannelProps, ChannelType, ChannelOrig}
    end.

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
            lager:debug("Account-ID not defined for this device"),
            {'error', 'not_found'};
        _ ->
            AuthType = wh_json:get_value([?CCV, <<"Authorizing-Type">>], JObj),
            DeviceRes = couch_mgr:open_cache_doc(wh_util:format_account_id(AccountId, 'encoded'), AuthID),
            AuthType1 = case AuthType of
                            'undefined' ->
                                lager:debug("Authorizing-Type not defined for this device"),
                                case DeviceRes of
                                    {'ok', _} ->
                                        lager:debug("Device document found so it's a device"),
                                        <<"device">>;
                                    _ ->
                                        lager:debug("Device document wasn't found so it's not a device"),
                                        'undefined'
                                end;
                            _ ->
                                lager:debug("Authorizing-Type found"),
                                AuthType
                        end,
            case AuthType1 of
                <<"device">> ->
                    {'ok', DeviceDoc} = DeviceRes,
                    case DeviceType = wh_json:get_value(<<"device_type">>, DeviceDoc) of
                        <<"sip_device">> ->
                            lager:debug("Device type is sip_device"),
                            DeviceName = wh_json:get_value(<<"name">>, DeviceDoc),
                            SipUserName = wh_json:get_value([<<"sip">>, <<"username">>], DeviceDoc),
                            {'ok', {DeviceName, SipUserName}};
                        <<"voip">> ->
                            lager:debug("Device type is voip"),
                            DeviceName = wh_json:get_value(<<"name">>, DeviceDoc),
                            SipUserName = wh_json:get_value([<<"sip">>, <<"username">>], DeviceDoc),
                            {'ok', {DeviceName, SipUserName}};
                        _ ->
                            lager:debug("Device type is ~p", [DeviceType]),
                            DeviceName = wh_json:get_value(<<"name">>, DeviceDoc),
                            {'ok', {DeviceName, <<"">>}}
                    end;
                _ ->
                    lager:debug("It's still not a device"),
                    {'ok', {<<"">>, <<"">>}}
            end
    end.

insert_device_info_if_needed(JObj, _Type) ->
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    lager:debug("Trying to get device info for CallID ~p", [CallId]),
%%     case wh_json:get_value([?CCV, <<"Originator-Type">>], JObj) of
%%         <<"SIP">> ->
            case ets:lookup(?ETS_DEVICE_INFO, CallId) of
                [] ->
                    lager:debug("Device info wasn't found in ETS. Trying to detect it."),
                    case get_sip_device_info(JObj) of
                        {'ok', {DeviceName, SipUserName}} ->
                            lager:debug("Store SIP Device Info in ETS: ~p", [{CallId, DeviceName, SipUserName}]),
                            ets:insert(?ETS_DEVICE_INFO, {CallId, DeviceName, SipUserName}),
                            wh_json:set_values([{[?CCV, <<"Device-Name">>], DeviceName}
                                ,{[?CCV, <<"Device-SIP-User-Name">>], SipUserName}]
                                ,JObj);
                        {'error', _} ->
                            lager:debug("No device info was found, so no changes were applied."),
                            JObj
                    end;
                [{CallId, DeviceName, SipUserName}] ->
                    lager:debug("Device info was found in ETS. Using it: ~p", [{CallId, DeviceName, SipUserName}]),
                    wh_json:set_values([{[?CCV, <<"Device-Name">>], DeviceName}
                        ,{[?CCV, <<"Device-SIP-User-Name">>], SipUserName}]
                        ,JObj)
            end.
%%         _Other ->
%%             lager:debug("Originator-Type is ~p so there no SIP info", [_Other]),
%%             JObj
%%     end.

mark_channel_as_loopback(CallId) ->
    lager:debug("Channel ~p marked as loopback", [CallId]),
    ets:insert(?ETS_LOOPBACK_CHANNELS, {CallId, 'loopback'}).

unmark_channel_as_loopback(CallId) ->
    lager:debug("Channel ~p unmarked as loopback", [CallId]),
    ets:delete(?ETS_LOOPBACK_CHANNELS, CallId).

is_channel_loopback(CallId) ->
    lager:debug("Channel ~p checked for loopback type", [CallId]),
    case ets:lookup(?ETS_LOOPBACK_CHANNELS, CallId) of
        [] -> 'false';
        [{CallId, 'loopback'}] -> 'true'
    end.

get_channel_fs_status(CallId) ->
    case whapps_call_command:fs_channel_status(CallId) of
        {'ok', ChannelStatus} ->
            ets:insert(?ETS_CACHED_CHANNEL_FS_STATUS, {CallId, ChannelStatus}),
            {'ok', ChannelStatus};
        {'error', Error} ->
            lager:error("Error (~p) when getting status of channel ~p from FreeSwitch. Trying get cached value...", [Error, CallId]),
            case ets:lookup(?ETS_CACHED_CHANNEL_FS_STATUS, CallId) of
                [] ->
                    lager:error("No status of channel ~p in cache", [CallId]),
                    {'error', 'not_found'};
                [{CallId, ChannelStatus}] ->
                    {'ok', ChannelStatus}
            end
    end.

cleanup_channel_fs_status(CallId) ->
    lager:debug("Cleanup cached channel status for the channel ~p", [CallId]),
    ets:delete(?ETS_CACHED_CHANNEL_FS_STATUS, CallId).

store_channel_type(CallId, ChannelType) ->
    lager:debug("Channel type ~p of channel ~p stored in cache", [ChannelType, CallId]),
    ets:insert(?ETS_CACHED_CHANNEL_TYPE, {CallId, ChannelType}).

read_channel_type(CallId) ->
    case ets:lookup(?ETS_CACHED_CHANNEL_TYPE, CallId) of
        [] ->
            lager:debug("No type of channel ~p was found in cache", [CallId]),
            {'error', 'not_found'};
        [{CallId, ChannelType}] ->
            lager:debug("Channel type ~p of channel ~p got from cache", [ChannelType, CallId]),
            ChannelType
    end.

cleanup_channel_type(CallId) ->
    lager:debug("Cleanup cached channel type for the channel ~p", [CallId]),
    ets:delete(?ETS_CACHED_CHANNEL_TYPE, CallId).

ets_add_inbound_originate_leg(OrigLegCallId, OrigJObj) ->
    lager:debug("Store inbound originate leg ~p", [OrigLegCallId]),
    ets:insert(?ETS_ORIG_INBOUND_LEG, {{OrigLegCallId}, 0, OrigJObj, 'orig'}).

ets_add_other_leg_of_inbound_originate_leg(OtherLegCallId, OtherJObj, OrigLegCallId) ->
    case ets:lookup(?ETS_ORIG_INBOUND_LEG, {OrigLegCallId}) of
        [] ->
            lager:warning("No inbound orig legs with CallID ~p", [OrigLegCallId]);
        [_|_] ->
            ets:insert(?ETS_ORIG_INBOUND_LEG, {{OrigLegCallId, OtherLegCallId}, OtherJObj, 'other'}),
            ets:update_counter(?ETS_ORIG_INBOUND_LEG, {OrigLegCallId}, {2, 1}),
            lager:debug("Store other leg ~p of inbound originate leg ~p. Number of legs is ~p",
                [OtherLegCallId, OrigLegCallId, ets:lookup_element(?ETS_ORIG_INBOUND_LEG, {OrigLegCallId}, 2)])
    end.

ets_update_leg_jobj_originator_type(OrigLegCallId, LegJObj) ->
    case ets:lookup(?ETS_ORIG_INBOUND_LEG, {OrigLegCallId}) of
        [] ->
            lager:warning("No inbound orig legs with CallID ~p", [OrigLegCallId]),
            LegJObj;
        [{{OrigLegCallId}, _, _, 'orig'}] ->
            case cm_util:get_channel_fs_status(OrigLegCallId) of
                {'ok', ChannelStatus} ->
                    lager:debug("Channel fs status: ~p", [ChannelStatus]),
                    OriginatorType = wh_json:get_value([?CCV, <<"Originator-Type">>], ChannelStatus),
                    case OriginatorType of
                        'undefined' ->
                            LegJObj;
                        _ ->
                            CallId = wh_json:get_value(<<"Call-ID">>, LegJObj),
                            lager:info("Current retrieved from the channel ~p FS Originator-Type is ~p. Copy it into the channel ~p",
                                [OrigLegCallId, OriginatorType, CallId]),
                            wh_json:set_values([{[?CCV, <<"Originator-Type">>], OriginatorType}], LegJObj)
                    end;
                {'error', Error} ->
                    lager:error("Error (~p) when getting status of channel ~p . No update.", [Error, OrigLegCallId]),
                    LegJObj
            end
    end.

ets_cleanup_other_and_orig_legs(OrigLegCallId, OtherLegCallId) ->
    case ets:lookup(?ETS_ORIG_INBOUND_LEG, {OrigLegCallId, OtherLegCallId}) of
        [] ->
            lager:warning("No records where other leg is ~p of inbound originate leg ~p",
                [OtherLegCallId, OrigLegCallId]);
        [{Key = {OrigLegCallId, OtherLegCallId}, _, 'other'}] ->
            lager:debug("Cleanup other leg ~p of inbound originate leg ~p", [OtherLegCallId, OrigLegCallId]),
            ets:delete(?ETS_ORIG_INBOUND_LEG, Key),
            ets:update_counter(?ETS_ORIG_INBOUND_LEG, {OrigLegCallId}, {2, -1}),
            case (Counter = ets:lookup_element(?ETS_ORIG_INBOUND_LEG, {OrigLegCallId}, 2)) > 0 of
                'true' ->
                    lager:debug("No needs to cleanup inbound originate leg ~p because ~p other legs still exists",
                        [OrigLegCallId, Counter]);
                'false' ->
                    lager:debug("Cleanup inbound originate leg ~p", [OrigLegCallId]),
                    ets:delete(?ETS_ORIG_INBOUND_LEG, {OrigLegCallId}),
                    cm_util:cleanup_channel_fs_status(OrigLegCallId)
            end
    end.


get_actual_aaa_document(_JObj, 'undefined') ->
    lager:debug("Search for actual AAA doc..."),
    lager:error("error: call for undefined account"),
    {'error', 'wrong_account_id'};
get_actual_aaa_document(JObj, AccountId) when is_binary(AccountId) ->
    lager:debug("Search for actual AAA doc..."),
    {'ok', Account} = couch_mgr:open_cache_doc(?WH_ACCOUNTS_DB, AccountId),
    AccountDb = wh_json:get_value(<<"pvt_account_db">>, Account),
    {'ok', AaaDoc} = couch_mgr:open_cache_doc(AccountDb, <<"aaa">>),
    case wh_json:get_value(<<"aaa_mode">>, AaaDoc) of
        <<"off">> ->
            lager:debug("Searching... AAA functionality disabled for the ~p account", [AccountId]),
            {'ok', 'aaa_mode_off'};
        <<"on">> ->
            % AAA functionality is on for this account
            lager:debug("Searching... AAA functionality enabled for the ~p account", [AccountId]),
            ParentAccountId = cm_util:parent_account_id(Account),
            maybe_suitable_servers(JObj, AaaDoc, AccountId, ParentAccountId);
        <<"inherit">> ->
            % AAA functionality is in the 'inherit' mode for this account
            lager:debug("Searching... AAA functionality enabled (inherit mode) for the ~p account", [AccountId]),
            ParentAccountId = cm_util:parent_account_id(Account),
            maybe_suitable_servers(JObj, AaaDoc, AccountId, ParentAccountId)
    end.

maybe_suitable_servers(JObj, AaaDoc, AccountId, ParentAccountId) ->
    AaaRequestType = cm_util:determine_aaa_request_type(JObj),
    ServersJson = wh_json:get_value(<<"servers">>, AaaDoc),
    lager:debug("Searching... available servers count for this account is ~p", [length(ServersJson)]),
    Servers = [S || S <- wh_json:recursive_to_proplist(ServersJson), props:get_is_true(<<"enabled">>, S)],
    lager:debug("Searching... active servers: ~p", [Servers]),
    ServersFilterArg = case AaaRequestType of
                           'authz' -> <<"authorization">>;
                           'accounting' -> <<"accounting">>
                       end,
    FilteredServers = [S || S <- Servers,
        lists:member(props:get_value(<<"name">>, S), wh_json:get_value(ServersFilterArg, AaaDoc))],
    maybe_server_request(FilteredServers, JObj, AaaDoc, AccountId, ParentAccountId).

maybe_server_request([], JObj, AaaDoc, _AccountId, ParentAccountId) ->
    lager:debug("Searching... all active AAA servers for this account were checked"),
    case wh_json:get_value(<<"aaa_mode">>, AaaDoc) of
        <<"inherit">> ->
            % we are in the "inherit" mode so we should get parent account and use its AAA settings
            lager:debug("Searching... the 'inherit' mode used - switching to parent account"),
            get_actual_aaa_document(JObj, ParentAccountId);
        _ ->
            lager:debug("Searching... no 'inherit' mode - servers search stopped"),
            {'error', 'no_respond'}
    end;
maybe_server_request(_AllServers, _JObj, AaaDoc, _AccountId, _ParentAccountId) ->
    lager:debug("Searching... AAA Doc found"),
    {'ok', AaaDoc}.
