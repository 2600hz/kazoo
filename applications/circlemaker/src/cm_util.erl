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
         ,determine_aaa_request_type/1]).

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
                    {Attr, list_to_binary(NewValue)}
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
            {Attr, Value}
    end.

-spec determine_aaa_request_type(wh_json:object()) -> atom().
determine_aaa_request_type(JObj) ->
    case {wh_json:get_value(<<"Event-Category">>, JObj), wh_json:get_value(<<"Event-Name">>, JObj)} of
        {<<"aaa">>, <<"aaa_authn_req">>} -> 'authn';
        {<<"authn">>, _} -> 'authn';
        {<<"authz">>, _} -> 'authz';
        {<<"call_event">>, <<"CHANNEL_CREATE">>} -> 'accounting';
        {<<"call_event">>, <<"CHANNEL_DESTROY">>} -> 'accounting'
    end.
