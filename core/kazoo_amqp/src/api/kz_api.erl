%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Kazoo API Helpers.
%%% Most API functions take a proplist, filter it against required headers
%%% and optional headers, and return either the JSON string if all
%%% required headers (default AND API-call-specific) are present, or an
%%% error if some headers are missing.
%%%
%%% To only check the validity, use the API call's corresponding *_v/1 function.
%%% This will parse the proplist and return a boolean()if the proplist is valid
%%% for creating a JSON message.
%%%
%%%
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_api).

%% API
-export([default_headers_v/1
        ,default_headers/2
        ,default_headers/3
        ,default_headers/4
        ,default_headers/5

        ,call_id/1, call_id/2
        ,account_id/1
        ,server_id/1
        ,queue_id/1
        ,msg_id/1, msg_id/2
        ,msg_reply_id/1
        ,event_category/1
        ,event_name/1
        ,app_name/1
        ,app_version/1
        ,node/1
        ,defer_response/1
        ,from_pid/1
        ,reply_to/1
        ]).

-export([is_federated_event/1
        ,event_zone/1
        ,exec/2
        ]).

-export([prepare_api_payload/2, prepare_api_payload/3]).
-export([set_missing_values/2]).
-export([remove_empty_values/1]).
-export([extract_defaults/1, remove_defaults/1]).
-export([disambiguate_and_publish/3]).
-export([error_resp/1, error_resp_v/1]).
-export([publish_error/2, publish_error/3]).
-export([public_fields/1]).

%% Other AMQP API validators can use these helpers
-export([build_message/3
        ,build_message_specific/3
        ,build_message_specific_headers/3
        ,validate/4
        ,validate_message/4
        ]).

-include_lib("kz_amqp_util.hrl").

-ifdef(TEST).
-export([has_any/2, has_all/2]).
-endif.

-export_type([api_formatter_return/0
             ,api_headers/0
             ,api_types/0
             ,api_valid_values/0
             ]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec server_id(kz_json:object()) -> kz_term:api_binary().
server_id(JObj) ->
    kz_json:get_ne_binary_value(?KEY_SERVER_ID, JObj).

-spec queue_id(kz_json:object()) -> kz_term:api_binary().
queue_id(JObj) ->
    kz_json:get_ne_binary_value(?KEY_QUEUE_ID, JObj, server_id(JObj)).

-spec event_category(kz_json:object()) -> kz_term:api_binary().
event_category(JObj) ->
    kz_json:get_value(?KEY_EVENT_CATEGORY, JObj).

-spec event_name(kz_term:api_terms()) -> kz_term:api_binary().
event_name(Props) when is_list(Props) ->
    props:get_value(?KEY_EVENT_NAME, Props);
event_name(JObj) ->
    kz_json:get_value(?KEY_EVENT_NAME, JObj).

-spec app_name(kz_json:object()) -> kz_term:api_binary().
app_name(JObj) ->
    kz_json:get_value(?KEY_APP_NAME, JObj).

-spec app_version(kz_json:object()) -> kz_term:api_binary().
app_version(JObj) ->
    kz_json:get_value(?KEY_APP_VERSION, JObj).

-spec node(kz_json:object()) -> kz_term:api_ne_binary().
node(JObj) ->
    kz_json:get_ne_binary_value(?KEY_NODE, JObj).

-spec msg_id(kz_term:api_terms()) -> kz_term:api_ne_binary().
msg_id(API) -> msg_id(API, 'undefined').

-spec msg_id(kz_term:api_terms(), Default) -> kz_term:ne_binary() | Default.
msg_id(Props, Default) when is_list(Props) ->
    props:get_ne_binary_value(?KEY_MSG_ID, Props, Default);
msg_id(JObj, Default) ->
    kz_json:get_ne_binary_value(?KEY_MSG_ID, JObj, Default).

-spec msg_reply_id(kz_term:api_terms()) -> kz_term:api_binary().
msg_reply_id(Props) when is_list(Props) ->
    props:get_value(?KEY_MSG_REPLY_ID, Props, msg_id(Props));
msg_reply_id(JObj) ->
    kz_json:get_value(?KEY_MSG_REPLY_ID, JObj, msg_id(JObj)).

-spec account_id(kz_term:api_terms()) -> kz_term:api_binary().
account_id(Props) when is_list(Props) ->
    props:get_value(?KEY_API_ACCOUNT_ID, Props);
account_id(JObj) ->
    kz_json:get_value(?KEY_API_ACCOUNT_ID, JObj).

-spec call_id(kz_term:api_terms()) -> kz_term:api_ne_binary().
call_id(API) ->
    call_id(API, 'undefined').

-spec call_id(kz_term:api_terms(), Default) -> kz_term:ne_binary() | Default.
call_id(Props, Default) when is_list(Props) ->
    props:get_ne_binary_value(?KEY_API_CALL_ID, Props, Default);
call_id(JObj, Default) ->
    kz_json:get_ne_binary_value(?KEY_API_CALL_ID, JObj, Default).

-spec defer_response(kz_term:api_terms()) -> boolean().
defer_response(Props) when is_list(Props) ->
    props:get_is_true(?KEY_DEFER_RESPONSE, Props);
defer_response(JObj) ->
    kz_json:is_true(?KEY_DEFER_RESPONSE, JObj).

-spec from_pid(kz_term:api_terms()) -> kz_term:api_binary().
from_pid(Props) when is_list(Props) ->
    props:get_value(?KEY_REQUEST_FROM_PID, Props);
from_pid(JObj) ->
    kz_json:get_value(?KEY_REQUEST_FROM_PID, JObj).

-spec reply_to(kz_term:api_terms()) -> kz_term:api_binary().
reply_to(Props) when is_list(Props) ->
    props:get_value(?KEY_REPLY_TO_PID, Props);
reply_to(JObj) ->
    kz_json:get_value(?KEY_REPLY_TO_PID, JObj).

%%------------------------------------------------------------------------------
%% @doc Default Headers in all messages - see wiki
%% Creates the seed proplist for the eventual message to be sent
%% All fields are required general headers.
%% @end
%%------------------------------------------------------------------------------

-spec default_headers(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:proplist().
default_headers(AppName, AppVsn) ->
    default_headers('undefined', AppName, AppVsn).

-spec default_headers(kz_term:api_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:proplist().
default_headers(ServerID, AppName, AppVsn) ->
    [{?KEY_SERVER_ID, ServerID}
    ,{?KEY_APP_NAME, AppName}
    ,{?KEY_APP_VERSION, AppVsn}
    ,{?KEY_NODE, kz_term:to_binary(node())}
    ].

-spec default_headers(kz_term:api_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:proplist().
default_headers(EvtCat, EvtName, AppName, AppVsn) ->
    default_headers(<<>>, EvtCat, EvtName, AppName, AppVsn).

-spec default_headers(kz_term:api_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:proplist().
default_headers(ServerID, EvtCat, EvtName, AppName, AppVsn) ->
    props:filter_empty(
      [{?KEY_SERVER_ID, ServerID}
      ,{?KEY_EVENT_CATEGORY, EvtCat}
      ,{?KEY_EVENT_NAME, EvtName}
      ,{?KEY_APP_NAME, AppName}
      ,{?KEY_APP_VERSION, AppVsn}
      ,{?KEY_NODE, kz_term:to_binary(node())}
      ]).

-spec default_headers_v(kz_term:api_terms()) -> boolean().


default_headers_v(Props) when is_list(Props) ->
    Filtered = props:filter_empty(Props),
    lists:all(fun(K) -> default_header_v(K, Filtered) end, ?DEFAULT_HEADERS);
default_headers_v(JObj) ->
    default_headers_v(kz_json:to_proplist(JObj)).

-spec default_header_v(kz_term:ne_binary(), kz_term:proplist()) -> boolean().
default_header_v(Header, Props) ->
    not kz_term:is_empty(props:get_value(Header, Props)).

-spec disambiguate_and_publish(kz_json:object(), kz_json:object(), kz_term:ne_binary() | atom()) -> any().
disambiguate_and_publish(ReqJObj, RespJObj, Binding) ->
    Wapi = list_to_binary([<<"kapi_">>, kz_term:to_binary(Binding)]),
    ApiMod = kz_term:to_atom(Wapi),
    ApiMod:disambiguate_and_publish(ReqJObj, RespJObj).

%%------------------------------------------------------------------------------
%% @doc Set any missing defaults with the values defined in the by the
%% validation definitions and remove any empty values
%% @end
%%------------------------------------------------------------------------------
-type api_formatter_fun() :: fun((kz_term:api_terms()) -> api_formatter_return()).
-type prepare_option_el() :: {'formatter', api_formatter_fun()} |
                             {'remove_recursive', boolean()}.
-type prepare_options() :: [prepare_option_el()].


-spec prepare_api_payload(kz_term:api_terms(), kz_term:proplist()) -> api_formatter_return() | kz_term:proplist().
prepare_api_payload(Prop, HeaderValues) ->
    prepare_api_payload(Prop, HeaderValues, []).

-spec prepare_api_payload(kz_term:api_terms(), kz_term:proplist(), api_formatter_fun() | prepare_options()) ->
                                 api_formatter_return() | kz_term:proplist().
prepare_api_payload(Prop, HeaderValues, FormatterFun) when is_function(FormatterFun, 1) ->
    prepare_api_payload(Prop, HeaderValues, [{'formatter', FormatterFun}]);
prepare_api_payload(Prop, HeaderValues, Options) when is_list(Prop) ->
    FormatterFun = props:get_value('formatter', Options, fun kz_term:identity/1),
    CleanupFuns = [fun (P) -> remove_empty_values(P, props:get_is_true('remove_recursive', Options, 'true')) end
                  ,fun (P) -> set_missing_values(P, ?DEFAULT_VALUES) end
                  ,fun (P) -> set_missing_values(P, HeaderValues) end
                  ],
    FormatterFun(lists:foldr(fun(F, P) -> F(P) end, Prop, CleanupFuns));
prepare_api_payload(JObj, HeaderValues, Options) ->
    prepare_api_payload(kz_json:to_proplist(JObj), HeaderValues, Options).

%%------------------------------------------------------------------------------
%% @doc Set any missing defaults with the values defined in the by the
%% validation definitions
%% @end
%%------------------------------------------------------------------------------
-spec set_missing_values(kz_term:api_terms(), kz_term:proplist()) -> kz_term:api_terms().
set_missing_values(Prop, HeaderValues) when is_list(Prop) ->
    lists:foldl(fun({_, V}, PropAcc) when is_list(V) ->
                        PropAcc;
                   ({K, _}=KV, PropAcc) ->
                        case should_strip_from_payload(props:get_value(K, Prop)) of
                            'true' -> [ KV | PropAcc ];
                            'false' -> PropAcc
                        end
                end, Prop, HeaderValues);
set_missing_values(JObj, HeaderValues) ->
    kz_json:from_list(set_missing_values(kz_json:to_proplist(JObj), HeaderValues)).

%%------------------------------------------------------------------------------
%% @doc Recursively cleans a proplist or json object, returning the same
%% type given
%% @end
%%------------------------------------------------------------------------------
-spec remove_empty_values(kz_term:api_terms()) -> kz_term:api_terms().
remove_empty_values(API) ->
    remove_empty_values(API, 'true').

remove_empty_values(Prop, Recursive) when is_list(Prop) ->
    do_empty_value_removal(Prop, Recursive, []);
remove_empty_values(JObj, Recursive) ->
    Prop = remove_empty_values(kz_json:to_proplist(JObj), Recursive),
    kz_json:from_list(Prop).

do_empty_value_removal([], _Recursive, Acc) ->
    lists:reverse(Acc);
do_empty_value_removal([{?KEY_SERVER_ID,_}=KV|T], Recursive, Acc) ->
    do_empty_value_removal(T, Recursive, [KV|Acc]);
do_empty_value_removal([{?KEY_MSG_ID,_}=KV|T], Recursive, Acc) ->
    do_empty_value_removal(T, Recursive, [KV|Acc]);
do_empty_value_removal([{K,V}=KV|T], Recursive, Acc) ->
    case should_strip_from_payload(V) of
        'true' -> do_empty_value_removal(T, Recursive, Acc);
        'false' ->
            case (kz_json:is_json_object(V)
                  orelse kz_term:is_proplist(V)
                 )
                andalso Recursive
            of
                'true' ->
                    SubElm = {K, remove_empty_values(V, Recursive)},
                    do_empty_value_removal(T, Recursive, [SubElm|Acc]);
                'false' ->
                    do_empty_value_removal(T, Recursive, [KV|Acc])
            end
    end.

-spec should_strip_from_payload(any()) -> boolean().
should_strip_from_payload('undefined') -> 'true';
should_strip_from_payload(<<>>) -> 'true';
should_strip_from_payload(_) -> 'false'.

%%------------------------------------------------------------------------------
%% @doc Extract just the default headers from a message
%% @end
%%------------------------------------------------------------------------------
-spec extract_defaults(kz_term:api_terms()) -> kz_term:proplist().
extract_defaults(Prop) when is_list(Prop) ->
    %% not measurable faster over the foldl, but cleaner (IMO)
    [ {H, V} || H <- ?DEFAULT_HEADERS ++ ?OPTIONAL_DEFAULT_HEADERS,
                (V = props:get_value(H, Prop)) =/= 'undefined'
    ];
extract_defaults(JObj) ->
    extract_defaults(kz_json:to_proplist(JObj)).

-spec remove_defaults(kz_term:api_terms()) -> kz_term:api_terms().
remove_defaults(Prop) when is_list(Prop) ->
    props:delete_keys(?OPTIONAL_DEFAULT_HEADERS
                     ,props:delete_keys(?DEFAULT_HEADERS, Prop)
                     );
remove_defaults(JObj) ->
    kz_json:delete_keys(?OPTIONAL_DEFAULT_HEADERS
                       ,kz_json:delete_keys(?DEFAULT_HEADERS, JObj)
                       ).

%%------------------------------------------------------------------------------
%% @doc Format an error event
%% Takes proplist, creates JSON string or error
%% @end
%%------------------------------------------------------------------------------
-spec error_resp(kz_term:api_terms()) -> api_formatter_return().
error_resp(Prop) when is_list(Prop) ->
    case error_resp_v(Prop) of
        'true' -> build_message(Prop, ?ERROR_RESP_HEADERS, ?OPTIONAL_ERROR_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for error_resp"}
    end;
error_resp(JObj) ->
    error_resp(kz_json:to_proplist(JObj)).

-spec error_resp_v(kz_term:api_terms()) -> boolean().
error_resp_v(Prop) when is_list(Prop) ->
    validate(Prop, ?ERROR_RESP_HEADERS, ?ERROR_RESP_VALUES, ?ERROR_RESP_TYPES);
error_resp_v(JObj) ->
    error_resp_v(kz_json:to_proplist(JObj)).

-spec publish_error(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_error(TargetQ, JObj) ->
    publish_error(TargetQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_error(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_error(TargetQ, Error, ContentType) ->
    {'ok', Payload} = prepare_api_payload(Error, ?ERROR_RESP_VALUES, fun error_resp/1),
    kz_amqp_util:targeted_publish(TargetQ, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Sanitizes generic AMQP payloads
%% This function removes the default sensitive data that should not be
%% exposed externally.
%% @end
%%------------------------------------------------------------------------------
-spec public_fields(kz_json:object()) -> kz_json:object().
public_fields(JObj) ->
    Routines = [fun remove_optional_defaults/1],
    lists:foldl(fun(F, J) -> F(J) end, JObj, Routines).

-spec remove_optional_defaults(kz_json:object()) -> kz_json:object().
remove_optional_defaults(JObj) ->
    Keys = [Key || Key <- ?OPTIONAL_DEFAULT_HEADERS
                       ,Key /= ?KEY_API_ACCOUNT_ID
                       ,Key /= ?KEY_API_CALL_ID
           ],
    kz_json:delete_keys(Keys, JObj).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate(kz_term:api_terms(), api_headers(), api_valid_values(), api_types()) -> boolean().
validate(Prop, ReqH, Vals, Types) when is_list(Prop) ->
    case has_all(Prop, ?DEFAULT_HEADERS)
        andalso validate_message(Prop, ReqH, Vals, Types)
    of
        'true' -> 'true';
        'false' ->
            lager:debug("failing API JSON: ~s", [kz_json:encode(kz_json:from_list(Prop))]),
            'false'
    end;
validate(JObj, ReqH, Vals, Types) ->
    validate(kz_json:to_proplist(JObj), ReqH, Vals, Types).

-spec validate_message(kz_term:api_terms(), api_headers(), kz_term:proplist(), kz_term:proplist()) -> boolean().
validate_message(Prop, ReqH, Vals, Types) when is_list(Prop) ->
    has_all(Prop, ReqH)
        andalso values_check(Prop, Vals)
        andalso type_check(Prop, Types);
validate_message(JObj, ReqH, Vals, Types) ->
    validate_message(kz_json:to_proplist(JObj), ReqH, Vals, Types).

-spec build_message(kz_term:api_terms(), api_headers(), api_headers()) -> api_formatter_return().
build_message(Prop, ReqH, OptH) when is_list(Prop) ->
    case defaults(Prop, ReqH ++ OptH) of
        {'error', _Reason}=Error ->
            lager:debug("API message does not have the default headers ~s: ~p"
                       ,[string:join([kz_term:to_list(H) || H <- ReqH], ","), Error]
                       ),
            Error;
        HeadAndProp ->
            case build_message_specific_headers(HeadAndProp, ReqH, OptH) of
                {'ok', FinalHeaders} -> headers_to_json(FinalHeaders);
                Err -> Err
            end
    end;
build_message(JObj, ReqH, OptH) ->
    build_message(kz_json:to_proplist(JObj), ReqH, OptH).

-spec build_message_specific_headers(kz_term:proplist() | {api_headers(), kz_term:proplist()}, api_headers(), api_headers()) ->
                                            {'ok', kz_term:proplist()} |
                                            {'error', string()}.
build_message_specific_headers({Headers, Prop}, ReqH, OptH) ->
    case update_required_headers(Prop, ReqH, Headers) of
        {'error', _Reason} = Error ->
            lager:debug("API message does not have the required headers ~s: ~p : ~p"
                       ,[kz_binary:join(ReqH, <<",">>), Error, ReqH]
                       ),
            Error;
        {Headers1, Prop1} ->
            {Headers2, _Prop2} = update_optional_headers(Prop1, OptH, Headers1),
            {'ok', Headers2}
    end;
build_message_specific_headers(Prop, ReqH, OptH) ->
    build_message_specific_headers({[], Prop}, ReqH, OptH).

-spec build_message_specific(kz_term:proplist() | {api_headers(), kz_term:proplist()}, api_headers(), api_headers()) ->
                                    api_formatter_return().
build_message_specific({Headers, Prop}, ReqH, OptH) ->
    case update_required_headers(Prop, ReqH, Headers) of
        {'error', _Reason} = Error ->
            lager:debug("API message does not have the required headers ~s: ~p"
                       ,[kz_binary:join(ReqH, <<",">>), Error]
                       ),
            Error;
        {Headers1, Prop1} ->
            {Headers2, _Prop2} = update_optional_headers(Prop1, OptH, Headers1),
            headers_to_json(Headers2)
    end;
build_message_specific(Prop, ReqH, OptH) ->
    build_message_specific({[], Prop}, ReqH, OptH).

-spec headers_to_json(kz_term:proplist()) -> api_formatter_return().
headers_to_json([_|_]=HeadersProp) ->
    _ = kz_log:kz_log_md_put('msg_id', msg_id(HeadersProp)),
    try kz_json:encode(kz_json:from_list(HeadersProp)) of
        JSON -> {'ok', JSON}
    catch
        _What:_Why -> {'error', io_lib:format("KAZOO TO_JSON ~p: ~p~n~p~n", [_What, _Why, HeadersProp])}
    end.

%% Checks Prop for all default headers, throws error if one is missing
%% defaults(PassedProps, MessageHeaders) -> { Headers, NewPropList } | {error, Reason}

-spec defaults(kz_term:api_terms(), api_headers()) ->
                      {kz_term:proplist(), kz_term:proplist()} |
                      {'error', string()}.
defaults(Prop, MsgHeaders) -> defaults(Prop, expand_headers(MsgHeaders), []).
defaults(Prop, MsgHeaders, Headers) ->
    case update_required_headers(Prop, ?DEFAULT_HEADERS -- MsgHeaders, Headers) of
        {'error', _Reason} = Error ->
            Error;
        {Headers1, Prop1} ->
            update_optional_headers(Prop1, ?OPTIONAL_DEFAULT_HEADERS -- MsgHeaders, Headers1)
    end.

-spec expand_headers(list()) -> kz_term:ne_binaries().
expand_headers(Headers) ->
    lists:foldr(fun expand_header/2, [], Headers).

-spec expand_header(kz_term:ne_binary() | kz_term:ne_binaries(), kz_term:ne_binaries()) -> kz_term:ne_binaries().
expand_header(Header, Acc)
  when is_binary(Header) -> [Header | Acc];
expand_header(Headers, Acc)
  when is_list(Headers) -> expand_headers(Headers) ++ Acc.

-spec update_required_headers(kz_term:proplist(), api_headers(), kz_term:proplist()) ->
                                     {kz_term:proplist(), kz_term:proplist()} |
                                     {'error', string()}.
update_required_headers(Prop, Fields, Headers) ->
    case has_all(Prop, Fields) of
        'true' -> add_headers(Prop, Fields, Headers);
        'false' -> {'error', "All required headers not defined"}
    end.

-spec update_optional_headers(kz_term:proplist(), api_headers(), kz_term:proplist()) ->
                                     {kz_term:proplist(), kz_term:proplist()}.
update_optional_headers(Prop, Fields, Headers) ->
    case has_any(Prop, Fields) of
        'true' -> add_optional_headers(Prop, Fields, Headers);
        'false' -> {Headers, Prop}
    end.

%% add [Header] from Prop to HeadProp
-spec add_headers(kz_term:proplist(), api_headers(), kz_term:proplist()) ->
                         {kz_term:proplist(), kz_term:proplist()}.
add_headers(Prop, Fields, Headers) ->
    lists:foldl(fun(K, {Headers1, KVs}) when is_list(K) ->
                        K1 = [Ki || Ki <- K, props:is_defined(Ki, KVs)],
                        add_headers(KVs, K1, Headers1);
                   (K, {Headers1, KVs}) ->
                        {[{K, props:get_value(K, KVs)} | Headers1], props:delete(K, KVs)}
                end, {Headers, Prop}, Fields).

-spec add_optional_headers(kz_term:proplist(), api_headers(), kz_term:proplist()) ->
                                  {kz_term:proplist(), kz_term:proplist()}.
add_optional_headers(Prop, Fields, Headers) ->
    lists:foldl(fun(K, {Headers1, KVs}) ->
                        case props:get_value(K, KVs) of
                            'undefined' -> {Headers1, KVs};
                            V -> {[{K, V} | Headers1], props:delete(K, KVs)}
                        end
                end, {Headers, Prop}, Fields).

%% Checks Prop against a list of required headers, returns true | false
-spec has_all(kz_term:proplist(), api_headers()) -> boolean().
has_all(Prop, Headers) ->
    lists:all(fun(Header) -> has_all_header(Header, Prop) end, Headers).

-spec has_all_header(api_headers(), kz_term:proplist()) -> boolean().
has_all_header(Headers, Prop) when is_list(Headers) ->
    case has_any(Prop, Headers) of
        'true' -> 'true';
        'false' ->
            lager:debug("failed to find one of keys '~p' on API message", [Headers]),
            'false'
    end;
has_all_header(Header, Prop) when is_binary(Header) ->
    case props:is_defined(Header, Prop) of
        'true' -> 'true';
        'false' ->
            lager:debug("failed to find key '~s' on API message", [Header]),
            'false'
    end.

%% Checks Prop against a list of optional headers, returns true | false if at least one if found
-spec has_any(kz_term:proplist(), api_headers()) -> boolean().
has_any(Prop, Headers) ->
    lists:any(fun(Header) -> props:is_defined(Header, Prop) end, Headers).

%% checks Prop against a list of values to ensure known key/value pairs are correct (like Event-Category
%% and Event-Name). We don't care if a key is defined in Values and not in Prop; that is handled by has_all/1
-spec values_check(kz_term:proplist(), kz_term:proplist()) -> boolean().
values_check(Prop, Values) ->
    lists:all(fun(Value) -> values_check_all(Prop, Value) end, Values).

-spec values_check_all(kz_term:proplist(), {any(), any()}) -> boolean().
values_check_all(Prop, {Key, Vs}) when is_list(Vs) ->
    case props:get_value(Key, Prop) of
        'undefined' -> 'true'; % isn't defined in Prop, has_all will error if req'd
        V ->
            case lists:member(V, Vs) of
                'true' -> 'true';
                'false' ->
                    lager:debug("API key '~s' value '~p' is not one of the values: ~p"
                               ,[Key, V, Vs]
                               ),
                    'false'
            end
    end;
values_check_all(Prop, {Key, V}) ->
    case props:get_value(Key, Prop) of
        'undefined' -> 'true'; % isn't defined in Prop, has_all will error if req'd
        V -> 'true';
        _Val ->
            lager:debug("API key '~s' value '~p' is not '~p'", [Key, _Val, V]),
            'false'
    end.

%% checks Prop against a list of {Key, Fun}, running the value of Key through Fun, which returns a
%% boolean.
-type typecheck() :: {kz_term:ne_binary(), fun((_) -> boolean())}.
-type typechecks() :: [typecheck()].
-spec type_check(kz_term:proplist(), typechecks()) -> boolean().
type_check(Prop, Types) ->
    lists:all(fun(Type) -> type_check_all(Prop, Type) end, Types).

-spec type_check_all(kz_term:proplist(), typecheck()) -> boolean().
type_check_all(Prop, {Key, Fun}) ->
    case props:get_value(Key, Prop) of
        %% isn't defined in Prop, has_all will error if req'd
        'undefined' -> 'true';
        Value ->
            try Fun(Value) of % returns boolean
                'true' -> 'true';
                'false' ->
                    lager:debug("API key '~s' value '~p' failed validation fun", [Key, Value]),
                    'false'
            catch
                _:_R ->
                    lager:debug("API key '~s' value '~p' caused validation fun exception: ~p", [Key, Value, _R]),
                    'false'
            end
    end.

-spec is_federated_event(kz_json:object()) -> boolean().
is_federated_event(JObj) ->
    case kz_json:get_ne_binary_value(?KEY_AMQP_BROKER, JObj) of
        'undefined' -> 'false';
        _Broker -> 'true'
    end.

-spec event_zone(kz_json:object()) -> kz_term:ne_binary().
event_zone(JObj) ->
    kz_json:get_ne_binary_value(?KEY_AMQP_ZONE, JObj).

-type exec_fun_1() :: fun((kz_term:api_terms()) -> kz_term:api_terms()).
-type exec_fun_2() :: {fun((_, kz_term:api_terms()) -> kz_term:api_terms()), _}.
-type exec_fun_3() :: {fun((_, _, kz_term:api_terms()) -> kz_term:api_terms()), _, _}.
-type exec_fun() :: exec_fun_1() | exec_fun_2() | exec_fun_3().
-type exec_funs() :: [exec_fun(),...].

-define(is_json(Obj), is_tuple(Obj)
        andalso is_list(element(1, Obj))
       ).

-spec exec(exec_funs(), kz_term:api_terms()) -> kz_term:api_terms().
exec(Funs, API)
  when is_list(API);
       ?is_json(API) ->
    lists:foldl(fun exec_fold/2, API, Funs).

-spec exec_fold(exec_fun(), kz_term:api_terms()) -> kz_term:api_terms().
exec_fold({F, K, V}, C) when is_function(F, 3) -> F(K, V, C);
exec_fold({F, V}, C) when is_function(F, 2) -> F(V, C);
exec_fold(F, C) when is_function(F, 1) -> F(C);
exec_fold(_, C) -> C.
