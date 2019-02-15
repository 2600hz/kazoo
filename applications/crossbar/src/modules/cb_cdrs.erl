%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc CDR
%%% Read only access to CDR docs
%%%
%%%
%%% @author Edouard Swiac
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% @author Ben Wann
%%% @author Sponsored by GTNetwork LLC, Implemented by SIPLABS LLC
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_cdrs).

-export([init/0
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2
        ,resource_exists/0, resource_exists/1, resource_exists/2
        ,content_types_provided/1, content_types_provided/2, content_types_provided/3
        ,validate/1, validate/2, validate/3
        ,to_json/1
        ,to_csv/1
        ]).

-ifdef(TEST).
-export([handle_utc_time_offset/2]).
-endif.

-export([fix_qs_filter_keys/1
        ,normalize_cdr/3
        ]).

-include("crossbar.hrl").

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".cdrs">>).
-define(MAX_BULK, kapps_config:get_pos_integer(?MOD_CONFIG_CAT, <<"maximum_bulk">>, 50)).
-define(STALE_CDR, kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"cdr_stale_view">>, 'false')).

-define(CB_LIST, <<"cdrs/crossbar_listing">>).
-define(CB_LIST_BY_USER, <<"cdrs/listing_by_owner">>).
-define(CB_INTERACTION_LIST, <<"interactions/interaction_listing">>).
-define(CB_INTERACTION_LIST_BY_USER, <<"interactions/interaction_listing_by_owner">>).
-define(CB_INTERACTION_LIST_BY_ID, <<"interactions/interaction_listing_by_id">>).
-define(CB_SUMMARY_VIEW, <<"cdrs/summarize_cdrs">>).
-define(CB_SUMMARY_LIST, <<"format_summary">>).

-define(PATH_INTERACTION, <<"interaction">>).
-define(PATH_LEGS, <<"legs">>).
-define(PATH_SUMMARY, <<"summary">>).

-define(KEY_UTC_OFFSET, <<"utc_offset">>).
-define(KEY_CCV, <<"custom_channel_vars">>).

-define(COLUMNS
       ,[{<<"id">>, fun col_id/3}
        ,{<<"call_id">>, fun col_call_id/3}
        ,{<<"caller_id_number">>, fun col_caller_id_number/3}
        ,{<<"caller_id_name">>, fun col_caller_id_name/3}
        ,{<<"callee_id_number">>, fun col_callee_id_number/3}
        ,{<<"callee_id_name">>, fun col_callee_id_name/3}
        ,{<<"duration_seconds">>, fun col_duration_seconds/3}
        ,{<<"billing_seconds">>, fun col_billing_seconds/3}
        ,{<<"timestamp">>, fun col_timestamp/3}
        ,{<<"hangup_cause">>, fun col_hangup_cause/3}
        ,{<<"other_leg_call_id">>, fun col_other_leg_call_id/3}
        ,{<<"owner_id">>, fun col_owner_id/3}
        ,{<<"to">>, fun col_to/3}
        ,{<<"from">>, fun col_from/3}
        ,{<<"direction">>, fun col_call_direction/3}
        ,{<<"request">>, fun col_request/3}
        ,{<<"authorizing_id">>, fun col_authorizing_id/3}
        ,{<<"cost">>, fun col_customer_cost/3}
         %% New fields
        ,{<<"dialed_number">>, fun col_dialed_number/3}
        ,{<<"calling_from">>, fun col_calling_from/3}
        ,{<<"datetime">>, fun col_pretty_print/3}
        ,{<<"unix_timestamp">>, fun col_unix_timestamp/3}
        ,{<<"rfc_1036">>, fun col_rfc1036/3}
        ,{<<"iso_8601">>, fun col_iso8601/3}
        ,{<<"iso_8601_combined">>, fun col_iso8601_combined/3}
        ,{<<"call_type">>, fun col_account_call_type/3}
        ,{<<"rate">>, fun col_rate/3}
        ,{<<"rate_name">>, fun col_rate_name/3}
        ,{<<"bridge_id">>, fun col_bridge_id/3}
        ,{<<"recording_url">>, fun col_recording_url/3}
        ,{<<"media_recordings">>, fun col_media_recordings/3}
        ,{<<"media_server">>, fun col_media_server/3}
        ,{<<"call_priority">>, fun col_call_priority/3}
        ,{<<"interaction_id">>, fun col_interaction_id/3}
        ]).

-define(COLUMNS_RESELLER
       ,[{<<"reseller_cost">>, fun col_reseller_cost/3}
        ,{<<"reseller_call_type">>, fun col_reseller_call_type/3}
        ]).

-type csv_column_fun() :: fun((kz_json:object(), kz_time:gregorian_seconds(), cb_context:context()) -> kz_term:ne_binary()).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.cdrs">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.cdrs">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.cdrs">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.to_json.get.cdrs">>, ?MODULE, 'to_json'),
    _ = crossbar_bindings:bind(<<"*.to_csv.get.cdrs">>, ?MODULE, 'to_csv'),
    _ = crossbar_bindings:bind(<<"*.validate.cdrs">>, ?MODULE, 'validate'),
    'ok'.

-spec to_json(cb_cowboy_payload()) -> cb_cowboy_payload().
to_json({Req, Context}) ->
    {Req, to_response(Context, <<"json">>, cb_context:req_nouns(Context))}.

-spec to_csv(cb_cowboy_payload()) -> cb_cowboy_payload().
to_csv({Req, Context}) ->
    {Req, to_response(Context, <<"csv">>, cb_context:req_nouns(Context))}.

-spec to_response(cb_context:context(), kz_term:ne_binary(), req_nouns()) ->
                         cb_context:context().
to_response(Context, _, [{<<"cdrs">>, []}, {?KZ_ACCOUNTS_DB, _}|_]) ->
    Context;
to_response(Context, _, [{<<"cdrs">>, []}, {<<"users">>, _}|_]) ->
    Context;
to_response(Context, RespType, [{<<"cdrs">>, [?PATH_INTERACTION]}, {?KZ_ACCOUNTS_DB, _}|_]) ->
    load_chunked_cdrs(Context, RespType);
to_response(Context, RespType, [{<<"cdrs">>, [?PATH_INTERACTION]}, {<<"users">>, _}|_]) ->
    load_chunked_cdrs(Context, RespType);
to_response(Context, _, _) ->
    Context.

%%------------------------------------------------------------------------------
%% @doc This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/cdr/' can only accept GET
%%
%% Failure here returns `405 Method Not Allowed'.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?PATH_INTERACTION) ->
    [?HTTP_GET];
allowed_methods(?PATH_SUMMARY) ->
    [?HTTP_GET];
allowed_methods(_CDRId) ->
    [?HTTP_GET].

-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(?PATH_LEGS, _InteractionId) ->
    [?HTTP_GET].

%%------------------------------------------------------------------------------
%% @doc This function determines if the provided list of Nouns are valid.
%% Failure here returns `404 Not Found'.
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> boolean().
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> boolean().
resource_exists(_) -> 'true'.

-spec resource_exists(path_token(), path_token()) -> boolean().
resource_exists(?PATH_LEGS, _) -> 'true';
resource_exists(_, _) -> 'false'.

%%------------------------------------------------------------------------------
%% @doc Add content types accepted and provided by this module
%% @end
%%------------------------------------------------------------------------------
-spec content_types_provided(cb_context:context()) -> cb_context:context().
content_types_provided(Context) ->
    provided_types(Context).

-spec content_types_provided(cb_context:context(), path_token()) -> cb_context:context().
content_types_provided(Context, _) ->
    provided_types(Context).

-spec content_types_provided(cb_context:context(), path_token(), path_token()) -> cb_context:context().
content_types_provided(Context, _, _) ->
    provided_types(Context).

-spec provided_types(cb_context:context()) -> cb_context:context().
provided_types(Context) ->
    cb_context:add_content_types_provided(Context
                                         ,[{'to_json', ?JSON_CONTENT_TYPES}
                                          ,{'to_csv', ?CSV_CONTENT_TYPES}
                                          ]).

%%------------------------------------------------------------------------------
%% @doc This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400.
%% @end
%%------------------------------------------------------------------------------

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_utc_offset(Context).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, ?PATH_INTERACTION) ->
    validate_chunk_view(Context);
validate(Context, ?PATH_SUMMARY) ->
    load_cdr_summary(Context);
validate(Context, CDRId) ->
    load_cdr(CDRId, Context).

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, ?PATH_LEGS, InteractionId) ->
    load_legs(InteractionId, Context);
validate(Context, _, _) ->
    lager:debug("invalid URL chain for cdr request"),
    cb_context:add_system_error('faulty_request', Context).

-spec validate_utc_offset(cb_context:context()) -> cb_context:context().
validate_utc_offset(Context) ->
    UTCSecondsOffset = cb_context:req_value(Context, ?KEY_UTC_OFFSET),
    validate_utc_offset(Context, UTCSecondsOffset).

-spec validate_utc_offset(cb_context:context(), kz_time:gregorian_seconds()) -> cb_context:context().
validate_utc_offset(Context, 'undefined') ->
    validate_chunk_view(Context);
validate_utc_offset(Context, 'true') ->
    crossbar_util:response('error', <<"utc_offset must be a number">>, 404, Context);
validate_utc_offset(Context, UTCSecondsOffset) ->
    try kz_term:to_number(UTCSecondsOffset) of
        _ ->
            lager:debug("adjusting CDR datetime field with UTC Time Offset: ~p", [UTCSecondsOffset]),
            validate_chunk_view(Context)
    catch
        'error':'badarg' ->
            crossbar_util:response('error', <<"utc_offset must be a number">>, 404, Context)
    end.

-spec validate_chunk_view(cb_context:context()) -> cb_context:context().
validate_chunk_view(Context) ->
    case get_view_options(cb_context:req_nouns(Context)) of
        {'undefined', []} ->
            lager:debug("invalid URL chain for cdrs request"),
            cb_context:add_system_error('faulty_request', Context);
        {ViewName, Options} ->
            load_chunk_view(Context, ViewName, Options)
    end.

-spec load_chunk_view(cb_context:context(), kz_term:ne_binary(), kz_term:proplist()) -> cb_context:context().
load_chunk_view(Context, ViewName, Options0) ->
    AuthAccountId = cb_context:auth_account_id(Context),

    Setters = [{fun cb_context:store/3, 'has_cdr_filter', crossbar_filter:is_defined(Context)}
              ,{fun cb_context:store/3, 'is_reseller', kz_services_reseller:is_reseller(AuthAccountId)}
              ],
    Options = [{'is_chunked', 'true'}
              ,{'chunk_size', ?MAX_BULK}
               | Options0
              ],
    crossbar_view:load_modb(cb_context:setters(fix_qs_filter_keys(Context), Setters), ViewName, Options).

-spec fix_qs_filter_keys(cb_context:context()) -> cb_context:context().
fix_qs_filter_keys(Context) ->
    NewQs = kz_json:map(fun(K, V) -> fix_filter_key(kz_binary:reverse(K), V) end
                       ,cb_context:query_string(Context)
                       ),
    cb_context:set_query_string(Context, NewQs).

-spec fix_filter_key(kz_term:ne_binary(), any()) -> {kz_term:ne_binary(), any()}.
fix_filter_key(<<"di_llac", _/binary>> = Key, Val) ->
    {kz_binary:reverse(Key), fix_filter_call_id(Val)};
fix_filter_key(Key, Val) ->
    {kz_binary:reverse(Key), Val}.

fix_filter_call_id(?MATCH_MODB_PREFIX(_Year, _Month, CallId)) ->
    CallId;
fix_filter_call_id(CallId) ->
    CallId.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Attempt to CDRs summary.
%% @end
%%------------------------------------------------------------------------------
-spec load_cdr_summary(cb_context:context()) -> cb_context:context().
load_cdr_summary(Context) ->
    lager:debug("loading cdr summary for account ~s", [cb_context:account_id(Context)]),
    Options = [{'mapper', fun normalize_summary_results/2}
              ,{'range_start_keymap', []}
              ,{'range_end_keymap', crossbar_view:suffix_key_fun([kz_json:new()])}
              ,{'list', ?CB_SUMMARY_LIST}
              ],
    C1 = crossbar_view:load_modb(Context, ?CB_SUMMARY_VIEW, Options),
    case cb_context:resp_status(C1) of
        'success' ->
            JObjs = cb_context:resp_data(C1),
            cb_context:set_resp_data(C1, lists:foldl(fun merge_cdr_summary/2, kz_json:new(), JObjs));
        _ -> C1
    end.

-spec merge_cdr_summary(kz_json:object(), kz_json:objects()) -> kz_json:object().
merge_cdr_summary(JObj1, JObj2) ->
    kz_json:foldl(fun(Key1, Value1, JObj) ->
                          case kz_json:get_value(Key1, JObj1) of
                              'undefined' -> kz_json:set_value(Key1, Value1, JObj);
                              Value1 when is_integer(Value1) ->
                                  kz_json:set_value(Key1, Value1 + Value1, JObj);
                              Value1 ->
                                  NewValue = merge_cdr_summary(Value1, Value1),
                                  kz_json:set_value(Key1, NewValue, JObj)
                          end
                  end
                 ,JObj2
                 ,JObj1
                 ).

-spec normalize_summary_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_summary_results(JObj, Acc) -> [JObj|Acc].

%%------------------------------------------------------------------------------
%% @doc Generate specific view options for the path.
%% @end
%%------------------------------------------------------------------------------
-spec get_view_options(req_nouns()) -> {kz_term:api_ne_binary(), crossbar_view:options()}.
get_view_options([{<<"cdrs">>, []}, {?KZ_ACCOUNTS_DB, _}|_]) ->
    {?CB_LIST
    ,[{'mapper', fun cdrs_listing_mapper/3}
     ,{'range_start_keymap', []}
     ,{'range_end_keymap', crossbar_view:suffix_key_fun([kz_json:new()])}
     ,'include_docs'
     ]
    };
get_view_options([{<<"cdrs">>, []}, {<<"users">>, [OwnerId]}|_]) ->
    {?CB_LIST_BY_USER
    ,[{'range_start_keymap', [OwnerId]}
     ,{'range_end_keymap', fun(Ts) -> [OwnerId, Ts, kz_json:new()] end}
     ,{'mapper', fun cdrs_listing_mapper/3}
     ,'include_docs'
     ]
    };
get_view_options([{<<"cdrs">>, [?PATH_INTERACTION]}, {?KZ_ACCOUNTS_DB, _}|_]) ->
    {?CB_INTERACTION_LIST
    ,props:filter_undefined(
       [{'range_start_keymap', []}
       ,{'range_end_keymap', crossbar_view:suffix_key_fun([kz_json:new()])}
       ,{'group', 'true'}
       ,{'group_level', 2}
       ,{'reduce', 'true'}
       ,{'no_filter', 'true'}
        | maybe_add_stale_to_options(?STALE_CDR)
       ])
    };
get_view_options([{<<"cdrs">>, [?PATH_INTERACTION]}, {<<"users">>, [OwnerId]}|_]) ->
    {?CB_INTERACTION_LIST_BY_USER
    ,props:filter_undefined(
       [{'range_start_keymap', [OwnerId]}
       ,{'range_end_keymap', fun(Ts) -> [OwnerId, Ts, kz_json:new()] end}
       ,{'group', 'true'}
       ,{'group_level', 3}
       ,{'reduce', 'true'}
       ,{'no_filter', 'true'}
        | maybe_add_stale_to_options(?STALE_CDR)
       ])
    };
get_view_options(_) ->
    {'undefined', []}.

-spec maybe_add_stale_to_options(boolean()) -> crossbar_doc:view_options().
maybe_add_stale_to_options('true') -> [{'stale', 'ok'}];
maybe_add_stale_to_options('false') ->[].

-spec cdrs_listing_mapper(cb_context:context(), kz_json:object(), kz_json:objects()) -> kz_json:objects().
cdrs_listing_mapper(Context, JObj, Acc) ->
    [normalize_cdr(Context, <<"json">>, JObj) | Acc].

%%------------------------------------------------------------------------------
%% @doc Loads CDR docs from database and normalized the them.
%% @end
%%------------------------------------------------------------------------------
-spec load_chunked_cdrs(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
load_chunked_cdrs(Context, RespType) ->
    load_chunked_cdrs(Context, RespType, cb_context:resp_data(Context)).

-spec load_chunked_cdrs(cb_context:context(), kz_term:ne_binary(), kz_json:objects()) -> cb_context:context().
load_chunked_cdrs(Context, RespType, RespData) ->
    AccountId = cb_context:account_id(Context),
    Fun = fun(JObj, Acc) -> split_to_modbs(AccountId, kz_doc:id(JObj), Acc) end,
    MapIds = lists:foldl(Fun, #{}, RespData),

    C1 = cb_context:set_resp_data(Context, []),
    try maps:fold(fun(Db, Ids, C) -> load_chunked_cdr_ids(C, RespType, Db, Ids) end, C1, MapIds)
    catch
        _T:_E ->
            cb_context:add_system_error('datastore_fault', Context)
    end.

%% if request is not chunked, map Ids to MODBs
-spec split_to_modbs(kz_term:ne_binary(), kz_term:ne_binary(), map()) -> map().
split_to_modbs(AccountId, ?MATCH_MODB_PREFIX(Year, Month, _)=Id, Map) ->
    Db = kazoo_modb:get_modb(AccountId, Year, Month),
    maps:update_with(Db, fun(List) -> List ++ [Id] end, [Id], Map).

-spec load_chunked_cdr_ids(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binaries()) -> cb_context:context().
load_chunked_cdr_ids(Context, RespType, Db, Ids) ->
    load_chunked_cdr_ids(Context, RespType, Db, Ids, cb_context:resp_status(Context)).

load_chunked_cdr_ids(Context, RespType, Db, Ids, 'success') ->
    case kz_datamgr:open_docs(Db, Ids, [{'doc_type', kzd_cdrs:type()}]) of
        {'ok', Results} ->
            Resp0 = [normalize_cdr(Context, RespType, Result)
                     || Result <- Results,
                        %% Filter those docs which have accidentally put into this db.
                        %% See {@link cdr_channel_destroy} comment for function `prepare_and_save/3`
                        %% when it uses interaction_timestamp to generates modb_id like ID.
                        kz_json:get_value(<<"error">>, Result) =:= 'undefined',

                        %% if there are no filters, include doc
                        %% otherwise run filters against doc for inclusion
                        crossbar_filter:by_doc(kz_json:get_json_value(<<"doc">>, Result), Context, cb_context:fetch(Context, 'has_cdr_filter'))
                    ],
            RespAcc = cb_context:resp_data(Context),
            maybe_add_csv_header(Context, RespType, RespAcc ++ Resp0);
        {'error', Reason} ->
            lager:debug("failed to load cdrs doc from ~s: ~p", [Db, Reason]),
            crossbar_doc:handle_datamgr_errors(Reason, <<"load_cdrs">>, Context)
    end;
load_chunked_cdr_ids(Context, _RespType, _Db, _Ids, _Status) ->
    Context.

-spec normalize_cdr(cb_context:context(), kz_term:ne_binary(), kz_json:object()) -> kz_json:object() | kz_term:binary().
normalize_cdr(Context, <<"json">>, Result) ->
    JObj = kz_json:get_json_value(<<"doc">>, Result),
    Duration = kzd_cdrs:duration_seconds(JObj, 0),
    Timestamp = kzd_cdrs:timestamp(JObj, 0) - Duration,

    kz_json:from_list([{K, F(JObj, Timestamp, Context)} || {K, F} <- csv_rows(Context)]);
normalize_cdr(Context, <<"csv">>, Result) ->
    JObj = kz_json:get_json_value(<<"doc">>, Result),
    Duration = kzd_cdrs:duration_seconds(JObj, 0),
    Timestamp = kzd_cdrs:timestamp(JObj, 0) - Duration,

    <<(kz_binary:join([F(JObj, Timestamp, Context) || {_, F} <- csv_rows(Context)], <<",">>))/binary, "\r\n">>.

-spec maybe_add_csv_header(cb_context:context(), kz_term:ne_binary(), kz_json:objects() | kz_term:binaries()) -> cb_context:context().
maybe_add_csv_header(Context, _, []) ->
    Context;
maybe_add_csv_header(Context, <<"json">>, Data) ->
    cb_context:set_resp_data(Context, Data);
maybe_add_csv_header(Context, <<"csv">>, [Head | Tail]=Data) ->
    case cb_context:fetch(Context, 'chunking_started') of
        'true' ->
            cb_context:set_resp_data(Context, Data);
        'false' ->
            CSVHeader = kz_binary:join([K || {K, _Fun} <- csv_rows(Context)], <<",">>),
            cb_context:set_resp_data(Context, [<<CSVHeader/binary, "\r\n", Head/binary>> | Tail])
    end.

-spec csv_rows(cb_context:context()) -> [{kz_term:ne_binary(), csv_column_fun()}].
csv_rows(Context) ->
    case cb_context:fetch(Context, 'is_reseller', 'false') of
        'false' -> ?COLUMNS;
        'true' -> ?COLUMNS ++ ?COLUMNS_RESELLER
    end.

%% see csv_column_fun() for specs for each function here
col_id(JObj, _Timestamp, _Context) -> kz_doc:id(JObj, <<>>).
col_call_id(JObj, _Timestamp, _Context) -> kzd_cdrs:call_id(JObj, <<>>).
col_caller_id_number(JObj, _Timestamp, _Context) -> kzd_cdrs:caller_id_number(JObj, <<>>).
col_caller_id_name(JObj, _Timestamp, _Context) -> kzd_cdrs:caller_id_name(JObj, <<>>).
col_callee_id_number(JObj, _Timestamp, _Context) -> kzd_cdrs:callee_id_number(JObj, <<>>).
col_callee_id_name(JObj, _Timestamp, _Context) -> kzd_cdrs:callee_id_name(JObj, <<>>).
col_duration_seconds(JObj, _Timestamp, _Context) -> kzd_cdrs:duration_seconds(JObj, <<>>).
col_billing_seconds(JObj, _Timestamp, _Context) -> kzd_cdrs:billing_seconds(JObj, <<>>).
col_timestamp(_JObj, Timestamp, _Context) -> kz_term:to_binary(Timestamp).
col_hangup_cause(JObj, _Timestamp, _Context) -> kzd_cdrs:hangup_cause(JObj, <<>>).
col_other_leg_call_id(JObj, _Timestamp, _Context) -> kzd_cdrs:other_leg_call_id(JObj, <<>>).
col_owner_id(JObj, _Timestamp, _Context) -> kz_json:get_value([?KEY_CCV, <<"owner_id">>], JObj, <<>>).
col_to(JObj, _Timestamp, _Context) -> kzd_cdrs:to(JObj, <<>>).
col_from(JObj, _Timestamp, _Context) -> kzd_cdrs:from(JObj, <<>>).
col_call_direction(JObj, _Timestamp, _Context) -> kzd_cdrs:call_direction(JObj, <<>>).
col_request(JObj, _Timestamp, _Context) -> kzd_cdrs:request(JObj, <<>>).
col_authorizing_id(JObj, _Timestamp, _Context) ->
    case {kz_json:get_value([?KEY_CCV, <<"account_id">>], JObj, <<>>)
         ,kz_json:get_value([?KEY_CCV, <<"authorizing_id">>], JObj, <<>>)
         }
    of
        {A, A} -> <<>>;
        {_A, B} -> B
    end.
col_customer_cost(JObj, _Timestamp, _Context) -> kz_term:to_binary(customer_cost(JObj)).

col_dialed_number(JObj, _Timestamp, _Context) -> dialed_number(JObj).
col_calling_from(JObj, _Timestamp, _Context) -> calling_from(JObj).
col_pretty_print(_JObj, Timestamp, Context) ->
    UTCSecondsOffset = cb_context:req_value(Context, ?KEY_UTC_OFFSET),
    pretty_print_datetime(handle_utc_time_offset(Timestamp, UTCSecondsOffset)).
col_unix_timestamp(_JObj, Timestamp, _Context) -> kz_term:to_binary(kz_time:gregorian_seconds_to_unix_seconds(Timestamp)).
col_rfc1036(_JObj, Timestamp, _Context) -> kz_time:rfc1036(Timestamp).
col_iso8601(_JObj, Timestamp, _Context) -> kz_date:to_iso8601_extended(Timestamp).
col_iso8601_combined(_JObj, Timestamp, _Context) -> kz_time:iso8601(Timestamp).
col_account_call_type(JObj, _Timestamp, _Context) -> kz_json:get_value([?KEY_CCV, <<"account_billing">>], JObj, <<>>).
col_rate(JObj, _Timestamp, _Context) -> kz_term:to_binary(kz_currency:units_to_dollars(kz_json:get_value([?KEY_CCV, <<"rate">>], JObj, 0))).
col_rate_name(JObj, _Timestamp, _Context) -> kz_json:get_value([?KEY_CCV, <<"rate_name">>], JObj, <<>>).
col_bridge_id(JObj, _Timestamp, _Context) -> kz_json:get_value([?KEY_CCV, <<"bridge_id">>], JObj, <<>>).
col_recording_url(JObj, _Timestamp, _Context) -> kz_json:get_value([<<"recording_url">>], JObj, <<>>).
col_media_recordings(JObj, _Timestamp, _Context) -> format_recordings(JObj).
col_media_server(JObj, _Timestamp, _Context) -> kzd_cdrs:media_server(JObj, <<>>).
col_call_priority(JObj, _Timestamp, _Context) -> kz_json:get_value([?KEY_CCV, <<"call_priority">>], JObj, <<>>).

col_reseller_cost(JObj, _Timestamp, _Context) -> kz_term:to_binary(reseller_cost(JObj)).
col_reseller_call_type(JObj, _Timestamp, _Context) -> kz_json:get_value([?KEY_CCV, <<"reseller_billing">>], JObj, <<>>).

col_interaction_id(JObj, _Timestamp, _Context) -> kzd_cdrs:interaction_id(JObj, <<>>).

-spec pretty_print_datetime(kz_time:datetime() | kz_time:gregorian_second()) -> kz_term:ne_binary().
pretty_print_datetime(Timestamp) when is_integer(Timestamp) ->
    pretty_print_datetime(calendar:gregorian_seconds_to_datetime(Timestamp));
pretty_print_datetime({{Y,Mo,D},{H,Mi,S}}) ->
    iolist_to_binary(io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w"
                                  ,[Y, Mo, D, H, Mi, S]
                                  )).

-spec handle_utc_time_offset(kz_time:gregorian_seconds(), kz_term:api_integer()) -> kz_time:gregorian_seconds().
handle_utc_time_offset(Timestamp, 'undefined') -> Timestamp;
handle_utc_time_offset(Timestamp, UTCSecondsOffset) ->
    Timestamp + kz_term:to_number(UTCSecondsOffset).

-spec format_recordings(kz_json:object()) -> kz_term:binaries().
format_recordings(JObj) ->
    case kz_json:get_value([?KEY_CCV, <<"media_recordings">>], JObj, []) of
        Recordings when is_list(Recordings) -> Recordings;
        Recording -> [Recording]
    end.

-spec dialed_number(kzd_cdrs:doc()) -> binary().
dialed_number(JObj) ->
    case kzd_cdrs:call_direction(JObj) of
        <<"inbound">> ->
            [Num|_] = binary:split(kzd_cdrs:request(JObj, <<>>), <<"@">>),
            Num;
        <<"outbound">> ->
            [Num|_] = binary:split(kzd_cdrs:to(JObj, <<>>), <<"@">>),
            Num
    end.

-spec calling_from(kz_json:object()) -> binary().
calling_from(JObj) ->
    case kzd_cdrs:call_direction(JObj) of
        <<"inbound">> -> kzd_cdrs:caller_id_number(JObj, <<>>);
        <<"outbound">> ->
            [Num|_] = binary:split(kzd_cdrs:from_uri(JObj, <<>>), <<"@">>),
            Num
    end.

-spec customer_cost(kz_json:object()) -> pos_integer().
customer_cost(JObj) ->
    CCVs = kzd_cdrs:custom_channel_vars(JObj, kz_json:new()),
    case kz_json:get_ne_binary_value(<<"account_billing">>, CCVs) of
        <<"per_minute">> -> kapps_call_util:call_cost(JObj);
        _ -> 0
    end.

-spec reseller_cost(kz_json:object()) -> pos_integer().
reseller_cost(JObj) ->
    CCVs = kzd_cdrs:custom_channel_vars(JObj, kz_json:new()),

    case kz_json:get_ne_binary_value([<<"reseller_billing">>], CCVs) of
        <<"per_minute">> -> kapps_call_util:call_cost(JObj);
        _ -> 0
    end.

%%------------------------------------------------------------------------------
%% @doc Load a CDR document from the database
%% @end
%%------------------------------------------------------------------------------
-spec load_cdr(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
load_cdr(?MATCH_MODB_PREFIX(Year, Month, _Day) = CDRId, Context) ->
    AccountId = cb_context:account_id(Context),
    AccountDb = kazoo_modb:get_modb(AccountId, kz_term:to_integer(Year), kz_term:to_integer(Month)),
    Context1 = cb_context:set_account_db(Context, AccountDb),
    crossbar_doc:load({kzd_cdrs:type(), CDRId}, Context1, ?TYPE_CHECK_OPTION(kzd_cdrs:type()));
load_cdr(CDRId, Context) ->
    lager:debug("error loading cdr by id ~p", [CDRId]),
    crossbar_util:response('error', <<"could not find cdr with supplied id">>, 404, Context).

%%------------------------------------------------------------------------------
%% @doc Load Legs for a cdr interaction from the database
%% @end
%%------------------------------------------------------------------------------
-spec load_legs(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
load_legs(<<Year:4/binary, Month:2/binary, "-", _/binary>> = DocId, Context) ->
    AccountId = cb_context:account_id(Context),
    MODB = kazoo_modb:get_modb(AccountId, kz_term:to_integer(Year), kz_term:to_integer(Month)),
    case kz_datamgr:open_cache_doc(MODB, {kzd_cdrs:type(), DocId}) of
        {'ok', JObj} ->
            lager:debug("finding interaction id in ~s / ~s", [MODB, DocId]),
            load_legs(kzd_cdrs:interaction_id(JObj), Context);
        {'error', _} ->
            lager:debug("error loading legs for cdr id ~p", [DocId]),
            crossbar_util:response('error', <<"could not find legs for supplied id">>, 404, Context)
    end;
load_legs(<<BinTimestamp:11/binary, "-", _Key/binary>>=InteractionId, Context) ->
    MODB = kazoo_modb:get_modb(cb_context:account_id(Context), kz_term:to_integer(BinTimestamp)),

    lager:debug("finding legs for ~s / ~s", [MODB, InteractionId]),

    Options = [{'mapper', fun normalize_leg_view_results/2}
              ,{'range_start_keymap',  fun(_) -> [InteractionId] end}
              ,{'range_end_keymap', fun(_) -> [InteractionId, kz_json:new()] end}
              ,{'databases', [MODB]}
              ,'include_docs'
              ],
    crossbar_view:load_modb(Context, ?CB_INTERACTION_LIST_BY_ID, Options);
load_legs(Id, Context) ->
    crossbar_util:response_bad_identifier(Id, Context).

-spec normalize_leg_view_results(kz_json:object(), kz_json:objects()) ->
                                        kz_json:objects().
normalize_leg_view_results(JObj, Acc) ->
    Acc ++ [kz_json:get_json_value(<<"doc">>, JObj)].
