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
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
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
-define(MOD_MAX_RANGE, kapps_config:get_pos_integer(?MOD_CONFIG_CAT, <<"maximum_range">>, ?MAX_RANGE)).
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
              ,{'max_range', ?MOD_MAX_RANGE}
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
              ,{'max_range', ?MOD_MAX_RANGE}
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
       ,{'range_end_keymap', [OwnerId]}
       ,{'key_min_length', 4}
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

-spec normalize_cdr(cb_context:context(), kz_term:ne_binary(), kz_json:object()) -> kz_json:object() | kz_term:ne_binary().
normalize_cdr(Context, <<"json">>, Result) ->
    JObj = kz_json:get_json_value(<<"doc">>, Result),
    Duration = kzd_cdrs:duration_seconds(JObj, 0),
    Timestamp = kzd_cdrs:timestamp(JObj, 0) - Duration,

    kz_json:from_list([{K, apply_row_mapper(K, F, JObj, Timestamp, Context)} || {K, F} <- csv_rows(Context)]);
normalize_cdr(Context, <<"csv">>, Result) ->
    JObj = kz_json:get_json_value(<<"doc">>, Result),
    Duration = kzd_cdrs:duration_seconds(JObj, 0),
    Timestamp = kzd_cdrs:timestamp(JObj, 0) - Duration,

    <<(kz_binary:join([apply_row_mapper(K, F, JObj, Timestamp, Context)
                       || {K, F} <- csv_rows(Context)
                      ]
                     ,<<",">>
                     ))/binary
     ,"\r\n"
    >>.

-spec apply_row_mapper(kz_term:ne_binary(), fun(), kz_json:object(), kz_time:gregorian_seconds(), cb_context:context()) -> binary().
apply_row_mapper(<<"datetime">>, _F, JObj, Timestamp, Context) ->
    col_pretty_print(JObj, Timestamp, Context);
apply_row_mapper(_, F, JObj, Timestamp, _Context) ->
    F(JObj, Timestamp, 'undefined').

-spec col_pretty_print(kz_json:object(), kz_time:gregorian_seconds(), cb_context:context()) -> kz_term:ne_binary().
col_pretty_print(_JObj, Timestamp, Context) ->
    UTCSecondsOffset = cb_context:req_value(Context, ?KEY_UTC_OFFSET),
    kz_time:pretty_print_datetime(handle_utc_time_offset(Timestamp, UTCSecondsOffset)).

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
            CSVHeader = kz_binary:join([K || {K, _Fun} <- csv_rows(Context)]
                                      ,<<",">>
                                      ),
            cb_context:set_resp_data(Context, [<<CSVHeader/binary, "\r\n", Head/binary>> | Tail])
    end.

csv_rows(Context) ->
    kzd_cdrs:csv_headers(cb_context:fetch(Context, 'is_reseller', 'false')).

-spec handle_utc_time_offset(kz_time:gregorian_seconds(), kz_term:api_integer()) -> kz_time:gregorian_seconds().
handle_utc_time_offset(Timestamp, 'undefined') -> Timestamp;
handle_utc_time_offset(Timestamp, UTCSecondsOffset) ->
    Timestamp + kz_term:to_number(UTCSecondsOffset).

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
              ,{'max_range', ?MOD_MAX_RANGE}
              ,'include_docs'
              ],
    crossbar_view:load_modb(Context, ?CB_INTERACTION_LIST_BY_ID, Options);
load_legs(Id, Context) ->
    crossbar_util:response_bad_identifier(Id, Context).

-spec normalize_leg_view_results(kz_json:object(), kz_json:objects()) ->
                                        kz_json:objects().
normalize_leg_view_results(JObj, Acc) ->
    Acc ++ [kz_json:get_json_value(<<"doc">>, JObj)].
