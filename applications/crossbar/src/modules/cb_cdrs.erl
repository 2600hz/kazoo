%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz INC
%%% @doc
%%%
%%% CDR
%%% Read only access to CDR docs
%%%
%%% @end
%%% @contributors
%%%   Edouard Swiac
%%%   James Aimonetti
%%%   Karl Anderson
%%%   Ben Wann
%%%   KAZOO-3596: Sponsored by GTNetwork LLC, implemented by SIPLABS LLC
%%%-------------------------------------------------------------------
-module(cb_cdrs).

-export([init/0
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2
        ,resource_exists/0, resource_exists/1, resource_exists/2
        ,content_types_provided/1, content_types_provided/2, content_types_provided/3
        ,validate/1, validate/2, validate/3
        ,to_json/1
        ,to_csv/1
        ]).

-export([load_chunked_cdr_ids/3]).

-include("crossbar.hrl").

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".cdrs">>).
-define(MAX_BULK, kapps_config:get_pos_integer(?MOD_CONFIG_CAT, <<"maximum_bulk">>, 50)).
-define(STALE_CDR, kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"cdr_stale_view">>, false)).

-define(CB_LIST, <<"cdrs/crossbar_listing">>).
-define(CB_LIST_BY_USER, <<"cdrs/listing_by_owner">>).
-define(CB_INTERACTION_LIST, <<"cdrs/interaction_listing">>).
-define(CB_INTERACTION_LIST_BY_USER, <<"cdrs/interaction_listing_by_owner">>).
-define(CB_INTERACTION_LIST_BY_ID, <<"cdrs/interaction_listing_by_id">>).
-define(CB_SUMMARY_VIEW, <<"cdrs/summarize_cdrs">>).
-define(CB_SUMMARY_LIST, <<"format_summary">>).

-define(PATH_INTERACTION, <<"interaction">>).
-define(PATH_LEGS, <<"legs">>).
-define(PATH_SUMMARY, <<"summary">>).

-define(KEY_CCV, <<"custom_channel_vars">>).

-define(COLUMNS
       ,[{<<"id">>, fun col_id/2}
        ,{<<"call_id">>, fun col_call_id/2}
        ,{<<"caller_id_number">>, fun col_caller_id_number/2}
        ,{<<"caller_id_name">>, fun col_caller_id_name/2}
        ,{<<"callee_id_number">>, fun col_callee_id_number/2}
        ,{<<"callee_id_name">>, fun col_callee_id_name/2}
        ,{<<"duration_seconds">>, fun col_duration_seconds/2}
        ,{<<"billing_seconds">>, fun col_billing_seconds/2}
        ,{<<"timestamp">>, fun col_timestamp/2}
        ,{<<"hangup_cause">>, fun col_hangup_cause/2}
        ,{<<"other_leg_call_id">>, fun col_other_leg_call_id/2}
        ,{<<"owner_id">>, fun col_owner_id/2}
        ,{<<"to">>, fun col_to/2}
        ,{<<"from">>, fun col_from/2}
        ,{<<"direction">>, fun col_call_direction/2}
        ,{<<"request">>, fun col_request/2}
        ,{<<"authorizing_id">>, fun col_authorizing_id/2}
        ,{<<"cost">>, fun col_customer_cost/2}
         %% New fields
        ,{<<"dialed_number">>, fun col_dialed_number/2}
        ,{<<"calling_from">>, fun col_calling_from/2}
        ,{<<"datetime">>, fun col_pretty_print/2}
        ,{<<"unix_timestamp">>, fun col_unix_timestamp/2}
        ,{<<"rfc_1036">>, fun col_rfc1036/2}
        ,{<<"iso_8601">>, fun col_iso8601/2}
        ,{<<"call_type">>, fun col_account_call_type/2}
        ,{<<"rate">>, fun col_rate/2}
        ,{<<"rate_name">>, fun col_rate_name/2}
        ,{<<"bridge_id">>, fun col_bridge_id/2}
        ,{<<"recording_url">>, fun col_recording_url/2}
        ,{<<"media_recordings">>, fun col_media_recordings/2}
        ,{<<"call_priority">>, fun col_call_priority/2}
        ]).

-define(COLUMNS_RESELLER
       ,[{<<"reseller_cost">>, fun col_reseller_cost/2}
        ,{<<"reseller_call_type">>, fun col_reseller_call_type/2}
        ]).

-type csv_column_fun() :: fun((kz_json:object(), gregorian_seconds()) -> ne_binary()).

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> ok.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.cdrs">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.cdrs">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.cdrs">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.to_json.get.cdrs">>, ?MODULE, 'to_json'),
    _ = crossbar_bindings:bind(<<"*.to_csv.get.cdrs">>, ?MODULE, 'to_csv'),
    _ = crossbar_bindings:bind(<<"*.validate.cdrs">>, ?MODULE, 'validate'),
    ok.

-spec to_json(cb_cowboy_payload()) -> cb_cowboy_payload().
to_json({Req, Context}) ->
    AuthAccountId = cb_context:auth_account_id(Context),
    IsReseller = kz_services:is_reseller(AuthAccountId),
    to_json(Req, cb_context:store(Context, 'is_reseller', IsReseller), get_view_options(cb_context:req_nouns(Context))).

-spec to_json(cowboy_req:req(), cb_context:context(), {api_ne_binary(), crossbar_view:options()}) -> cb_cowboy_payload().
to_json(Req, Context, {'undefined', _}) ->
    {Req, Context};
to_json(Req, Context, {ViewName, Options0}) ->
    Options = [{'is_chunked', 'true'}
              ,{'chunk_size', ?MAX_BULK}
              ,{'cowboy_req', Req}
              ,{'chunked_mapper', fun load_chunked_cdrs/3}
              ,{'chunk_response_type', 'json'}
               | Options0
              ],
    crossbar_view:load_modb(Context, ViewName, Options).

-spec to_csv(cb_cowboy_payload()) -> cb_cowboy_payload().
to_csv({Req, Context}) ->
    AuthAccountId = cb_context:auth_account_id(Context),
    IsReseller = kz_services:is_reseller(AuthAccountId),
    to_csv(Req, cb_context:store(Context, 'is_reseller', IsReseller), get_view_options(cb_context:req_nouns(Context))).

-spec to_csv(cowboy_req:req(), cb_context:context(), {api_ne_binary(), crossbar_view:options()}) -> cb_cowboy_payload().
to_csv(Req, Context, {'undefined', _}) ->
    lager:debug("invalid URL chain for cdrs request"),
    {Req, cb_context:add_system_error('faulty_request', Context)};
to_csv(Req, Context, {ViewName, Options0}) ->
    Options = [{'is_chunked', 'true'}
              ,{'chunk_size', ?MAX_BULK}
              ,{'cowboy_req', Req}
              ,{'chunked_mapper', fun load_chunked_cdrs/3}
              ,{'chunk_response_type', 'csv'}
               | Options0
              ],
    crossbar_view:load_modb(cb_context:store(Context, 'is_csv', 'true'), ViewName, Options).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/cdr/' can only accept GET
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET].
allowed_methods(?PATH_INTERACTION) ->
    [?HTTP_GET];
allowed_methods(?PATH_SUMMARY) ->
    [?HTTP_GET];
allowed_methods(_CDRId) ->
    [?HTTP_GET].
allowed_methods(?PATH_LEGS, _InteractionId) ->
    [?HTTP_GET].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> boolean().
-spec resource_exists(path_token()) -> boolean().
-spec resource_exists(path_token(), path_token()) -> boolean().
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.
resource_exists(?PATH_LEGS, _) -> 'true';
resource_exists(_, _) -> 'false'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add content types accepted and provided by this module
%%
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context) ->
    validate_path_and_date_range(Context, cb_context:req_nouns(Context)).

validate(Context, ?PATH_INTERACTION) ->
    validate_path_and_date_range(Context, cb_context:req_nouns(Context));
validate(Context, ?PATH_SUMMARY) ->
    load_cdr_summary(Context);
validate(Context, CDRId) ->
    load_cdr(CDRId, Context).

validate(Context, ?PATH_LEGS, InteractionId) ->
    load_legs(InteractionId, Context);
validate(Context, _, _) ->
    lager:debug("invalid URL chain for cdr request"),
    cb_context:add_system_error('faulty_request', Context).

-spec validate_path_and_date_range(cb_context:context(), req_nouns()) -> cb_context:context().
validate_path_and_date_range(Context, [{<<"cdrs">>, _}, {?KZ_ACCOUNTS_DB, _}|_]) ->
    validate_date_range(Context);
validate_path_and_date_range(Context, [{<<"cdrs">>, _}, {<<"users">>, [_]}|_]) ->
    validate_date_range(Context);
validate_path_and_date_range(Context, [{<<"cdrs">>, _}|_]) ->
    lager:debug("invalid URL chain for cdrs request"),
    cb_context:add_system_error('faulty_request', Context).

-spec validate_date_range(cb_context:context()) -> cb_context:context().
validate_date_range(Context) ->
    case crossbar_view:time_range(Context) of
        {_StartTime, _EndTime} -> cb_context:set_resp_status(Context, 'success');
        Ctx -> Ctx
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to CDRs summary.
%% @end
%%--------------------------------------------------------------------
-spec load_cdr_summary(cb_context:context()) -> cb_context:context().
load_cdr_summary(Context) ->
    lager:debug("loading cdr summary for account ~s", [cb_context:account_id(Context)]),
    Options = [{'mapper', fun normalize_summary_results/2}
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
                  end, JObj2, JObj1).

-spec normalize_summary_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_summary_results(JObj, Acc) -> [JObj|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generate specific view options for the path.
%% @end
%%--------------------------------------------------------------------
-spec get_view_options(req_nouns()) -> {api_ne_binary(), crossbar_view:options()}.
get_view_options([{<<"cdrs">>, []}, {?KZ_ACCOUNTS_DB, _}|_]) ->
    {?CB_LIST, []};
get_view_options([{<<"cdrs">>, []}, {<<"users">>, [OwnerId]}|_]) ->
    {?CB_LIST_BY_USER
    ,[{'range_start_keymap', [OwnerId]}
     ,{'range_end_keymap', [OwnerId]}
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
        | maybe_add_stale_to_options(?STALE_CDR)
       ])
    };
get_view_options(_) ->
    {'undefined', []}.

-spec maybe_add_stale_to_options(boolean()) -> crossbar_doc:view_options().
maybe_add_stale_to_options('true') -> [{'stale', 'ok'}];
maybe_add_stale_to_options('false') ->[].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Loads CDR docs from database and normalized the them.
%% @end
%%--------------------------------------------------------------------
-spec load_chunked_cdrs(cb_cowboy_payload(), kz_json:objects(), ne_binary()) -> crossbar_view:chunked_mapper_ret().
load_chunked_cdrs(Payload, JObjs, Db) ->
    Ids = [kz_doc:id(JObj) || JObj <- JObjs],
    load_chunked_cdr_ids(Payload, Ids, Db).

%% @public
-spec load_chunked_cdr_ids(cb_cowboy_payload(), ne_binaries(), ne_binary()) -> crossbar_view:chunked_mapper_ret().
load_chunked_cdr_ids({Req, Context}, Ids, Db) ->
    case kz_datamgr:open_docs(Db, Ids, [{'doc_type', <<"cdr">>}]) of
        {'ok', Results} ->
            JObjs = [kz_json:get_value(<<"doc">>, Result)
                     || Result <- Results,
                        crossbar_filter:by_doc(kz_json:get_value(<<"doc">>, Result), Context)
                    ],
            case cb_context:fetch(Context, 'is_csv', 'false') of
                'true' ->
                    {CSVs, Context1} = lists:foldl(fun normalize_cdr_to_csv/2, {[], Context}, JObjs),
                    {lists:reverse(CSVs), {Req, Context1}};
                'false' ->
                    {[normalize_cdr_to_jobj(JObj, Context) || JObj <- JObjs], {Req, Context}}
            end;
        {'error', _Reason} ->
            lager:debug("failed to load cdrs doc from ~s: ~p", [Db, _Reason]),
            {'stop', {Req, Context}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalize CDR in JSON
%% @end
%%--------------------------------------------------------------------
-spec normalize_cdr_to_jobj(kz_json:object(), cb_context:context()) -> kz_json:object().
normalize_cdr_to_jobj(JObj, Context) ->
    Duration = kz_json:get_integer_value(<<"duration_seconds">>, JObj, 0),
    Timestamp = kz_json:get_integer_value(<<"timestamp">>, JObj, 0) - Duration,
    kz_json:from_list([{K, F(JObj, Timestamp)} || {K, F} <- csv_rows(Context)]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalize CDR in CSV
%% @end
%%--------------------------------------------------------------------
-spec normalize_cdr_to_csv(kz_json:object(), {binaries(), cb_context:context()}) ->
                                  {binaries(), cb_context:context()}.
normalize_cdr_to_csv(JObj, {CSVs, Context}) ->
    Timestamp = kz_json:get_integer_value(<<"timestamp">>, JObj, 0),
    CSV = kz_binary:join([F(JObj, Timestamp) || {_, F} <- csv_rows(Context)], <<",">>),
    case cb_context:fetch(Context, 'started_chunk') of
        'true' ->
            {[<<CSV/binary, "\r\n">>|CSVs], Context};
        'false' ->
            CSVHeader = kz_binary:join([K || {K, _Fun} <- csv_rows(Context)], <<",">>),
            {[<<CSVHeader/binary, "\r\n", CSV/binary, "\r\n">>|CSVs], Context}

    end.

-spec csv_rows(cb_context:context()) -> [{ne_binary(), csv_column_fun()}].
csv_rows(Context) ->
    AuthAccountId = cb_context:auth_account_id(Context),
    case kz_services:is_reseller(AuthAccountId) of
        'false' -> ?COLUMNS;
        'true' -> ?COLUMNS ++ ?COLUMNS_RESELLER
    end.

%% see csv_column_fun() for specs for each function here
col_id(JObj, _Timestamp) -> kz_doc:id(JObj, <<>>).
col_call_id(JObj, _Timestamp) -> kz_json:get_value(<<"call_id">>, JObj, <<>>).
col_caller_id_number(JObj, _Timestamp) -> kz_json:get_value(<<"caller_id_number">>, JObj, <<>>).
col_caller_id_name(JObj, _Timestamp) -> kz_json:get_value(<<"caller_id_name">>, JObj, <<>>).
col_callee_id_number(JObj, _Timestamp) -> kz_json:get_value(<<"callee_id_number">>, JObj, <<>>).
col_callee_id_name(JObj, _Timestamp) -> kz_json:get_value(<<"callee_id_name">>, JObj, <<>>).
col_duration_seconds(JObj, _Timestamp) -> kz_json:get_value(<<"duration_seconds">>, JObj, <<>>).
col_billing_seconds(JObj, _Timestamp) -> kz_json:get_value(<<"billing_seconds">>, JObj, <<>>).
col_timestamp(_JObj, Timestamp) -> kz_term:to_binary(Timestamp).
col_hangup_cause(JObj, _Timestamp) -> kz_json:get_value(<<"hangup_cause">>, JObj, <<>>).
col_other_leg_call_id(JObj, _Timestamp) -> kz_json:get_value(<<"other_leg_call_id">>, JObj, <<>>).
col_owner_id(JObj, _Timestamp) -> kz_json:get_value([?KEY_CCV, <<"owner_id">>], JObj, <<>>).
col_to(JObj, _Timestamp) -> kz_json:get_value(<<"to">>, JObj, <<>>).
col_from(JObj, _Timestamp) -> kz_json:get_value(<<"from">>, JObj, <<>>).
col_call_direction(JObj, _Timestamp) -> kz_json:get_value(<<"call_direction">>, JObj, <<>>).
col_request(JObj, _Timestamp) -> kz_json:get_value(<<"request">>, JObj, <<>>).
col_authorizing_id(JObj, _Timestamp) ->
    case {kz_json:get_value([?KEY_CCV, <<"account_id">>], JObj, <<>>)
         ,kz_json:get_value([?KEY_CCV, <<"authorizing_id">>], JObj, <<>>)
         }
    of
        {A, A} -> <<>>;
        {_A, B} -> B
    end.
col_customer_cost(JObj, _Timestamp) -> kz_term:to_binary(customer_cost(JObj)).

col_dialed_number(JObj, _Timestamp) -> dialed_number(JObj).
col_calling_from(JObj, _Timestamp) -> calling_from(JObj).
col_pretty_print(_JObj, Timestamp) -> pretty_print_datetime(Timestamp).
col_unix_timestamp(_JObj, Timestamp) -> kz_term:to_binary(kz_time:gregorian_seconds_to_unix_seconds(Timestamp)).
col_rfc1036(_JObj, Timestamp) -> list_to_binary([$", kz_time:rfc1036(Timestamp), $"]).
col_iso8601(_JObj, Timestamp) -> list_to_binary([$", kz_date:to_iso8601_extended(Timestamp), $"]).
col_account_call_type(JObj, _Timestamp) -> kz_json:get_value([?KEY_CCV, <<"account_billing">>], JObj, <<>>).
col_rate(JObj, _Timestamp) -> kz_term:to_binary(wht_util:units_to_dollars(kz_json:get_value([?KEY_CCV, <<"rate">>], JObj, 0))).
col_rate_name(JObj, _Timestamp) -> kz_json:get_value([?KEY_CCV, <<"rate_name">>], JObj, <<>>).
col_bridge_id(JObj, _Timestamp) -> kz_json:get_value([?KEY_CCV, <<"bridge_id">>], JObj, <<>>).
col_recording_url(JObj, _Timestamp) -> kz_json:get_value([<<"recording_url">>], JObj, <<>>).
col_media_recordings(JObj, _Timestamp) -> format_recordings(JObj).
col_call_priority(JObj, _Timestamp) -> kz_json:get_value([?KEY_CCV, <<"call_priority">>], JObj, <<>>).

col_reseller_cost(JObj, _Timestamp) -> kz_term:to_binary(reseller_cost(JObj)).
col_reseller_call_type(JObj, _Timestamp) -> kz_json:get_value([?KEY_CCV, <<"reseller_billing">>], JObj, <<>>).

-spec pretty_print_datetime(kz_datetime() | integer()) -> ne_binary().
pretty_print_datetime(Timestamp) when is_integer(Timestamp) ->
    pretty_print_datetime(calendar:gregorian_seconds_to_datetime(Timestamp));
pretty_print_datetime({{Y,Mo,D},{H,Mi,S}}) ->
    iolist_to_binary(io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w"
                                  ,[Y, Mo, D, H, Mi, S]
                                  )).

-spec format_recordings(kz_json:object()) -> binaries().
format_recordings(JObj) ->
    case kz_json:get_value([?KEY_CCV, <<"media_recordings">>], JObj, []) of
        Recordings when is_list(Recordings) -> Recordings;
        Recording -> [Recording]
    end.

-spec dialed_number(kz_json:object()) -> binary().
dialed_number(JObj) ->
    case kz_json:get_value(<<"call_direction">>, JObj) of
        <<"inbound">> ->
            [Num|_] = binary:split(kz_json:get_value(<<"request">>, JObj, <<>>), <<"@">>),
            Num;
        <<"outbound">> ->
            [Num|_] = binary:split(kz_json:get_value(<<"to">>, JObj, <<>>), <<"@">>),
            Num
    end.

-spec calling_from(kz_json:object()) -> binary().
calling_from(JObj) ->
    case kz_json:get_value(<<"call_direction">>, JObj) of
        <<"inbound">> -> kz_json:get_value(<<"caller_id_number">>, JObj, <<>>);
        <<"outbound">> ->
            [Num|_] = binary:split(kz_json:get_value(<<"from_uri">>, JObj, <<>>), <<"@">>),
            Num
    end.

-spec customer_cost(kz_json:object()) -> pos_integer().
customer_cost(JObj) ->
    case kz_json:get_value([?KEY_CCV, <<"account_billing">>], JObj) of
        <<"per_minute">> -> wht_util:call_cost(JObj);
        _ -> 0
    end.

-spec reseller_cost(kz_json:object()) -> pos_integer().
reseller_cost(JObj) ->
    case kz_json:get_value([?KEY_CCV, <<"reseller_billing">>], JObj) of
        <<"per_minute">> -> wht_util:call_cost(JObj);
        _ -> 0
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a CDR document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_cdr(ne_binary(), cb_context:context()) -> cb_context:context().
load_cdr(?MATCH_MODB_PREFIX(Year,Month,_) = CDRId, Context) ->
    AccountId = cb_context:account_id(Context),
    AccountDb = kazoo_modb:get_modb(AccountId, kz_term:to_integer(Year), kz_term:to_integer(Month)),
    Context1 = cb_context:set_account_db(Context, AccountDb),
    crossbar_doc:load({<<"cdr">>, CDRId}, Context1, ?TYPE_CHECK_OPTION(<<"cdr">>));
load_cdr(CDRId, Context) ->
    lager:debug("error loading cdr by id ~p", [CDRId]),
    crossbar_util:response('error', <<"could not find cdr with supplied id">>, 404, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load Legs for a cdr interaction from the database
%% @end
%%--------------------------------------------------------------------
-spec load_legs(ne_binary(), cb_context:context()) -> cb_context:context().
load_legs(<<Year:4/binary, Month:2/binary, "-", _/binary>> = DocId, Context) ->
    AccountId = cb_context:account_id(Context),
    AccountDb = kazoo_modb:get_modb(AccountId, kz_term:to_integer(Year), kz_term:to_integer(Month)),
    Context1 = cb_context:set_account_db(Context, AccountDb),
    case kz_datamgr:open_doc(AccountDb, {<<"cdr">>, DocId}) of
        {'ok', JObj} ->
            load_legs(kz_json:get_value(<<"interaction_id">>, JObj), Context1);
        _ ->
            lager:debug("error loading legs for cdr id ~p", [DocId]),
            crossbar_util:response('error', <<"could not find legs for supplied id">>, 404, Context1)
    end;
load_legs(InteractionId, Context) ->
    Options = ['include_docs'
              ,{'startkey', [InteractionId]}
              ,{'endkey', [InteractionId, kz_json:new()]}
              ],
    crossbar_doc:load_view(?CB_INTERACTION_LIST_BY_ID
                          ,Options
                          ,Context
                          ,fun normalize_leg_view_results/2
                          ).

-spec normalize_leg_view_results(kz_json:object(), kz_json:objects()) ->
                                        kz_json:objects().
normalize_leg_view_results(JObj, Acc) ->
    Acc ++ [kz_json:get_value(<<"doc">>, JObj)].
