%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% Read only access to ACDC stats docs
%%%
%%% This code is VERY similar to that in cb_cdrs. At some point code
%%% that is used by both should be refactored into a MODB utility
%%% module.
%%%
%%% This was initially developed on 3.22, and since modified to reflect
%%% updates to cb_cdrs.
%%%
%%% @end
%%% @contributors
%%%   Sponsored by Raffel Internet B.V., implemented by Conversant Ltd
%%%-------------------------------------------------------------------
-module(cb_acdc_call_stats).

-export([init/0
        ,allowed_methods/0
        ,resource_exists/0
        ,content_types_provided/1
        ,validate/1
        ,to_json/1
        ,to_csv/1
        ]).
-export([pagination/1]).
-export([fetch_view_options/1]).
-export([get_stat_ids/3]).
-export([maybe_paginate_and_clean/2]).
-export([load_chunked_stats/3]).

-include("crossbar.hrl").

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".acdc_call_stats">>).
-define(MAX_BULK, kapps_config:get_integer(?MOD_CONFIG_CAT, <<"maximum_bulk">>, 50)).
-define(CB_LIST, <<"call_stats/crossbar_listing">>).

-define(COLUMNS
       ,[{<<"id">>, fun col_id/2}
        ,{<<"handled_timestamp">>, fun col_handled_timestamp/2}
        ,{<<"caller_id_number">>, fun col_caller_id_number/2}
        ,{<<"caller_id_name">>, fun col_caller_id_name/2}
        ,{<<"entered_position">>, fun col_entered_position/2}
        ,{<<"status">>, fun col_status/2}
        ,{<<"agent_id">>, fun col_agent_id/2}
        ,{<<"wait_time">>, fun col_wait_time/2}
        ,{<<"talk_time">>, fun col_talk_time/2}
        ,{<<"queue_id">>, fun col_queue_id/2}
        ]).

-type payload() :: {cowboy_req:req(), cb_context:context()}.
-export_type([payload/0]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec init() -> ok.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.acdc_call_stats">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.acdc_call_stats">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.acdc_call_stats">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.to_json.get.acdc_call_stats">>, ?MODULE, 'to_json'),
    _ = crossbar_bindings:bind(<<"*.to_csv.get.acdc_call_stats">>, ?MODULE, 'to_csv'),
    _ = crossbar_bindings:bind(<<"*.validate.acdc_call_stats">>, ?MODULE, 'validate'),
    ok.

-spec to_json(payload()) -> payload().
to_json({Req, Context}) ->
    Nouns = cb_context:req_nouns(Context),
    case props:get_value(<<"acdc_call_stats">>, Nouns, []) of
        [] -> to_json(Req, Context);
        [_|_] -> {Req, Context}
    end.

-spec to_json(cowboy_req:req(), cb_context:context()) -> payload().
to_json(Req0, Context) ->
    Headers = cowboy_req:get('resp_headers', Req0),
    {'ok', Req1} = cowboy_req:chunked_reply(200, Headers, Req0),
    'ok' = cowboy_req:chunk("{\"status\":\"success\", \"data\":[", Req1),
    {Req2, Context1} = send_chunked_stats({Req1, Context}),
    'ok' = cowboy_req:chunk("]", Req2),
    _ = pagination({Req2, Context1}),
    'ok' = cowboy_req:chunk([",\"request_id\":\"", cb_context:req_id(Context), "\""
                            ,",\"auth_token\":\"", cb_context:auth_token(Context), "\""
                            ,"}"
                            ]
                           ,Req2
                           ),
    {Req2, cb_context:store(Context1, 'is_chunked', 'true')}.

-spec pagination(payload()) -> payload().
pagination({Req, Context}=Payload) ->
    PageSize = cb_context:fetch(Context, 'page_size', 0),
    'ok' = cowboy_req:chunk(<<", \"page_size\": ", (kz_term:to_binary(PageSize))/binary>>, Req),
    case cb_context:fetch(Context, 'next_start_key') of
        'undefined' -> 'ok';
        [_, Next] -> cowboy_req:chunk(<<", \"next_start_key\": \"", (kz_term:to_binary(Next))/binary, "\"">>, Req);
        Next -> cowboy_req:chunk(<<", \"next_start_key\": \"", (kz_term:to_binary(Next))/binary, "\"">>, Req)
    end,
    StartKey = case cb_context:fetch(Context, 'start_key') of
                   [_, Key] -> Key;
                   Key -> Key
               end,
    'ok' = cowboy_req:chunk(<<", \"start_key\": \"", (kz_term:to_binary(StartKey))/binary, "\"">>, Req),
    Payload.

-spec to_csv(payload()) -> payload().
to_csv({Req, Context}) ->
    Nouns = cb_context:req_nouns(Context),
    case props:get_value(<<"acdc_call_stats">>, Nouns, []) of
        [] -> to_csv(Req, Context);
        [_|_] -> {Req, Context}
    end.

-spec to_csv(cowboy_req:req(), cb_context:context()) -> payload().
to_csv(Req, Context) ->
    Headers = props:set_values([{<<"content-type">>, <<"application/octet-stream">>}
                               ,{<<"content-disposition">>, <<"attachment; filename=\"acdc_call_stats.csv\"">>}
                               ]
                              ,cowboy_req:get('resp_headers', Req)
                              ),
    {'ok', Req1} = cowboy_req:chunked_reply(200, Headers, Req),
    Context1 = cb_context:store(Context, 'is_csv', 'true'),
    {Req2, _} = send_chunked_stats({Req1, Context1}),
    {Req2, cb_context:store(Context1,'is_chunked', 'true')}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/acdc_call_stats/' can only accept GET
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() ->
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
resource_exists() -> 'true'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add content types accepted and provided by this module
%%
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided(cb_context:context()) -> cb_context:context().
content_types_provided(Context) ->
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
validate(Context) ->
    load_stats_summary(Context, cb_context:req_nouns(Context)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of accounts, each summarized.  Or a specific
%% account summary.
%% @end
%%--------------------------------------------------------------------
-spec load_stats_summary(cb_context:context(), req_nouns()) -> cb_context:context().
load_stats_summary(Context, [_, {?KZ_ACCOUNTS_DB, [_]} | _]) ->
    lager:debug("loading call stats for account ~s", [cb_context:account_id(Context)]),
    case create_view_options('undefined', Context) of
        {'ok', ViewOptions} ->
            load_view(?CB_LIST
                     ,props:filter_undefined(ViewOptions)
                     ,remove_qs_keys(Context)
                     );
        Else -> Else
    end;
load_stats_summary(Context, _Nouns) ->
    lager:debug("invalid URL chain for stats summary request"),
    cb_context:add_system_error('faulty_request', Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-type view_option_fun() :: fun((api_binary(), cb_context:context(), gregorian_seconds(), gregorian_seconds()) -> {'ok', crossbar_doc:view_options()}).

-spec create_view_options(api_binary(), cb_context:context()) ->
                                 {'ok', crossbar_doc:view_options()} |
                                 cb_context:context().
-spec create_view_options(api_binary(), view_option_fun(), cb_context:context()) ->
                                 {'ok', crossbar_doc:view_options()} |
                                 cb_context:context().
create_view_options(OwnerId, Context) ->
    create_view_options(OwnerId, fun create_view_options/4, Context).

create_view_options(OwnerId, Fun, Context) ->
    MaxRange = kapps_config:get_integer(?MOD_CONFIG_CAT, <<"maximum_range">>, (?SECONDS_IN_DAY * 31  + ?SECONDS_IN_HOUR)),

    case cb_modules_util:range_view_options(Context, MaxRange) of
        {CreatedFrom, CreatedTo} ->
            Fun(OwnerId, Context, CreatedFrom, CreatedTo);
        Context1 -> Context1
    end.

-spec create_view_options(api_binary(), cb_context:context(), gregorian_seconds(), gregorian_seconds()) ->
                                 {'ok', crossbar_doc:view_options()}.
create_view_options('undefined', Context, CreatedFrom, CreatedTo) ->
    {'ok', [{'startkey', CreatedTo}
           ,{'endkey', CreatedFrom}
           ,{'limit', pagination_page_size(Context)}
           ,'descending'
           ]}.

-spec pagination_page_size(cb_context:context()) -> pos_integer().
pagination_page_size(Context) ->
    case crossbar_doc:pagination_page_size(Context) of
        'undefined' -> 'undefined';
        PageSize -> PageSize + 1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec load_view(ne_binary(), kz_proplist(), cb_context:context()) ->
                       cb_context:context().

load_view(View, ViewOptions, Context) ->
    ChunkedDbs = chunked_dbs(cb_context:account_id(Context), ViewOptions),
    ContextChanges =
        [{fun cb_context:store/3, 'chunked_dbs', ChunkedDbs}
        ,{fun cb_context:store/3, 'chunked_view_options', ViewOptions}
        ,{fun cb_context:store/3, 'chunked_view', View}
        ,{fun cb_context:set_resp_status/2, 'success'}
        ],
    cb_context:setters(Context, ContextChanges).

-spec chunked_dbs(ne_binary(), kz_proplist()) -> ne_binaries().
chunked_dbs(AccountId, ViewOptions) ->
    To = view_option('startkey',ViewOptions),
    From = view_option('endkey',  ViewOptions),
    kazoo_modb:get_range(AccountId, From, To).

-spec view_option('endkey' | 'startkey', crossbar_doc:view_options()) ->
                         gregorian_seconds().
view_option(Key, ViewOptions) ->
    case props:get_value(Key, ViewOptions) of
        [_, Option] -> Option;
        Option -> Option
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec send_chunked_stats(payload()) -> payload().
send_chunked_stats({Req, Context}) ->
    Dbs = cb_context:fetch(Context, 'chunked_dbs'),
    send_chunked_stats(Dbs, {Req, Context}).

-spec send_chunked_stats(ne_binaries(), payload()) -> payload().
send_chunked_stats([], Payload) -> Payload;
send_chunked_stats([Db | Dbs], {Req, Context}) ->
    View = cb_context:fetch(Context, 'chunked_view'),
    ViewOptions = fetch_view_options(Context),
    Context1 = cb_context:store(Context, 'start_key', props:get_value('startkey', ViewOptions)),
    Context2 = cb_context:store(Context1, 'page_size', 0),
    {'ok', Ids} = get_stat_ids(Db, View, ViewOptions),
    {Context3, StatIds} = maybe_paginate_and_clean(Context2, Ids),
    send_chunked_stats(Dbs, load_chunked_stats(Db, StatIds, {Req, Context3})).

-spec fetch_view_options(cb_context:context()) -> crossbar_doc:view_options().
fetch_view_options(Context) ->
    ViewOptions = cb_context:fetch(Context, 'chunked_view_options'),
    case cb_context:fetch(Context, 'is_csv') of
        'true' -> props:delete('limit', ViewOptions);
        _ -> ViewOptions
    end.

-spec maybe_paginate_and_clean(cb_context:context(), ne_binaries()) ->
                                      {cb_context:context(), ne_binaries()}.
maybe_paginate_and_clean(Context, []) -> {Context, []};
maybe_paginate_and_clean(Context, Ids) ->
    case cb_context:fetch(Context, 'is_csv') of
        'true' -> {Context, [Id || {Id, _} <- Ids]};
        _ -> paginate_and_clean(Context, Ids)
    end.

-spec paginate_and_clean(cb_context:context(), ne_binaries()) ->
                                {cb_context:context(), ne_binaries()}.
paginate_and_clean(Context, Ids) ->
    ViewOptions = cb_context:fetch(Context, 'chunked_view_options'),
    PageSize = erlang:length(Ids),
    AskedFor =
        case props:get_value('limit', ViewOptions) of
            'undefined' -> PageSize;
            Limit -> Limit - 1
        end,

    case AskedFor >= PageSize of
        'true' ->
            Context1 = cb_context:store(Context, 'page_size', AskedFor),
            {Context1, [Id || {Id, _} <- Ids]};
        'false' ->
            {_, LastKey}=Last = lists:last(Ids),
            Context1 = cb_context:store(Context, 'page_size', AskedFor),
            Context2 = cb_context:store(Context1, 'next_start_key', LastKey),
            {Context2, [Id || {Id, _} <- lists:delete(Last, Ids)]}
    end.

-spec get_stat_ids(ne_binary(), ne_binary(), kz_datamgr:view_options()) ->
                          {'ok', kz_proplist()}.
get_stat_ids(Db, View, ViewOptions) ->
    _ = maybe_add_design_doc(Db),
    case kz_datamgr:get_results(Db, View, ViewOptions) of
        {'error', _R} ->
            lager:debug("unable to fetch ~s from ~s: ~p", [View, Db, _R]),
            {'ok', []};
        {'ok', JObjs} ->
            lager:debug("fetched ~p stat ids from ~s", [length(JObjs), Db]),
            Stats = [{kz_doc:id(JObj), kz_json:get_value(<<"key">>, JObj)} || JObj <- JObjs],
            {'ok', Stats}
    end.

-spec maybe_add_design_doc(ne_binary()) -> 'ok' | {'error', 'not_found'}.
maybe_add_design_doc(Db) ->
    case kz_datamgr:lookup_doc_rev(Db, <<"_design/call_stats">>) of
        {'ok', _} -> 'ok';
        %% kz_datamgr:revise_views_from_folder doesn't load ACDC views
        {'error', 'not_found'} -> kz_datamgr:revise_views_from_folder(Db, 'acdc')
    end.

-spec load_chunked_stats(ne_binary(), ne_binaries(), payload()) -> payload().
load_chunked_stats(_, [], Payload) -> Payload;
load_chunked_stats(Db, Ids, {_, Context}=Payload) ->
    {BulkIds, Remaining} =
        case length(Ids) < ?MAX_BULK of
            'true' -> {Ids, []};
            'false' -> lists:split(?MAX_BULK, Ids)
        end,
    case kz_datamgr:open_cache_docs(Db, BulkIds, [{'doc_type', <<"call_stat">>}]) of
        {'ok', Results} ->
            HasQSFilter = crossbar_doc:has_qs_filter(Context),
            JObjs = [kz_json:get_value(<<"doc">>, Result)
                     || Result <- Results,
                        crossbar_doc:filtered_doc_by_qs(Result, HasQSFilter, Context)
                    ],
            P = normalize_and_send(JObjs, Payload),
            load_chunked_stats(Db, Remaining, P);
        {'error', _E} ->
            load_chunked_stats(Db, Remaining, Payload)
    end.

-spec normalize_and_send(kz_json:objects(), payload()) -> payload().
-spec normalize_and_send('json' | 'csv', kz_json:objects(), payload()) -> payload().
normalize_and_send(JObjs, {_, Context}=Payload) ->
    case cb_context:fetch(Context, 'is_csv') of
        'true' -> normalize_and_send('csv', JObjs, Payload);
        _ -> normalize_and_send('json', JObjs, Payload)
    end.

normalize_and_send('json', [], Payload) -> Payload;
normalize_and_send('json', [JObj|JObjs], {Req, Context}) ->
    Stat = normalize_stat(JObj, Context),
    case cb_context:fetch(Context, 'started_chunk') of
        'true' ->
            'ok' = cowboy_req:chunk(<<",", (kz_json:encode(Stat))/binary>>, Req),
            normalize_and_send('json', JObjs, {Req, Context});
        _Else ->
            'ok' = cowboy_req:chunk(kz_json:encode(Stat), Req),
            normalize_and_send('json', JObjs, {Req, cb_context:store(Context, 'started_chunk', 'true')})
    end;

normalize_and_send('csv', [], Payload) -> Payload;
normalize_and_send('csv', [JObj|JObjs], {Req, Context}) ->
    case cb_context:fetch(Context, 'started_chunk') of
        'true' ->
            'ok' = cowboy_req:chunk(normalize_stat_to_csv(JObj, Context), Req),
            normalize_and_send('csv', JObjs, {Req, Context});
        _Else ->
            CSV = <<(normalize_stat_to_csv_header(JObj, Context))/binary
                    ,(normalize_stat_to_csv(JObj, Context))/binary
                  >>,
            'ok' = cowboy_req:chunk(CSV, Req),
            normalize_and_send('csv', JObjs, {Req, cb_context:store(Context, 'started_chunk', 'true')})
    end.

-spec normalize_stat(kz_json:object(), cb_context:context()) -> kz_json:object().
normalize_stat(JObj, _Context) ->
    Duration = kz_json:get_integer_value(<<"duration_seconds">>, JObj, 0),
    Timestamp = kz_json:get_integer_value(<<"timestamp">>, JObj, 0) - Duration,
    kz_json:from_list(
      [{K, F(JObj, Timestamp)} || {K, F} <- ?COLUMNS]
     ).

-spec normalize_stat_to_csv(kz_json:object(), cb_context:context()) -> ne_binary().
normalize_stat_to_csv(JObj, _Context) ->
    Timestamp = kz_json:get_integer_value(<<"timestamp">>, JObj, 0),
    CSV = kz_binary:join(
            [F(JObj, Timestamp) || {_, F} <- ?COLUMNS]
                        ,<<",">>
           ),
    <<CSV/binary, "\r\n">>.

-spec normalize_stat_to_csv_header(kz_json:object(), cb_context:context()) -> ne_binary().
normalize_stat_to_csv_header(_JObj, _Context) ->
    CSV =
        kz_binary:join(
          [K || {K, _Fun} <- ?COLUMNS]
                      ,<<",">>
         ),

    <<CSV/binary, "\r\n">>.

col_id(JObj, _Timestamp) -> kz_doc:id(JObj, <<>>).
col_handled_timestamp(JObj, _Timestamp) -> kz_json:get_value(<<"handled_timestamp">>, JObj, <<>>).
col_caller_id_number(JObj, _Timestamp) -> kz_json:get_value(<<"caller_id_number">>, JObj, <<>>).
col_caller_id_name(JObj, _Timestamp) -> kz_json:get_value(<<"caller_id_name">>, JObj, <<>>).
col_entered_position(JObj, _Timestamp) -> kz_json:get_value(<<"entered_position">>, JObj, <<>>).
col_status(JObj, _Timestamp) -> kz_json:get_value(<<"status">>, JObj, <<>>).
col_agent_id(JObj, _Timestamp) -> kz_json:get_value(<<"agent_id">>, JObj, <<>>).
col_wait_time(JObj, _Timestamp) -> kz_json:get_value(<<"wait_time">>, JObj, <<>>).
col_talk_time(JObj, _Timestamp) -> kz_json:get_value(<<"talk_time">>, JObj, <<>>).
col_queue_id(JObj, _Timestamp) -> kz_json:get_value(<<"queue_id">>, JObj, <<>>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec remove_qs_keys(cb_context:context()) -> cb_context:context().
remove_qs_keys(Context) ->
    cb_context:set_query_string(Context
                               ,kz_json:delete_keys([<<"created_from">>
                                                    ,<<"created_to">>
                                                    ]
                                                   ,cb_context:query_string(Context)
                                                   )
                               ).
