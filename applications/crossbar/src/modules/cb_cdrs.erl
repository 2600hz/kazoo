%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
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
%%%-------------------------------------------------------------------
-module(cb_cdrs).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,content_types_provided/1
         ,validate/1, validate/2
         ,to_json/1
         ,to_csv/1
        ]).

-include("../crossbar.hrl").

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".cdrs">>).
-define(MAX_BULK, whapps_config:get_integer(?MOD_CONFIG_CAT, <<"maximum_bulk">>, 50)).
-define(CB_LIST_BY_USER, <<"cdrs/listing_by_owner">>).
-define(CB_LIST, <<"cdrs/crossbar_listing">>).

-type payload() :: {cowboy_req:req(), cb_context:context()}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.cdrs">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.cdrs">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.cdrs">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.to_json.get.cdrs">>, ?MODULE, 'to_json'),
    _ = crossbar_bindings:bind(<<"*.to_csv.get.cdrs">>, ?MODULE, 'to_csv'),
    _ = crossbar_bindings:bind(<<"*.validate.cdrs">>, ?MODULE, 'validate').


-spec to_json(payload()) -> payload().
to_json({Req1, Context}) ->
    Nouns = cb_context:req_nouns(Context),
    case props:get_value(<<"cdrs">>, Nouns, []) of
        [_|_] -> {Req1, Context};
        [] ->
            Headers = cowboy_req:get('resp_headers', Req1),
            {'ok', Req2} = cowboy_req:chunked_reply(200, Headers, Req1),
            'ok' = cowboy_req:chunk("{\"status\":\"success\", \"data\":[", Req2),
            {Req3, Context1} = send_chunked_cdrs({Req2, Context}),
            'ok' = cowboy_req:chunk("]", Req3),
            _ = pagination({Req3, Context1}),
            'ok' = cowboy_req:chunk("}", Req3),
            'ok' = cowboy_req:ensure_response(Req3, 200),
            {Req3, cb_context:store(Context1, 'is_chunked', 'true')}
    end.

-spec pagination(payload()) -> payload().
pagination({Req, Context}=Payload) ->
    PageSize = cb_context:fetch(Context, 'page_size'),
    StartKey = cb_context:fetch(Context, 'start_key'),
    'ok' = cowboy_req:chunk(<<", \"page_size\": ", (wh_util:to_binary(PageSize))/binary>>, Req),
    case cb_context:fetch(Context, 'next_start_key') of
        'undefined' -> 'ok';
        Next -> cowboy_req:chunk(<<", \"next_start_key\": \"", (wh_util:to_binary(Next))/binary, "\"">>, Req)
    end,
    'ok' = cowboy_req:chunk(<<", \"start_key\": \"", (wh_util:to_binary(StartKey))/binary, "\"">>, Req),
    Payload.

-spec to_csv(payload()) -> payload().
to_csv({Req, Context}) ->
    Nouns = cb_context:req_nouns(Context),
    case props:get_value(<<"cdrs">>, Nouns, []) of
        [_|_] -> {Req, Context};
        [] ->
            Headers = props:set_values([{<<"content-type">>, <<"application/octet-stream">>}]
                                       ,cowboy_req:get('resp_headers', Req)),
            {'ok', Req1} = cowboy_req:chunked_reply(200, Headers, Req),
            Context1 = cb_context:store(Context, 'is_csv', 'true'),
            {Req2, _} = send_chunked_cdrs({Req1, Context1}),
            'ok' = cowboy_req:ensure_response(Req2, 200),
            {Req2, cb_context:store(Context1,'is_chunked', 'true')}
    end.

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
allowed_methods() ->
    [?HTTP_GET].
allowed_methods(_) ->
    [?HTTP_GET].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add content types accepted and provided by this module
%%
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided(cb_context:context()) -> cb_context:context().
content_types_provided(Context) ->
    CTPs = [{'to_json', ?JSON_CONTENT_TYPES}
            ,{'to_csv', ?CSV_CONTENT_TYPES}
           ],
    cb_context:add_content_types_provided(Context, CTPs).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context) ->
    load_cdr_summary(Context, cb_context:req_nouns(Context)).
validate(Context, CDRId) ->
    load_cdr(CDRId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of accounts, each summarized.  Or a specific
%% account summary.
%% @end
%%--------------------------------------------------------------------
-spec load_cdr_summary(cb_context:context(), req_nouns()) -> cb_context:context().
load_cdr_summary(Context, [_, {?WH_ACCOUNTS_DB, [_]} | _]) ->
    lager:debug("loading cdrs for account ~s", [cb_context:account_id(Context)]),
    case create_view_options('undefined', Context) of
        {'ok', ViewOptions} ->
            load_view(?CB_LIST
                      ,ViewOptions
                      ,remove_qs_keys(Context)
                     );
        Else -> Else
    end;
load_cdr_summary(Context, [_, {<<"users">>, [UserId] } | _]) ->
    lager:debug("loading cdrs for user ~s", [UserId]),
    case create_view_options(UserId, Context) of
        {'ok', ViewOptions} ->
            load_view(?CB_LIST_BY_USER
                      ,ViewOptions
                      ,remove_qs_keys(Context)
                     );
        Else -> Else
    end;
load_cdr_summary(Context, _Nouns) ->
    lager:debug("invalid URL chain for cdr summary request"),
    cb_context:add_system_error('faulty_request', Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create_view_options(api_binary(), cb_context:context()) ->
                                 {'ok', wh_proplist()} |
                                 cb_context:context().
create_view_options(OwnerId, Context) ->
    TStamp =  wh_util:current_tstamp(),
    MaxRange = whapps_config:get_integer(?MOD_CONFIG_CAT, <<"maximum_range">>, (?SECONDS_IN_DAY * 31)),
    CreatedFrom = created_from(Context, TStamp, MaxRange),
    CreatedTo = created_to(Context, CreatedFrom, MaxRange),
    Limit = crossbar_doc:pagination_page_size(Context) + 1,
    case CreatedTo - CreatedFrom of
        N when N < 0 ->
            Message = <<"created_from is prior to created_to">>,
            cb_context:add_validation_error(<<"created_from">>
                                            ,<<"date_range">>
                                            ,Message
                                            ,Context
                                           );
        N when N > MaxRange ->
            Message = <<"created_to is more than "
                        ,(wh_util:to_binary(MaxRange))/binary
                        ," seconds from created_from"
                      >>,
            cb_context:add_validation_error(<<"created_from">>
                                            ,<<"date_range">>
                                            ,Message
                                            ,Context
                                           );
        _N when OwnerId =:= 'undefined' ->
            {'ok', [{'startkey', CreatedFrom}
                    ,{'endkey', CreatedTo}
                    ,{'limit', Limit}
                   ]};
        _N ->
            {'ok', [{'startkey', [OwnerId, CreatedFrom]}
                    ,{'endkey', [OwnerId, CreatedTo]}
                    ,{'limit', Limit}
                   ]}
    end.

-spec created_to(cb_context:context(), pos_integer(), pos_integer()) -> pos_integer().
created_to(Context, CreatedFrom, MaxRange) ->
    wh_util:to_integer(cb_context:req_value(Context, <<"created_to">>, CreatedFrom + MaxRange)).

-spec created_from(cb_context:context(), pos_integer(), pos_integer()) -> pos_integer().
created_from(Context, TStamp, MaxRange) ->
    created_from(Context, TStamp, MaxRange, crossbar_doc:start_key(Context)).

-spec created_from(cb_context:context(), pos_integer(), pos_integer(), api_binary()) -> pos_integer().
created_from(Context, TStamp, MaxRange, 'undefined') ->
    lager:debug("building created_from from req value"),
    wh_util:to_integer(cb_context:req_value(Context, <<"created_from">>, TStamp - MaxRange));
created_from(_Context, _TStamp, _MaxRange, StartKey) ->
    lager:debug("found startkey ~p as created_from", [StartKey]),
    wh_util:to_integer(StartKey).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec load_view(ne_binary(), wh_proplist(), cb_context:context()) -> cb_context:context().
load_view(View, ViewOptions, Context) ->
    AccountId = cb_context:account_id(Context),
    ToMODb = view_created_to_modb(AccountId, ViewOptions),
    FromMODb = view_created_from_modb(AccountId, ViewOptions),
    load_chunked_db(View, ViewOptions, ToMODb, FromMODb, Context).

-spec load_chunked_db(ne_binary(), wh_proplist(), ne_binary(), ne_binary(), cb_context:context()) -> cb_context:context().
load_chunked_db(View, ViewOptions, Db, Db, Context) ->
    C = cb_context:set_account_db(Context, [Db]),
    load_chunked_view_options(View, ViewOptions, C);
load_chunked_db(View, ViewOptions, ToMODb, FromMODb, Context) ->
    C = cb_context:set_account_db(Context, [ToMODb, FromMODb]),
    load_chunked_view_options(View, ViewOptions, C).

-spec load_chunked_view_options(ne_binary(), wh_proplist(), cb_context:context()) -> cb_context:context().
load_chunked_view_options(View, ViewOptions, Context) ->
    C = cb_context:store(Context, 'chunked_view_options', ViewOptions),
    load_chunked_view(View, C).

-spec load_chunked_view(ne_binary(), cb_context:context()) -> cb_context:context().
load_chunked_view(View, Context) ->
    C = cb_context:store(Context, 'chunked_view', View),
    cb_context:set_resp_status(C, 'success').

-spec view_created_to_modb(ne_binary(), wh_proplist()) -> ne_binary().
view_created_to_modb(AccountId, ViewOptions) ->
    kazoo_modb:get_modb(AccountId, view_key_created_to(ViewOptions)).

-spec view_key_created_to(wh_proplist()) -> pos_integer().
view_key_created_to(ViewOptions) ->
    case props:get_value('endkey', ViewOptions) of
        [_, CreatedTo] -> CreatedTo;
        CreatedTo -> CreatedTo
    end.

-spec view_created_from_modb(ne_binary(), wh_proplist()) -> ne_binary().
view_created_from_modb(AccountId, ViewOptions) ->
    kazoo_modb:get_modb(AccountId, view_key_created_from(ViewOptions)).

-spec view_key_created_from(wh_proplist()) -> pos_integer().
view_key_created_from(ViewOptions) ->
    case props:get_value('startkey', ViewOptions) of
        [_, CreatedFrom] -> CreatedFrom;
        CreatedFrom -> CreatedFrom
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec send_chunked_cdrs(payload()) -> payload().
send_chunked_cdrs({Req, Context}) ->
    Db = cb_context:account_db(Context),
    AuthAccountId = cb_context:auth_account_id(Context),
    IsReseller = wh_services:is_reseller(AuthAccountId),
    send_chunked_cdrs(Db, {Req, cb_context:store(Context, 'is_reseller', IsReseller)}).

-spec send_chunked_cdrs(ne_binaries(), payload()) -> payload().
send_chunked_cdrs([], Payload) -> Payload;
send_chunked_cdrs([Db | Dbs], {Req, Context}=Payload) ->
    View = cb_context:fetch(Context, 'chunked_view'),
    ViewOptions = fetch_view_options(Context),
    Context1 = cb_context:store(Context, 'start_key', props:get_value('startkey', ViewOptions)),
    Context2 = cb_context:store(Context1, 'page_size', 0),
    P = case get_cdr_ids(Db, View, ViewOptions) of
            {'ok', Ids} ->
                {Context3, CDRIds} = maybe_paginate_and_clean(Context2, Ids),
                load_chunked_cdrs(Db, CDRIds, {Req, Context3});
            {'error', _} -> Payload
        end,
    send_chunked_cdrs(Dbs, P).

-spec fetch_view_options(cb_context:context()) -> wh_proplist().
fetch_view_options(Context) ->
    ViewOptions = cb_context:fetch(Context, 'chunked_view_options'),
    case cb_context:fetch(Context, 'is_csv') of
        'true' -> props:delete('limit', ViewOptions);
        _ -> ViewOptions
    end.

-spec maybe_paginate_and_clean(cb_context:context(), ne_binaries()) -> {cb_context:context(), ne_binaries()}.
maybe_paginate_and_clean(Context, []) -> {Context, []};
maybe_paginate_and_clean(Context, Ids) ->
    case cb_context:fetch(Context, 'is_csv') of
        'true' ->
            {Context, [Id || {Id, _} <- Ids]};
        _ ->
            ViewOptions = cb_context:fetch(Context, 'chunked_view_options'),
            AskedFor = props:get_value('limit', ViewOptions)-1,
            PageSize = erlang:length(Ids)-1,
            case AskedFor > erlang:length(Ids) of
                'true' ->
                    Context1 = cb_context:store(Context, 'page_size', PageSize),
                    {Context1, [Id || {Id, _} <- Ids]};
                'false' ->
                    {_, LastKey}=Last = lists:last(Ids),
                    Context1 = cb_context:store(Context, 'page_size', PageSize),
                    Context2 = cb_context:store(Context1, 'next_start_key', LastKey),
                    {Context2, [Id || {Id, _} <- lists:delete(Last, Ids)]}
            end
    end.

-spec get_cdr_ids(ne_binary(), ne_binary(), wh_proplist()) ->
                                {'ok', wh_proplist()} | {'error', _}.
get_cdr_ids(Db, View, ViewOptions) ->
    _ = maybe_add_design_doc(Db),
    case couch_mgr:get_results(Db, View, ViewOptions) of
        {'error', _R}=E -> E;
        {'ok', JObjs} ->
            {'ok', [{wh_json:get_value(<<"id">>, JObj)
                     ,wh_json:get_value(<<"key">>, JObj)
                    }
                    || JObj <- JObjs]}
    end.

-spec maybe_add_design_doc(ne_binary()) -> 'ok' | {'error', 'not_found'}.
maybe_add_design_doc(Db) ->
    case couch_mgr:lookup_doc_rev(Db, <<"_design/cdrs">>) of
        {'error', 'not_found'} ->
            lager:warning("adding cdr views to Db ~s", [Db]),
            couch_mgr:revise_doc_from_file(Db
                                           ,'crossbar'
                                           ,<<"account/cdrs.json">>
                                          );
        {'ok', _ } -> 'ok'
    end.

-spec load_chunked_cdrs(ne_binary(), ne_binaries(), payload()) -> payload().
load_chunked_cdrs(_, [], Payload) -> Payload;
load_chunked_cdrs(Db, Ids, {_, Context}=Payload) ->
    {BulkIds, Remaining} =
        case erlang:length(Ids) < ?MAX_BULK of
            'true' -> {Ids, []};
            'false' -> lists:split(?MAX_BULK, Ids)
        end,
   ViewOptions = [{'keys', BulkIds}
                  ,'include_docs'
                 ],
    case couch_mgr:all_docs(Db, ViewOptions) of
        {'ok', Results} ->
            HasQSFilter = crossbar_doc:has_qs_filter(Context),
            JObjs = [wh_json:get_value(<<"doc">>, Result)
                     || Result <- Results
                        ,crossbar_doc:filtered_doc_by_qs(Result, HasQSFilter, Context)],
            P = normalize_and_send(JObjs, Payload),
            load_chunked_cdrs(Db, Remaining, P);
        {'error', _E} ->
            load_chunked_cdrs(Db, Remaining, Payload)
    end.

-spec normalize_and_send(wh_json:objects(), payload()) -> payload().
-spec normalize_and_send('json' | 'csv', wh_json:objects(), payload()) -> payload().
normalize_and_send(JObjs, {_, Context}=Payload) ->
    case cb_context:fetch(Context, 'is_csv') of
        'true' -> normalize_and_send('csv', JObjs, Payload);
        _ -> normalize_and_send('json', JObjs, Payload)
    end.

normalize_and_send('json', [], Payload) -> Payload;
normalize_and_send('json', [JObj|JObjs], {Req, Context}) ->
    CDR = normalize_cdr(JObj, Context),
    case cb_context:fetch(Context, 'started_chunk') of
        'true' ->
            'ok' = cowboy_req:chunk(<<",", (wh_json:encode(CDR))/binary>>, Req),
            normalize_and_send('json', JObjs, {Req, Context});
        _Else ->
            'ok' = cowboy_req:chunk(wh_json:encode(CDR), Req),
            normalize_and_send('json', JObjs, {Req, cb_context:store(Context, 'started_chunk', 'true')})
    end;

normalize_and_send('csv', [], Payload) -> Payload;
normalize_and_send('csv', [JObj|JObjs], {Req, Context}) ->
    case cb_context:fetch(Context, 'started_chunk') of
        'true' ->
            CSV =  <<(normalize_cdr_to_csv_header(JObj, Context))/binary
                      ,(normalize_cdr_to_csv(JObj, Context))/binary
                    >>,
            'ok' = cowboy_req:chunk(CSV, Req),
            normalize_and_send('csv', JObjs, {Req, Context});
        _Else ->
            'ok' = cowboy_req:chunk(normalize_cdr_to_csv(JObj, Context), Req),
            normalize_and_send('csv', JObjs, {Req, cb_context:store(Context, 'started_chunk', 'true')})
    end.

-spec normalize_cdr(wh_json:object(), cb_context:context()) -> wh_json:object().
normalize_cdr(JObj, Context) ->
    Timestamp = wh_json:get_value(<<"timestamp">>, JObj, 0),
    maybe_reseller_cdr(
        wh_json:from_list([
            {<<"id">>, wh_json:get_value(<<"_id">>, JObj, <<>>)}
            ,{<<"call_id">>, wh_json:get_value(<<"call_id">>, JObj, <<>>)}
            ,{<<"caller_id_number">>, wh_json:get_value(<<"caller_id_number">>, JObj, <<>>)}
            ,{<<"caller_id_name">>, wh_json:get_value(<<"caller_id_name">>, JObj, <<>>)}
            ,{<<"callee_id_number">>, wh_json:get_value(<<"callee_id_number">>, JObj, <<>>)}
            ,{<<"callee_id_name">>, wh_json:get_value(<<"callee_id_name">>, JObj, <<>>)}
            ,{<<"duration_seconds">>, wh_json:get_value(<<"duration_seconds">>, JObj, <<>>)}
            ,{<<"billing_seconds">>, wh_json:get_value(<<"billing_seconds">>, JObj, <<>>)}
            ,{<<"timestamp">>, Timestamp}
            ,{<<"hangup_cause">>, wh_json:get_value(<<"hangup_cause">>, JObj, <<>>)}
            ,{<<"other_leg_call_id">>, wh_json:get_value(<<"other_leg_call_id">>, JObj, <<>>)}
            ,{<<"owner_id">>, wh_json:get_value([<<"custom_channel_vars">>, <<"owner_id">>], JObj, <<>>)}
            ,{<<"to">>, wh_json:get_value(<<"to">>, JObj, <<>>)}
            ,{<<"from">>, wh_json:get_value(<<"from">>, JObj, <<>>)}
            ,{<<"direction">>, wh_json:get_value(<<"call_direction">>, JObj, <<>>)}
            ,{<<"request">>, wh_json:get_value(<<"request">>, JObj, <<>>)}
            ,{<<"authorizing_id">>, wh_json:get_value([<<"custom_channel_vars">>, <<"authorizing_id">>], JObj, <<>>)}
            ,{<<"cost">>, customer_cost(JObj)}
            % New fields
            ,{<<"dialed_number">>, dialed_number(JObj)}
            ,{<<"calling_from">>, calling_from(JObj)}
            ,{<<"datetime">>, pretty_print_datetime(Timestamp)}
            ,{<<"unix_timestamp">>, wh_util:gregorian_seconds_to_unix_seconds(Timestamp)}
            ,{<<"call_type">>, wh_json:get_value([<<"custom_channel_vars">>, <<"account_billing">>], JObj, <<>>)}
            ,{<<"rate">>, wht_util:units_to_dollars(wh_json:get_value([<<"custom_channel_vars">>, <<"rate">>], JObj, 0))}
            ,{<<"rate_name">>, wh_json:get_value([<<"custom_channel_vars">>, <<"rate_name">>], JObj, <<>>)}
        ])
        ,Context
    ).

-spec normalize_cdr_to_csv(wh_json:object(), cb_context:context()) -> ne_binary().
normalize_cdr_to_csv(JObj, Context) ->
    Timestamp = wh_json:get_value(<<"timestamp">>, JObj, 0),
    CSV = wh_util:join_binary([
        wh_json:get_value(<<"_id">>, JObj, <<>>)
        ,wh_json:get_value(<<"call_id">>, JObj, <<>>)
        ,wh_json:get_value(<<"caller_id_number">>, JObj, <<>>)
        ,wh_json:get_value(<<"caller_id_name">>, JObj, <<>>)
        ,wh_json:get_value(<<"callee_id_number">>, JObj, <<>>)
        ,wh_json:get_value(<<"callee_id_name">>, JObj, <<>>)
        ,wh_json:get_value(<<"duration_seconds">>, JObj, <<>>)
        ,wh_json:get_value(<<"billing_seconds">>, JObj, <<>>)
        ,wh_util:to_binary(Timestamp)
        ,wh_json:get_value(<<"hangup_cause">>, JObj, <<>>)
        ,wh_json:get_value(<<"other_leg_call_id">>, JObj, <<>>)
        ,wh_json:get_value([<<"custom_channel_vars">>, <<"owner_id">>], JObj, <<>>)
        ,wh_json:get_value(<<"to">>, JObj, <<>>)
        ,wh_json:get_value(<<"from">>, JObj, <<>>)
        ,wh_json:get_value(<<"call_direction">>, JObj, <<>>)
        ,wh_json:get_value(<<"request">>, JObj, <<>>)
        ,wh_json:get_value([<<"custom_channel_vars">>, <<"authorizing_id">>], JObj, <<>>)
        ,wh_util:to_binary(customer_cost(JObj))
        % New fields
        ,dialed_number(JObj)
        ,calling_from(JObj)
        ,pretty_print_datetime(Timestamp)
        ,wh_util:to_binary(wh_util:gregorian_seconds_to_unix_seconds(Timestamp))
        ,wh_json:get_value([<<"custom_channel_vars">>, <<"account_billing">>], JObj, <<>>)
        ,wh_util:to_binary(wht_util:units_to_dollars(wh_json:get_value([<<"custom_channel_vars">>, <<"rate">>], JObj, 0)))
        ,wh_json:get_value([<<"custom_channel_vars">>, <<"rate_name">>], JObj, <<>>)
    ], <<",">>),
    case cb_context:fetch(Context, 'is_reseller') of
        'false' -> <<CSV/binary, "\r">>;
        'true' ->  <<CSV/binary, ",reseller_cost,reseller_call_type\r">>
    end.

-spec normalize_cdr_to_csv_header(wh_json:object(), cb_context:context()) -> ne_binary().
normalize_cdr_to_csv_header(JObj, Context) ->
    CSV = <<"id,"
            ,"call_id,"
            ,"caller_id_number,"
            ,"caller_id_name,"
            ,"callee_id_number,"
            ,"callee_id_name,"
            ,"duration_seconds,"
            ,"billing_seconds,"
            ,"timestamp,"
            ,"hangup_cause,"
            ,"other_leg_call_id,"
            ,"owner_id,"
            ,"to,"
            ,"from,"
            ,"direction,"
            ,"request,"
            ,"authorizing_id,"
            ,"cost,"
            % New fields
            ,"dialed_number,"
            ,"calling_from,"
            ,"datetime,"
            ,"unix_timestamp,"
            ,"call_type,"
            ,"rate,"
            ,"rate_name"
          >>,
    case cb_context:fetch(Context, 'is_reseller') of
        'false' -> <<CSV/binary, "\r">>;
        'true' -> <<CSV/binary
                    ,(wh_util:to_binary(reseller_cost(JObj)))/binary, ","
                    ,(wh_json:get_value([<<"custom_channel_vars">>, <<"reseller_billing">>], JObj, <<>>))/binary
                    ,"\r"
                  >>
    end.

-spec pretty_print_datetime(wh_datetime() | wh_now()) -> ne_binary().
pretty_print_datetime(Timestamp) when is_integer(Timestamp) ->
    pretty_print_datetime(calendar:gregorian_seconds_to_datetime(Timestamp));
pretty_print_datetime({{Y,Mo,D},{H,Mi,S}}) ->
    iolist_to_binary(io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
                                   [Y, Mo, D, H, Mi, S])).

-spec dialed_number(wh_json:object()) -> binary().
dialed_number(JObj) ->
    case wh_json:get_value(<<"call_direction">>, JObj) of
        <<"inbound">> ->
            [Num|_] = binary:split(wh_json:get_value(<<"request">>, JObj, <<>>), <<"@">>),
            Num;
        <<"outbound">> ->
            [Num|_] = binary:split(wh_json:get_value(<<"to">>, JObj, <<>>), <<"@">>),
            Num
    end.

-spec calling_from(wh_json:object()) -> binary().
calling_from(JObj) ->
    case wh_json:get_value(<<"call_direction">>, JObj) of
        <<"inbound">> ->wh_json:get_value(<<"caller_id_number">>, JObj, <<>>);
        <<"outbound">> ->
            [Num|_] = binary:split(wh_json:get_value(<<"from_uri">>, JObj, <<>>), <<"@">>),
            Num
    end.

-spec maybe_reseller_cdr(wh_json:object(), cb_context:context()) -> wh_json:object().
maybe_reseller_cdr(JObj, Context) ->
    case cb_context:fetch(Context, 'is_reseller') of
        'false' -> JObj;
        'true' -> reseller_cdr(JObj)
    end.

-spec reseller_cdr(wh_json:object()) -> wh_json:object().
reseller_cdr(JObj) ->
    wh_json:set_values([
        {<<"reseller_cost">>, reseller_cost(JObj)}
        ,{<<"reseller_call_type">>, wh_json:get_value([<<"custom_channel_vars">>, <<"reseller_billing">>], JObj, <<>>)}
    ], JObj).

-spec customer_cost(wh_json:object()) -> pos_integer().
customer_cost(JObj) ->
    case wh_json:get_value([<<"custom_channel_vars">>, <<"account_billing">>], JObj) of
        <<"per_minute">> -> wht_util:call_cost(JObj);
        _ -> 0
    end.

-spec reseller_cost(wh_json:object()) -> pos_integer().
reseller_cost(JObj) ->
    case wh_json:get_value([<<"custom_channel_vars">>, <<"reseller_billing">>], JObj) of
        <<"per_minute">> -> wht_util:call_cost(JObj);
        _ -> 0
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec remove_qs_keys(cb_context:context()) -> cb_context:context().
remove_qs_keys(Context) ->
    cb_context:set_query_string(Context, wh_json:delete_keys([<<"created_from">>
                                                              ,<<"created_to">>
                                                             ]
                                                             ,cb_context:query_string(Context)
                                                            )).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a CDR document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_cdr(ne_binary(), cb_context:context()) -> cb_context:context().
load_cdr(<<Year:4/binary, Month:2/binary, "-", _/binary>> = CDRId, Context) ->
    AccountId = cb_context:account_id(Context),
    AcctDb = kazoo_modb:get_modb(AccountId, wh_util:to_integer(Year), wh_util:to_integer(Month)),
    Context1 = cb_context:set_account_db(Context,AcctDb),
    crossbar_doc:load(CDRId, Context1);
load_cdr(CDRId, Context) ->
    lager:debug("error loading cdr by id ~p", [CDRId]),
    crossbar_util:response('error', <<"could not find cdr with supplied id">>, 404, Context).
