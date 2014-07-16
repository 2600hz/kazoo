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
-module(cb_cdrs_v2).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,content_types_provided/1
         ,validate/1, validate/2
         ,to_json/1
        ]).

-include("../crossbar.hrl").

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".cdrs">>).
-define(MAX_BULK, whapps_config:get_integer(?MOD_CONFIG_CAT, <<"maximum_bulk">>, 50)).

-define(CB_LIST_BY_USER, <<"cdrs/listing_by_owner_v2">>).
-define(CB_LIST, <<"cdrs/crossbar_listing_v2">>).

%%%===================================================================
%%% Internal functions
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"v2_resource.allowed_methods.cdrs">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"v2_resource.resource_exists.cdrs">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"v2_resource.content_types_provided.cdrs">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"v2_resource.to_json.get.cdrs">>, ?MODULE, 'to_json'),
    _ = crossbar_bindings:bind(<<"v2_resource.validate.cdrs">>, ?MODULE, 'validate').


to_json({Req, Context}) ->
    Req1 = cowboy_req:set_resp_header(<<"is_chunked">>, 'true', Req),
    {Req1, Context}.

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
    CreatedTo = wh_util:to_integer(cb_context:req_value(Context, <<"created_to">>, CreatedFrom + MaxRange)),
    Diff = CreatedTo - CreatedFrom,
    case Diff of
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
                   ]};
        _N ->
            {'ok', [{'startkey', [OwnerId, CreatedFrom]}
                    ,{'endkey', [OwnerId, CreatedTo]}
                   ]}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
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
    From = view_key_created_from(ViewOptions),
    To = view_key_created_to(ViewOptions),

    Context1 = chunk('start', Context, 'ok'),

    case {kazoo_modb:get_modb(AccountId, From)
          ,kazoo_modb:get_modb(AccountId, To)}
    of
        {Db, Db} ->
            fetch_cdrs(View, ViewOptions, Context1, [Db]);
        {DbFrom, DbTo} ->
            fetch_cdrs(View, ViewOptions, Context1, [DbFrom, DbTo])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch_cdrs(ne_binary(), wh_proplist(), cb_context:context(), ne_binaries()) ->
                        cb_context:context().
fetch_cdrs(_, _, Context, []) ->
    Context1 = chunk('end', Context, 'ok'),
    cb_context:set_resp_status(Context1, 'success');
fetch_cdrs(View, ViewOptions, Context, [Db|Dbs]) ->
    case couch_mgr:get_results(Db, View, ViewOptions) of
        {'ok', JObjs} ->
            Ids = [wh_json:get_value(<<"id">>, JObj) || JObj <- JObjs],
            _ = chunk_cdrs(Db, Context, Ids),
            fetch_cdrs(View, ViewOptions, Context, Dbs);
        {'error', _E} ->
            fetch_cdrs(View, ViewOptions, Context, Dbs)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
chunk_cdrs(_, _, []) -> 'ok';
chunk_cdrs(Db, Context, Ids) ->
    Split = case erlang:length(Ids) < ?MAX_BULK of
                'true' -> erlang:length(Ids);
                'false' -> ?MAX_BULK
            end,
    {BulkIds, Remaining} = lists:split(Split, Ids),
    ViewOptions = [{'keys', BulkIds}
                   ,'include_docs'
                  ],
    case couch_mgr:all_docs(Db, ViewOptions) of
        {'ok', JObjs} ->
            normalize_and_send(JObjs, Context),
            case erlang:length(Ids) < ?MAX_BULK of
                'true' -> 'ok';
                'false' -> chunk('comma', Context, 'ok')
            end,
            chunk_cdrs(Db, Context, Remaining);
        {'error', _E} ->
            chunk_cdrs(Db, Context, Remaining)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
normalize_and_send([], _) -> 'ok';
normalize_and_send([JObj|[]], Context) ->
    Doc = wh_json:get_value(<<"doc">>, JObj),
    CDRJObj = normalize_cdr(Doc),
    _ = chunk('data', Context, CDRJObj),
    normalize_and_send([], Context);
normalize_and_send([JObj|JObjs], Context) ->
    Doc = wh_json:get_value(<<"doc">>, JObj),
    CDRJObj = normalize_cdr(Doc),
    _ = chunk('data', Context, CDRJObj),
    _ = chunk('comma', Context, 'ok'),
    normalize_and_send(JObjs, Context).

normalize_cdr(JObj) ->
    Timestamp = wh_json:get_value(<<"timestamp">>, JObj, 0),
    wh_json:from_list([
        {<<"datetime">>, wh_util:pretty_print_datetime(Timestamp)}
        ,{<<"call_id">>, wh_json:get_value(<<"call_id">>, JObj, <<>>)}
        ,{<<"to_number">>, wh_json:get_value(<<"callee_id_number">>, JObj, <<>>)}
        ,{<<"caller_id">>, wh_json:get_value(<<"caller_id_number">>, JObj, <<>>)}
        ,{<<"caller_id">>, wh_json:get_value(<<"caller_id_number">>, JObj, <<>>)}
        ,{<<"billing_seconds">>, wh_json:get_value(<<"billing_seconds">>, JObj, <<>>)}
        ,{<<"customer_cost">>, wht_util:call_cost(JObj)}
        ,{<<"gregorian_timestamp">>, Timestamp}
        ,{<<"unix_timestamp">>, wh_util:gregorian_seconds_to_unix_seconds(Timestamp)}
    ]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
chunk('start', Context, _) ->
    Req0 = cb_context:fetch(Context, 'req'),
    Headers = cowboy_req:get('resp_headers', Req0),
    {'ok', Req1} = cowboy_req:chunked_reply(200, Headers, Req0),
    Context1 = cb_context:store(Context, 'req', Req1),
    'ok' = cowboy_req:chunk("{\"status\":\"success\", \"data\":[", Req1),
    Context1;
chunk('data', Context, Data) ->
    Req = cb_context:fetch(Context, 'req'),
    'ok' = cowboy_req:chunk(
                binary:bin_to_list(
                    wh_json:encode(Data)
                ), Req),
    Context;
chunk('comma', Context, _) ->
    Req = cb_context:fetch(Context, 'req'),
    'ok' = cowboy_req:chunk(",", Req),
    Context;
chunk('end', Context, _) ->
    Req = cb_context:fetch(Context, 'req'),
    'ok' = cowboy_req:chunk("]}", Req),
    'ok' = cowboy_req:ensure_response(Req, 200),
    Context.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec view_key_created_to(wh_proplist()) -> pos_integer().
view_key_created_to(Props) ->
    case props:get_value('endkey', Props) of
        [_, CreatedTo] -> CreatedTo;
        CreatedTo -> CreatedTo
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec view_key_created_from(wh_proplist()) -> pos_integer().
view_key_created_from(Props) ->
    case props:get_value('startkey', Props) of
        [_, CreatedFrom] -> CreatedFrom;
        CreatedFrom -> CreatedFrom
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
