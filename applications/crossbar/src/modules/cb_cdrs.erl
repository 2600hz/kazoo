%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
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
	,content_types_provided/1
	,validate/1, validate/2, validate/3
	,to_json/1
	,to_csv/1
        ]).
-export([pagination/1]).
-export([fetch_view_options/1]).
-export([get_cdr_ids/3]).
-export([maybe_paginate_and_clean/2]).
-export([load_chunked_cdrs/3]).

-include("crossbar.hrl").

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".cdrs">>).
-define(MAX_BULK, kapps_config:get_integer(?MOD_CONFIG_CAT, <<"maximum_bulk">>, 50)).
-define(CB_LIST_BY_USER, <<"cdrs/listing_by_owner">>).
-define(CB_LIST, <<"cdrs/crossbar_listing">>).
-define(CB_INTERACTION_LIST, <<"cdrs/interaction_listing">>).
-define(CB_INTERACTION_LIST_BY_USER, <<"cdrs/interaction_listing_by_owner">>).
-define(CB_INTERACTION_LIST_BY_ID, <<"cdrs/interaction_listing_by_id">>).

-define(PATH_INTERACTION, <<"interaction">>).
-define(PATH_LEGS, <<"legs">>).

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
	,{<<"call_priority">>, fun col_call_priority/2}
	]).

-define(COLUMNS_RESELLER
       ,[{<<"reseller_cost">>, fun col_reseller_cost/2}
	,{<<"reseller_call_type">>, fun col_reseller_call_type/2}
	]).

-type payload() :: {cowboy_req:req(), cb_context:context()}.
-export_type([payload/0]).

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
to_json({Req, Context}) ->
    Nouns = cb_context:req_nouns(Context),
    case props:get_value(<<"cdrs">>, Nouns, []) of
        [] -> to_json(Req, Context);
        [?PATH_INTERACTION] -> to_json(Req, cb_context:store(Context, 'interaction', 'true'));
        [_|_] -> {Req, Context}
    end.

-spec to_json(cowboy_req:req(), cb_context:context()) -> payload().
to_json(Req0, Context) ->
    Headers = cowboy_req:get('resp_headers', Req0),
    {'ok', Req1} = cowboy_req:chunked_reply(200, Headers, Req0),
    'ok' = cowboy_req:chunk("{\"status\":\"success\", \"data\":[", Req1),
    {Req2, Context1} = send_chunked_cdrs({Req1, Context}),
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
    'ok' = cowboy_req:chunk(<<", \"page_size\": ", (kz_util:to_binary(PageSize))/binary>>, Req),
    IsInteraction = cb_context:fetch(Context, 'interaction', 'false'),
    case pagination_key(IsInteraction, 'next_start_key', Context) of
        'ok' -> 'ok';
        Next -> cowboy_req:chunk(<<", \"next_start_key\": \"", (kz_util:to_binary(Next))/binary, "\"">>, Req)
    end,
    StartKey = pagination_key(IsInteraction, 'start_key', Context),
    'ok' = cowboy_req:chunk(<<", \"start_key\": \"", (kz_util:to_binary(StartKey))/binary, "\"">>, Req),
    Payload.

-spec pagination_key(boolean(), atom(), cb_context:context()) ->
                            'ok' | ne_binary() | integer().
pagination_key('false', PaginationKey, Context) ->
    case cb_context:fetch(Context, PaginationKey) of
        'undefined' -> 'ok';
        [_, Key] -> Key;
        [Key] -> Key;
        Key -> Key
    end;
pagination_key('true', PaginationKey, Context) ->
    case cb_context:fetch(Context, PaginationKey) of
        'undefined' -> 'ok';
        [_, Key, _] -> Key;
        [Key, _] -> Key;
        [Key] -> Key;
        Key -> Key
    end.

-spec to_csv(payload()) -> payload().
to_csv({Req, Context}) ->
    Nouns = cb_context:req_nouns(Context),
    case props:get_value(<<"cdrs">>, Nouns, []) of
        [] -> to_csv(Req, Context);
        [?PATH_INTERACTION] -> to_csv(Req, Context);
        [_|_] -> {Req, Context}
    end.

-spec to_csv(cowboy_req:req(), cb_context:context()) -> payload().
to_csv(Req, Context) ->
    Headers = props:set_values([{<<"content-type">>, <<"application/octet-stream">>}
			       ,{<<"content-disposition">>, <<"attachment; filename=\"cdrs.csv\"">>}
                               ]
			      ,cowboy_req:get('resp_headers', Req)
                              ),
    {'ok', Req1} = cowboy_req:chunked_reply(200, Headers, Req),
    Context1 = cb_context:store(Context, 'is_csv', 'true'),
    {Req2, _} = send_chunked_cdrs({Req1, Context1}),
    {Req2, cb_context:store(Context1,'is_chunked', 'true')}.

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
allowed_methods(_) ->
    [?HTTP_GET].
allowed_methods(?PATH_LEGS, _) ->
    [?HTTP_GET];
allowed_methods(_ , _) -> [].

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
    load_cdr_summary(Context, cb_context:req_nouns(Context)).

validate(Context, ?PATH_INTERACTION) ->
    load_interaction_cdr_summary(Context, cb_context:req_nouns(Context));
validate(Context, CDRId) ->
    load_cdr(CDRId, Context).

validate(Context, ?PATH_LEGS, InteractionId) ->
    load_legs(InteractionId, Context);
validate(Context, _, _) ->
    cb_context:add_system_error('invalid request', Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of accounts, each summarized.  Or a specific
%% account summary.
%% @end
%%--------------------------------------------------------------------
-spec load_cdr_summary(cb_context:context(), req_nouns()) -> cb_context:context().
load_cdr_summary(Context, [_, {?KZ_ACCOUNTS_DB, [_]} | _]) ->
    lager:debug("loading cdrs for account ~s", [cb_context:account_id(Context)]),
    case create_view_options('undefined', Context) of
        {'ok', ViewOptions} ->
            load_view(?CB_LIST
		     ,props:filter_undefined(ViewOptions)
		     ,remove_qs_keys(Context)
                     );
        Else -> Else
    end;
load_cdr_summary(Context, [_, {<<"users">>, [UserId] } | _]) ->
    lager:debug("loading cdrs for user ~s", [UserId]),
    case create_view_options(UserId, Context) of
        {'ok', ViewOptions} ->
            load_view(?CB_LIST_BY_USER
		     ,props:filter_undefined(ViewOptions)
		     ,remove_qs_keys(Context)
                     );
        ErrorContext -> ErrorContext
    end;
load_cdr_summary(Context, _Nouns) ->
    lager:debug("invalid URL chain for cdr summary request"),
    cb_context:add_system_error('faulty_request', Context).

-spec load_interaction_cdr_summary(cb_context:context(), req_nouns()) ->
					  cb_context:context().
load_interaction_cdr_summary(Context, [_, {?KZ_ACCOUNTS_DB, [_]} | _]) ->
    lager:debug("loading interaction cdrs for account ~s"
	       ,[cb_context:account_id(Context)]
               ),
    case create_view_options('undefined'
			    ,fun create_interaction_view_options/4
			    ,Context
                            )
    of
        {'ok', ViewOptions} ->
            load_view(?CB_INTERACTION_LIST
		     ,props:filter_undefined(ViewOptions)
		     ,fun interaction_view_option/2
		     ,remove_qs_keys(Context)
                     );
        ErrorContext -> ErrorContext
    end;
load_interaction_cdr_summary(Context, [_, {<<"users">>, [UserId] } | _]) ->
    lager:debug("loading interaction cdrs for user ~s", [UserId]),
    case create_view_options(UserId, fun create_interaction_view_options/4, Context) of
        {'ok', ViewOptions} ->
            load_view(?CB_INTERACTION_LIST_BY_USER
		     ,props:filter_undefined(ViewOptions)
		     ,fun interaction_view_option/2
		     ,remove_qs_keys(Context)
                     );
        ErrorContext -> ErrorContext
    end;
load_interaction_cdr_summary(Context, _Nouns) ->
    lager:debug("invalid URL chain for interaction cdr summary request"),
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
           ]};
create_view_options(OwnerId, Context, CreatedFrom, CreatedTo) ->
    {'ok', [{'startkey', [OwnerId, CreatedTo]}
	   ,{'endkey', [OwnerId, CreatedFrom]}
	   ,{'limit', pagination_page_size(Context)}
	   ,'descending'
           ]}.

-spec create_interaction_view_options(api_binary(), cb_context:context(), pos_integer(), pos_integer()) ->
					     {'ok', crossbar_doc:view_options()}.
create_interaction_view_options('undefined', Context, CreatedFrom, CreatedTo) ->
    {'ok', [{'startkey', [CreatedTo]}
	   ,{'endkey', [CreatedFrom, kz_json:new()]}
	   ,{'limit', pagination_page_size(Context)}
	   ,{'group', 'true'}
	   ,{'group_level', 2}
	   ,{'reduce', 'true'}
	   ,'descending'
           ]};
create_interaction_view_options(OwnerId, Context, CreatedFrom, CreatedTo) ->
    {'ok', [{'startkey', [OwnerId, CreatedTo]}
	   ,{'endkey', [OwnerId, CreatedFrom, kz_json:new()]}
	   ,{'limit', pagination_page_size(Context)}
	   ,{'group', 'true'}
	   ,{'group_level', 3}
	   ,{'reduce', 'true'}
	   ,'descending'
           ]}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec pagination_page_size(cb_context:context()) ->pos_integer().
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
-type view_timestamp_fun() :: fun(('startkey' | 'endkey', crossbar_doc:view_options()) -> gregorian_seconds()).

-spec load_view(ne_binary(), kz_proplist(), cb_context:context()) ->
                       cb_context:context().
-spec load_view(ne_binary(), kz_proplist(), view_timestamp_fun(), cb_context:context()) ->
                       cb_context:context().
load_view(View, ViewOptions, Context) ->
    load_view(View, ViewOptions, fun view_option/2, Context).

load_view(View, ViewOptions, Fun, Context) ->
    AccountId = cb_context:account_id(Context),

    ContextChanges =
        [{fun cb_context:store/3, 'chunked_dbs', chunked_dbs(AccountId, ViewOptions, Fun)}
	,{fun cb_context:store/3, 'chunked_view_options', ViewOptions}
	,{fun cb_context:store/3, 'chunked_view', View}
	,{fun cb_context:set_resp_status/2, 'success'}
        ],
    cb_context:setters(Context, ContextChanges).

-spec chunked_dbs(ne_binary(), kz_proplist(), view_timestamp_fun()) ->
                         ne_binaries().
chunked_dbs(AccountId, ViewOptions, Fun) ->
    To = Fun('startkey',ViewOptions),
    From = Fun('endkey',  ViewOptions),
    kazoo_modb:get_range(AccountId, From, To).

-spec view_option('endkey' | 'startkey', crossbar_doc:view_options()) ->
                         gregorian_seconds().
view_option(Key, ViewOptions) ->
    case props:get_value(Key, ViewOptions) of
        [_, Option] -> Option;
        Option -> Option
    end.

-spec interaction_view_option('endkey' | 'startkey', crossbar_doc:view_options()) ->
				     gregorian_seconds().
interaction_view_option(Key, ViewOptions) ->
    case props:get_value(Key, ViewOptions) of
        [_, Option, _] -> Option;
        [Option, _] -> Option;
        [Option] -> Option;
        Option -> Option
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec send_chunked_cdrs(payload()) -> payload().
send_chunked_cdrs({Req, Context}) ->
    Dbs = cb_context:fetch(Context, 'chunked_dbs'),
    AuthAccountId = cb_context:auth_account_id(Context),
    IsReseller = kz_services:is_reseller(AuthAccountId),
    send_chunked_cdrs(Dbs, {Req, cb_context:store(Context, 'is_reseller', IsReseller)}).

-spec send_chunked_cdrs(ne_binaries(), payload()) -> payload().
send_chunked_cdrs([], Payload) -> Payload;
send_chunked_cdrs([Db | Dbs], {Req, Context}) ->
    View = cb_context:fetch(Context, 'chunked_view'),
    ViewOptions = fetch_view_options(Context),
    Context1 = cb_context:store(Context, 'start_key', props:get_value('startkey', ViewOptions)),
    Context2 = cb_context:store(Context1, 'page_size', 0),
    {'ok', Ids} = get_cdr_ids(Db, View, ViewOptions),
    {Context3, CDRIds} = maybe_paginate_and_clean(Context2, Ids),
    send_chunked_cdrs(Dbs, load_chunked_cdrs(Db, CDRIds, {Req, Context3})).

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
        'true' ->
            {Context, [Id || {Id, _} <- Ids]};
        _ ->
            paginate_and_clean(Context, Ids)
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

-spec get_cdr_ids(ne_binary(), ne_binary(), kz_datamgr:view_options()) ->
                         {'ok', kz_proplist()}.
get_cdr_ids(Db, View, ViewOptions) ->
    _ = maybe_add_design_doc(Db),
    case kz_datamgr:get_results(Db, View, ViewOptions) of
        {'error', _R} ->
            lager:debug("unable to fetch ~s from ~s: ~p"
                       ,[View, Db, _R]
                       ),
            {'ok', []};
        {'ok', JObjs} ->
            lager:debug("fetched cdr ids from ~s", [Db]),
            {'ok', [{kz_doc:id(JObj), kz_json:get_value(<<"key">>, JObj)}
                    || JObj <- JObjs
                   ]}
    end.

-spec maybe_add_design_doc(ne_binary()) ->
                                  'ok' |
                                  {'error', 'not_found'}.
maybe_add_design_doc(Db) ->
    case kz_datamgr:lookup_doc_rev(Db, <<"_design/cdrs">>) of
        {'error', 'not_found'} ->
            lager:warning("adding cdr views to modb: ~s", [Db]),
            kz_datamgr:revise_doc_from_file(Db
                                           ,'kazoo_modb'
                                           ,<<"cdrs.json">>
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
		  ,{'doc_type', <<"cdr">>}
		  ,'include_docs'
                  ],
    case kz_datamgr:all_docs(Db, ViewOptions) of
        {'ok', Results} ->
            HasQSFilter = crossbar_doc:has_qs_filter(Context),
            JObjs = [kz_json:get_value(<<"doc">>, Result)
                     || Result <- Results,
                        crossbar_doc:filtered_doc_by_qs(Result, HasQSFilter, Context)
                    ],
            P = normalize_and_send(JObjs, Payload),
            load_chunked_cdrs(Db, Remaining, P);
        {'error', _E} ->
            load_chunked_cdrs(Db, Remaining, Payload)
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
    CDR = normalize_cdr(JObj, Context),
    case cb_context:fetch(Context, 'started_chunk') of
        'true' ->
            'ok' = cowboy_req:chunk(<<",", (kz_json:encode(CDR))/binary>>, Req),
            normalize_and_send('json', JObjs, {Req, Context});
        _Else ->
            'ok' = cowboy_req:chunk(kz_json:encode(CDR), Req),
            normalize_and_send('json', JObjs, {Req, cb_context:store(Context, 'started_chunk', 'true')})
    end;
normalize_and_send('csv', [], Payload) -> Payload;
normalize_and_send('csv', [JObj|JObjs], {Req, Context}) ->
    case cb_context:fetch(Context, 'started_chunk') of
        'true' ->
            'ok' = cowboy_req:chunk(normalize_cdr_to_csv(JObj, Context), Req),
            normalize_and_send('csv', JObjs, {Req, Context});
        _Else ->
            CSV = <<(normalize_cdr_to_csv_header(JObj, Context))/binary
                    ,(normalize_cdr_to_csv(JObj, Context))/binary
                  >>,
            'ok' = cowboy_req:chunk(CSV, Req),
            normalize_and_send('csv', JObjs, {Req, cb_context:store(Context, 'started_chunk', 'true')})
    end.

-spec normalize_cdr(kz_json:object(), cb_context:context()) -> kz_json:object().
normalize_cdr(JObj, Context) ->
    Duration = kz_json:get_integer_value(<<"duration_seconds">>, JObj, 0),
    Timestamp = kz_json:get_integer_value(<<"timestamp">>, JObj, 0) - Duration,
    kz_json:from_list(
      [{K, F(JObj, Timestamp)} || {K, F} <- csv_rows(Context)]
     ).

-spec normalize_cdr_to_csv(kz_json:object(), cb_context:context()) -> ne_binary().
normalize_cdr_to_csv(JObj, Context) ->
    Timestamp = kz_json:get_integer_value(<<"timestamp">>, JObj, 0),

    CSV = kz_util:join_binary(
            [F(JObj, Timestamp) || {_, F} <- csv_rows(Context)]
			     ,<<",">>
           ),
    <<CSV/binary, "\r\n">>.

-spec normalize_cdr_to_csv_header(kz_json:object(), cb_context:context()) -> ne_binary().
normalize_cdr_to_csv_header(_JObj, Context) ->
    CSV =
        kz_util:join_binary(
          [K || {K, _Fun} <- csv_rows(Context)]
			   ,<<",">>
         ),

    <<CSV/binary, "\r\n">>.

-type csv_column_fun() :: fun((kz_json:object(), gregorian_seconds()) -> ne_binary()).

-spec csv_rows(cb_context:context()) -> [{ne_binary(), csv_column_fun()}].
csv_rows(Context) ->
    case cb_context:fetch(Context, 'is_reseller') of
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
col_timestamp(_JObj, Timestamp) -> kz_util:to_binary(Timestamp).
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
col_customer_cost(JObj, _Timestamp) -> kz_util:to_binary(customer_cost(JObj)).

col_dialed_number(JObj, _Timestamp) -> dialed_number(JObj).
col_calling_from(JObj, _Timestamp) -> calling_from(JObj).
col_pretty_print(_JObj, Timestamp) -> pretty_print_datetime(Timestamp).
col_unix_timestamp(_JObj, Timestamp) -> kz_util:to_binary(kz_util:gregorian_seconds_to_unix_seconds(Timestamp)).
col_rfc1036(_JObj, Timestamp) -> list_to_binary([$", kz_util:rfc1036(Timestamp), $"]).
col_iso8601(_JObj, Timestamp) -> list_to_binary([$", kz_util:iso8601(Timestamp), $"]).
col_account_call_type(JObj, _Timestamp) -> kz_json:get_value([?KEY_CCV, <<"account_billing">>], JObj, <<>>).
col_rate(JObj, _Timestamp) -> kz_util:to_binary(wht_util:units_to_dollars(kz_json:get_value([?KEY_CCV, <<"rate">>], JObj, 0))).
col_rate_name(JObj, _Timestamp) -> kz_json:get_value([?KEY_CCV, <<"rate_name">>], JObj, <<>>).
col_bridge_id(JObj, _Timestamp) -> kz_json:get_value([?KEY_CCV, <<"bridge_id">>], JObj, <<>>).
col_recording_url(JObj, _Timestamp) -> kz_json:get_value([<<"recording_url">>], JObj, <<>>).
col_call_priority(JObj, _Timestamp) -> kz_json:get_value([?KEY_CCV, <<"call_priority">>], JObj, <<>>).

col_reseller_cost(JObj, _Timestamp) -> kz_util:to_binary(reseller_cost(JObj)).
col_reseller_call_type(JObj, _Timestamp) -> kz_json:get_value([?KEY_CCV, <<"reseller_billing">>], JObj, <<>>).

-spec pretty_print_datetime(kz_datetime() | integer()) -> ne_binary().
pretty_print_datetime(Timestamp) when is_integer(Timestamp) ->
    pretty_print_datetime(calendar:gregorian_seconds_to_datetime(Timestamp));
pretty_print_datetime({{Y,Mo,D},{H,Mi,S}}) ->
    iolist_to_binary(io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w"
				  ,[Y, Mo, D, H, Mi, S]
                                  )).

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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a CDR document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_cdr(ne_binary(), cb_context:context()) -> cb_context:context().
load_cdr(?MATCH_MODB_PREFIX(Year,Month,_) = CDRId, Context) ->
    AccountId = cb_context:account_id(Context),
    AccountDb = kazoo_modb:get_modb(AccountId, kz_util:to_integer(Year), kz_util:to_integer(Month)),
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
    AccountDb = kazoo_modb:get_modb(AccountId, kz_util:to_integer(Year), kz_util:to_integer(Month)),
    Context1 = cb_context:set_account_db(Context, AccountDb),
    case kz_datamgr:open_cache_doc(AccountDb, {<<"cdr">>, DocId}) of
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
