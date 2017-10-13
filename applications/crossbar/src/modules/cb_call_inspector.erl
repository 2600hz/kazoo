%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors:
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_call_inspector).

-export([init/0
        ,allowed_methods/0, allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,validate/1, validate/2
        ,to_json/1
        ,to_csv/1
        ]).

-include("crossbar.hrl").

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".call_inspector">>).
-define(MAX_BULK, kapps_config:get_pos_integer(?MOD_CONFIG_CAT, <<"maximum_bulk">>, 50)).

-define(CB_LIST, <<"cdrs/crossbar_listing">>).
-define(CB_LIST_BY_USER, <<"cdrs/listing_by_owner">>).

-define(MATCH_CDR_ID(YYYYMM, CallId), <<YYYYMM:6/binary, "-", CallId/binary>>).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Initializes the bindings this module will respond to.
%% @end
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.call_inspector">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.call_inspector">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.to_json.get.call_inspector">>, ?MODULE, 'to_json'),
    _ = crossbar_bindings:bind(<<"*.to_csv.get.call_inspector">>, ?MODULE, 'to_csv'),
    crossbar_bindings:bind(<<"*.validate.call_inspector">>, ?MODULE, 'validate').

-spec to_json(cb_cowboy_payload()) -> cb_cowboy_payload().
to_json({Req, Context}) ->
    AuthAccountId = cb_context:auth_account_id(Context),
    IsReseller = kz_services:is_reseller(AuthAccountId),
    to_json(Req, cb_context:store(Context, 'is_reseller', IsReseller), get_view_options(cb_context:req_nouns(Context))).

-spec to_json(cowboy_req:req(), cb_context:context(), {api_ne_binary(), crossbar_view:options()}) -> cb_cowboy_payload().
to_json(Req, Context, {'undefined', _}) ->
    lager:debug("invalid URL chain for cdrs request"),
    {Req, cb_context:add_system_error('faulty_request', Context)};
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
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() -> [?HTTP_GET].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_CallId) -> [?HTTP_GET].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /call_inspector => []
%%    /call_inspector/foo => [<<"foo">>]
%%    /call_inspector/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /call_inspector mights load a list of skel objects
%% /call_inspector/123 might load the skel object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_path_and_date_range(Context, cb_context:req_nouns(Context)).

-spec validate_path_and_date_range(cb_context:context(), req_nouns()) -> cb_context:context().
validate_path_and_date_range(Context, [{<<"call_inspector">>, _}, {?KZ_ACCOUNTS_DB, _}|_]) ->
    validate_date_range(Context);
validate_path_and_date_range(Context, [{<<"call_inspector">>, _}, {<<"users">>, [_]}|_]) ->
    validate_date_range(Context);
validate_path_and_date_range(Context, [{<<"call_inspector">>, _}|_]) ->
    lager:debug("invalid URL chain for call_inspector request"),
    cb_context:add_system_error('faulty_request', Context).

-spec validate_date_range(cb_context:context()) -> cb_context:context().
validate_date_range(Context) ->
    case crossbar_view:time_range(Context) of
        {_StartTime, _EndTime} -> cb_context:set_resp_status(Context, 'success');
        Ctx -> Ctx
    end.

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, CallId) ->
    case kz_term:is_empty(CallId) of
        'true' ->
            cb_context:add_system_error('not_found', Context);
        'false' ->
            inspect_call_id(CallId, Context)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec inspect_call_id(ne_binary(), cb_context:context()) -> cb_context:context().
inspect_call_id(CallId, Context) ->
    Req = [{<<"Call-ID">>, CallId}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case kz_amqp_worker:call_collect(Req
                                    ,fun kapi_inspector:publish_lookup_req/1
                                    ,{call_inspector, fun kapi_inspector:lookup_resp_v/1, true}
                                    )
    of
        {ok, [JObj]} ->
            Chunks   = sanitize(kz_json:get_value(<<"Chunks">>, JObj, [])),
            Analysis = sanitize(kz_json:get_value(<<"Analysis">>, JObj, [])),
            Response = kz_json:from_list(
                         [{<<"call-id">>, CallId}
                         ,{<<"messages">>, Chunks}
                         ,{<<"dialog_entities">>, kz_json:get_value(<<"Dialog-Entities">>, JObj, [])}
                         ,{<<"analysis">>, Analysis}
                         ]
                        ),
            crossbar_util:response(Response, Context);
        {timeout, _Resp} ->
            lager:debug("timeout: ~s ~p", [CallId, _Resp]),
            crossbar_util:response_datastore_timeout(Context);
        {error, _E} ->
            lager:debug("error: ~s ~p", [CallId, _E]),
            crossbar_util:response_bad_identifier(CallId, Context)
    end.

%% @private
-spec sanitize(kz_json:objects()) -> kz_json:objects().
sanitize(JObjs) ->
    [kz_json:delete_key(<<"call-id">>, JObj) || JObj <- JObjs].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generate specific view options for the path.
%% @end
%%--------------------------------------------------------------------
-spec get_view_options(req_nouns()) -> {api_ne_binary(), crossbar_view:options()}.
get_view_options([{<<"call_inspector">>, []}, {?KZ_ACCOUNTS_DB, _}|_]) ->
    {?CB_LIST, []};
get_view_options([{<<"call_inspector">>, []}, {<<"users">>, [OwnerId]}|_]) ->
    {?CB_LIST_BY_USER
    ,[{'range_start_keymap', [OwnerId]}
     ,{'range_end_keymap', [OwnerId]}
     ]
    };
get_view_options(_) ->
    {'undefined', []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Loads CDR docs from database and normalized the them.
%% @end
%%--------------------------------------------------------------------
-spec load_chunked_cdrs(cb_cowboy_payload(), kz_json:objects(), ne_binary()) -> crossbar_view:chunked_mapper_ret().
load_chunked_cdrs(Payload, JObjs, Db) ->
    Ids = get_cdr_ids(JObjs),
    cb_cdrs:load_chunked_cdr_ids(Payload, Ids, Db).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_cdr_ids(kz_json:objects()) -> ne_binaries().
get_cdr_ids(JObjs) ->
    Ids = [kz_doc:id(JObj) || JObj <- JObjs],
    %% Remove leading year, month and dash
    CallIds = [CallId || {?MATCH_CDR_ID(_, CallId), _CDR} <- Ids],

    lager:debug("filtering ~p call_ids", [length(CallIds)]),
    FilteredCallIds = filter_callids(CallIds),

    lager:debug("found ~p dialogues", [length(FilteredCallIds)]),

    [Id || ?MATCH_CDR_ID(_, CallId)=Id <- Ids,
           lists:member(CallId, FilteredCallIds)
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Send a filter request to call_inspector application to filter
%% which cdr_id is on call_inspector data store
%% @end
%%--------------------------------------------------------------------
-spec filter_callids(ne_binaries()) -> ne_binaries().
filter_callids([]) -> [];
filter_callids(CallIds) ->
    Req = [{<<"Call-IDs">>, CallIds}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case kz_amqp_worker:call_collect(Req
                                    ,fun kapi_inspector:publish_filter_req/1
                                    ,{call_inspector, fun kapi_inspector:filter_resp_v/1, true}
                                    )
    of
        {ok, JObjs} ->
            FilterIds = fun (JObj) -> kz_json:get_value(<<"Call-IDs">>, JObj, []) end,
            lists:usort(lists:flatmap(FilterIds, JObjs));
        {timeout, JObjs} ->
            lager:debug("timeout ~s", [kz_json:encode(JObjs)]),
            FilterIds = fun (JObj) -> kz_json:get_value(<<"Call-IDs">>, JObj, []) end,
            lists:usort(lists:flatmap(FilterIds, JObjs));
        {error, _E} ->
            lager:debug("error: ~p", [_E]),
            []
    end.
