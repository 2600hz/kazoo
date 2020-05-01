%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_call_inspector).

-export([init/0
        ,allowed_methods/0, allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,validate/1, validate/2
        ]).

-include("crossbar.hrl").

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".call_inspector">>).
-define(MAX_BULK, kapps_config:get_pos_integer(?MOD_CONFIG_CAT, <<"maximum_bulk">>, 50)).

-define(CB_LIST, <<"cdrs/crossbar_listing">>).
-define(CB_LIST_BY_USER, <<"cdrs/listing_by_owner">>).

-define(MATCH_CDR_ID(YYYYMM, CallId), <<YYYYMM:6/binary, "-", CallId/binary>>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.call_inspector">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.call_inspector">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.to_json.get.call_inspector">>, ?MODULE, 'to_json'),
    _ = crossbar_bindings:bind(<<"*.to_csv.get.call_inspector">>, ?MODULE, 'to_csv'),
    crossbar_bindings:bind(<<"*.validate.call_inspector">>, ?MODULE, 'validate').

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() -> [?HTTP_GET].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_CallId) -> [?HTTP_GET].

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource.
%% For example:
%%
%% ```
%%    /call_inspector => []
%%    /call_inspector/foo => [<<"foo">>]
%%    /call_inspector/foo/bar => [<<"foo">>, <<"bar">>]
%% '''
%% @end
%%------------------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /call_inspector might load a list of skel objects
%% /call_inspector/123 might load the skel object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%------------------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
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
    C1 = cb_context:store(Context
                         ,'is_reseller'
                         ,kz_services_reseller:is_reseller(AuthAccountId)
                         ),
    Options = [{'is_chunked', 'true'}
              ,{'chunk_size', ?MAX_BULK}
              ,{'mapper', fun(JObjs) -> cdrs_listing_mapper(Context, JObjs) end}
              ,'include_docs'
               | Options0
              ],
    crossbar_view:load_modb(cb_cdrs:fix_qs_filter_keys(C1), ViewName, Options).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, CallId) ->
    case kz_term:is_empty(CallId) of
        'true' ->
            cb_context:add_system_error('not_found', Context);
        'false' ->
            inspect_call_id(CallId, Context)
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec inspect_call_id(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
inspect_call_id(CallId, Context) ->
    Req = [{<<"Call-ID">>, CallId}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case kz_amqp_worker:call_collect(Req
                                    ,fun kapi_inspector:publish_lookup_req/1
                                    ,{'call_inspector', fun kapi_inspector:lookup_resp_v/1, 'true'}
                                    )
    of
        {'ok', [JObj]} ->
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
        {'timeout', _Resp} ->
            lager:debug("timeout: ~s ~p", [CallId, _Resp]),
            crossbar_util:response_datastore_timeout(Context);
        {'error', _E} ->
            lager:debug("error: ~s ~p", [CallId, _E]),
            crossbar_util:response_bad_identifier(CallId, Context)
    end.

-spec sanitize(kz_json:objects()) -> kz_json:objects().
sanitize(JObjs) ->
    [kz_json:delete_key(<<"call-id">>, JObj) || JObj <- JObjs].

%%------------------------------------------------------------------------------
%% @doc Generate specific view options for the path.
%% @end
%%------------------------------------------------------------------------------
-spec get_view_options(req_nouns()) -> {kz_term:api_ne_binary(), crossbar_view:options()}.
get_view_options([{<<"call_inspector">>, []}, {?KZ_ACCOUNTS_DB, _}|_]) ->
    {?CB_LIST
    ,[{'range_start_keymap', []}
     ,{'range_end_keymap', crossbar_view:suffix_key_fun([kz_json:new()])}
     ]
    };
get_view_options([{<<"call_inspector">>, []}, {<<"users">>, [OwnerId]}|_]) ->
    {?CB_LIST_BY_USER
    ,[{'range_start_keymap', [OwnerId]}
     ,{'range_end_keymap', fun(Ts) -> [OwnerId, Ts, kz_json:new()] end}
     ]
    };
get_view_options(_) ->
    {'undefined', []}.

%%------------------------------------------------------------------------------
%% @doc Loads CDR docs from database and normalized the them.
%% @end
%%------------------------------------------------------------------------------
-spec cdrs_listing_mapper(cb_context:context(), kz_json:objects()) -> kz_json:objects() | {'error', kz_term:ne_binary()}.
cdrs_listing_mapper(Context, JObjs) ->
    CallIds = [kz_json:get_value([<<"doc">>, <<"call_id">>], JObj) || JObj <- JObjs],

    lager:debug("filtering ~p call_ids", [length(CallIds)]),
    case filter_callids(CallIds) of
        {'ok', FilteredCallIds} ->
            lager:debug("found ~p dialogues", [length(FilteredCallIds)]),

            [cb_cdrs:normalize_cdr(Context, <<"json">>, JObj)
             || JObj <- JObjs,
                lists:member(kz_json:get_value([<<"doc">>, <<"call_id">>], JObj), FilteredCallIds)
            ];
        {'error', _} = Error ->
            Error
    end.

%%------------------------------------------------------------------------------
%% @doc Send a filter request to call_inspector application to filter
%% which cdr_id is on call_inspector data store
%% @end
%%------------------------------------------------------------------------------
-spec filter_callids(kz_term:ne_binaries()) -> {'ok', kz_term:ne_binaries()} |
          {'error', kz_term:ne_binary()}.
filter_callids([]) -> {'ok', []};
filter_callids(CallIds) ->
    Req = [{<<"Call-IDs">>, CallIds}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case kz_amqp_worker:call_collect(Req
                                    ,fun kapi_inspector:publish_filter_req/1
                                    ,{'call_inspector', fun kapi_inspector:filter_resp_v/1, 'true'}
                                    )
    of
        {'ok', JObjs} ->
            FilterIds = fun (JObj) -> kz_json:get_value(<<"Call-IDs">>, JObj, []) end,
            {'ok', lists:usort(lists:flatmap(FilterIds, JObjs))};
        {'timeout', JObjs} ->
            lager:debug("timeout: got ~b response jobj though", [length(JObjs)]),
            {'error', <<"timeout during querying call inspector">>};
        {'error', _Reason} ->
            lager:debug("error: ~p", [_Reason]),
            {'error', <<"unknown error occurred during querying call inspector">>}
    end.
