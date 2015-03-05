%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% Listing of all expected v1 callbacks
%%%
%%% @end
%%% @contributors:
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_call_inspector).

-export([init/0
         ,allowed_methods/0
         ,allowed_methods/1
         ,resource_exists/0
         ,resource_exists/1
         ,validate/1
         ,validate/2
         ,to_json/1
         ,to_csv/1
        ]).

-include("../crossbar.hrl").

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

-spec to_json(cb_cdrs:payload()) -> cb_cdrs:payload().
to_json({Req1, Context}) ->
    Nouns = cb_context:req_nouns(Context),
    lager:debug("Nouns: ~p", [Nouns]),
    case props:get_value(<<"call_inspector">>, Nouns, []) of
        [_|_] -> {Req1, Context};
        [] ->
            Headers = cowboy_req:get('resp_headers', Req1),
            {'ok', Req2} = cowboy_req:chunked_reply(200, Headers, Req1),
            'ok' = cowboy_req:chunk("{\"status\":\"success\", \"data\":[", Req2),
            {Req3, Context1} = send_chunked_cdrs({Req2, Context}),
            'ok' = cowboy_req:chunk("]", Req3),
            _ = cb_cdrs:pagination({Req3, Context1}),
            'ok' = cowboy_req:chunk([",\"request_id\":\"", cb_context:req_id(Context), "\""
                                     ,",\"auth_token\":\"", cb_context:auth_token(Context), "\""
                                     ,"}"
                                    ], Req3),
            'ok' = cowboy_req:ensure_response(Req3, 200),
            {Req3, cb_context:store(Context1, 'is_chunked', 'true')}
    end.

-spec to_csv(cb_cdrs:payload()) -> cb_cdrs:payload().
to_csv({Req, Context}) ->
    Nouns = cb_context:req_nouns(Context),
    lager:debug("Nouns: ~p", [Nouns]),
    case props:get_value(<<"call_inspector">>, Nouns, []) of
        [_|_] -> {Req, Context};
        [] ->
            Headers = props:set_values([{<<"content-type">>, <<"application/octet-stream">>}
                                        ,{<<"content-disposition">>, <<"attachment; filename=\"cdrs.csv\"">>}
                                       ]
                                       ,cowboy_req:get('resp_headers', Req)
                                      ),
            {'ok', Req1} = cowboy_req:chunked_reply(200, Headers, Req),
            Context1 = cb_context:store(Context, 'is_csv', 'true'),
            {Req2, _} = send_chunked_cdrs({Req1, Context1}),
            'ok' = cowboy_req:ensure_response(Req2, 200),
            {Req2, cb_context:store(Context1,'is_chunked', 'true')}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_) ->
    [?HTTP_GET].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /skels => []
%%    /skels/foo => [<<"foo">>]
%%    /skels/foo/bar => [<<"foo">>, <<"bar">>]
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
%% /skels mights load a list of skel objects
%% /skels/123 might load the skel object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    cb_cdrs:validate(Context).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, CallId) ->
    case wh_util:is_empty(CallId) of
        'true' ->
            cb_context:add_system_error('not_found', Context);
        'false' ->
            inspect_call_id(CallId, Context)
    end.

-spec inspect_call_id(ne_binary(), cb_context:context()) -> cb_context:context().
inspect_call_id(CallId, Context) ->
    Req = [{<<"Call-ID">>, CallId}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case wh_amqp_worker:call(Req
                            ,fun wapi_inspector:publish_lookup_req/1
                            ,fun wapi_inspector:lookup_resp_v/1
                            )
    of
        {'ok', JObj} ->
            Response =
                wh_json:from_list(
                  [{<<"messages">>, wh_json:get_value(<<"Chunks">>, JObj)}
                  ,{<<"analysis">>, wh_json:get_value(<<"Analysis">>, JObj)}
                  ]
                 ),
            crossbar_util:response(Response, Context);
        {'timeout', _Resp} ->
            lager:debug("timeout: ~p", [_Resp]),
            crossbar_util:response_datastore_timeout(Context);
        {'error', _E} ->
            lager:debug("error: ~p", [_E]),
            crossbar_util:response_datastore_timeout(Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec send_chunked_cdrs(cb_cdrs:payload()) -> cb_cdrs:payload().
send_chunked_cdrs({Req, Context}) ->
    Dbs = cb_context:fetch(Context, 'chunked_dbs'),
    AuthAccountId = cb_context:auth_account_id(Context),
    IsReseller = wh_services:is_reseller(AuthAccountId),
    send_chunked_cdrs(Dbs, {Req, cb_context:store(Context, 'is_reseller', IsReseller)}).

-spec send_chunked_cdrs(ne_binaries(), cb_cdrs:payload()) -> cb_cdrs:payload().
send_chunked_cdrs([], Payload) -> Payload;
send_chunked_cdrs([Db | Dbs], {Req, Context}) ->
    View = cb_context:fetch(Context, 'chunked_view'),
    ViewOptions = cb_cdrs:fetch_view_options(Context),
    Context1 = cb_context:store(Context, 'start_key', props:get_value('startkey', ViewOptions)),
    Context2 = cb_context:store(Context1, 'page_size', 0),
    Ids = get_cdr_ids(Db, View, ViewOptions),
    lager:debug("IDS: ~p", [Ids]),
    {Context3, CDRIds} = cb_cdrs:maybe_paginate_and_clean(Context2, Ids),
    send_chunked_cdrs(Dbs, cb_cdrs:load_chunked_cdrs(Db, CDRIds, {Req, Context3})).

get_cdr_ids(Db, View, ViewOptions) ->
    {'ok', Ids} = cb_cdrs:get_cdr_ids(Db, View, ViewOptions),
    filter_cdr_ids(Ids).

-spec filter_cdr_ids(ne_binaries()) -> cb_context:context().
filter_cdr_ids(Ids) ->
    Req = [{<<"Call-IDs">>, [CallId || {<<_:7/binary, CallId/binary>>, _} <- Ids]}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case wh_amqp_worker:call(Req
                            ,fun wapi_inspector:publish_filter_req/1
                            ,fun wapi_inspector:filter_resp_v/1
                            )
    of
        {'ok', JObj} ->
            FilteredIds = wh_json:get_value(<<"Call-IDs">>, JObj, []),
            lager:debug("FiltereIDs ~p", [FilteredIds]),
            [Id || {<<_:7/binary, CallId/binary>>, _}=Id <- Ids, lists:member(CallId, FilteredIds)];
        {'timeout', _Resp} ->
            lager:debug("timeout: ~p", [_Resp]),
            [];
        {'error', _E} ->
            lager:debug("error: ~p", [_E]),
            []
    end.
