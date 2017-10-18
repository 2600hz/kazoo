%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% Read only access to ACDC stats docs
%%%
%%% This code is VERY similar to that in cb_cdrs. At some point code
%%% that is used by both should be re-factored into a MODB utility
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

-include("crossbar.hrl").

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".acdc_call_stats">>).
-define(MAX_BULK, kapps_config:get_pos_integer(?MOD_CONFIG_CAT, <<"maximum_bulk">>, 50)).
-define(CB_LIST, <<"call_stats/crossbar_listing">>).

-define(COLUMNS
       ,[{<<"id">>, fun col_id/1}
        ,{<<"handled_timestamp">>, fun col_handled_timestamp/1}
        ,{<<"caller_id_number">>, fun col_caller_id_number/1}
        ,{<<"caller_id_name">>, fun col_caller_id_name/1}
        ,{<<"entered_position">>, fun col_entered_position/1}
        ,{<<"status">>, fun col_status/1}
        ,{<<"agent_id">>, fun col_agent_id/1}
        ,{<<"wait_time">>, fun col_wait_time/1}
        ,{<<"talk_time">>, fun col_talk_time/1}
        ,{<<"queue_id">>, fun col_queue_id/1}
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

-spec to_json(cb_cowboy_payload()) -> cb_cowboy_payload().
to_json({Req, Context}) ->
    Options = [{'is_chunked', 'true'}
              ,{'chunk_size', ?MAX_BULK}
              ,{'cowboy_req', Req}
              ,{'chunked_mapper', fun load_chunked_acdc_stats/2}
              ,{'chunk_response_type', 'json'}
              ,{'mapper', crossbar_view:map_value_fun()}
              ],
    crossbar_view:load_modb(Context, ?CB_LIST, Options).

-spec to_csv(cb_cowboy_payload()) -> cb_cowboy_payload().
to_csv({Req, Context}) ->
    Options = [{'is_chunked', 'true'}
              ,{'chunk_size', ?MAX_BULK}
              ,{'cowboy_req', Req}
              ,{'chunked_mapper', fun load_chunked_acdc_stats/2}
              ,{'chunk_response_type', 'csv'}
              ,{'mapper', crossbar_view:map_value_fun()}
              ],
    crossbar_view:load_modb(cb_context:store(Context, 'is_csv', 'true'), ?CB_LIST, Options).

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
    case crossbar_view:time_range(Context) of
        {_StartTime, _EndTime} -> cb_context:set_resp_status(Context, 'success');
        Ctx -> Ctx
    end;
load_stats_summary(Context, _Nouns) ->
    lager:debug("invalid URL chain for stats summary request"),
    cb_context:add_system_error('faulty_request', Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec load_chunked_acdc_stats(cb_cowboy_payload(), kz_json:objects()) -> crossbar_view:chunked_mapper_ret().
load_chunked_acdc_stats({Req, Context}, JObjs) ->
    case cb_context:fetch(Context, 'is_csv', 'false') of
        'true' ->
            {CSVs, Context1} = lists:foldl(fun normalize_stat_to_csv/2, {[], Context}, JObjs),
            {lists:reverse(CSVs), {Req, Context1}};
        'false' ->
            {[kz_json:from_list([{K, F(JObj)} || {K, F} <- ?COLUMNS]) || JObj <- JObjs], {Req, Context}}
    end.

-spec normalize_stat_to_csv(kz_json:objects(), {binaries(), cb_context:context()}) -> {binaries(), cb_context:context()}.
normalize_stat_to_csv(JObj, {CSVs, Context}) ->
    CSV = kz_binary:join([F(JObj) || {_, F} <- ?COLUMNS], <<",">>),
    case cb_context:fetch(Context, 'started_chunk') of
        'true' ->
            {[<<CSV/binary, "\r\n">>|CSVs], Context};
        _Else ->
            Header = kz_binary:join([K || {K, _Fun} <- ?COLUMNS], <<",">>),
            {[<<Header/binary, "\r\n", CSV/binary, "\r\n">>|CSVs], cb_context:store(Context, 'started_chunk', 'true')}
    end.

col_id(JObj) -> kz_doc:id(JObj, <<>>).
col_handled_timestamp(JObj) -> kz_json:get_value(<<"handled_timestamp">>, JObj, <<>>).
col_caller_id_number(JObj) -> kz_json:get_value(<<"caller_id_number">>, JObj, <<>>).
col_caller_id_name(JObj) -> kz_json:get_value(<<"caller_id_name">>, JObj, <<>>).
col_entered_position(JObj) -> kz_json:get_value(<<"entered_position">>, JObj, <<>>).
col_status(JObj) -> kz_json:get_value(<<"status">>, JObj, <<>>).
col_agent_id(JObj) -> kz_json:get_value(<<"agent_id">>, JObj, <<>>).
col_wait_time(JObj) -> kz_json:get_value(<<"wait_time">>, JObj, <<>>).
col_talk_time(JObj) -> kz_json:get_value(<<"talk_time">>, JObj, <<>>).
col_queue_id(JObj) -> kz_json:get_value(<<"queue_id">>, JObj, <<>>).
