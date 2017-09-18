%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz
%%% @doc
%%% Parses HTTP API request parameters for kazoo_modb_view
%%% @end
%%% @contributors
%%%   Roman Galeev
%%%-------------------------------------------------------------------

-module(crossbar_view).
-include("crossbar.hrl").
-export([load/3]).

-define(PAGINATION_PAGE_SIZE, kapps_config:get_integer(?CONFIG_CAT, <<"pagination_page_size">>, 50)).

-spec load(cb_context:context(), ne_binary(), kz_proplist()) -> cb_context:context().
load(Context, ViewName, Options) ->
    ResultMapper = props:get_value(mapper, Options, fun kz_term:identity/1),
    CouchOptions = props:get_value(couch_options, Options, []),
    KeyMap = props:get_value(key_map, Options),
    StartKeyMap = props:get_value(start_key_map, Options),
    EndKeyMap = props:get_value(end_key_map, Options),
    AccountId = cb_context:account_id(Context),
    ModbViewOptions = props:filter_undefined(
                        [{mapper, build_filter_with_qs(Context, ResultMapper)}
                        ,{couch_options, make_unique(build_qs_filter_options(Context) ++ CouchOptions)}
                        ,{key_map, KeyMap}
                        ,{start_key_map, StartKeyMap}
                        ,{end_key_map, EndKeyMap}
                        ]),
    {StartKey, EndKey} = get_range(Context),
    case is_paged(Context) of
        'true' ->
            PageSize = page_size(Context),
            {LastKey, JObjs} = kazoo_modb_view:get_results(AccountId, ViewName, StartKey, EndKey, PageSize, ModbViewOptions),
            format_response(Context, StartKey, LastKey, PageSize, JObjs);
        'false' ->
            JObjs = kazoo_modb_view:get_results(AccountId, ViewName, StartKey, EndKey, ModbViewOptions),
            crossbar_doc:handle_datamgr_success(JObjs, Context)
    end.

-spec is_ascending(cb_context:context()) -> boolean().
is_ascending(Context) ->
    kz_json:is_true(<<"ascending">>, cb_context:query_string(Context)).

-spec is_paged(cb_context:context()) -> boolean().
is_paged(Context) ->
    kz_json:is_true(<<"pagination">>, cb_context:query_string(Context), true).

-spec get_range(cb_context:context()) -> {integer(), integer()}.
get_range(Context) ->
    get_range(Context, is_ascending(Context)).

-spec get_range(cb_context:context(), boolean()) -> {integer(), integer()}.
get_range(Context, _Ascending = true) ->
    StartKey = ascending_start_key(Context),
    EndKey = ascending_end_key(Context, StartKey),
    {StartKey, EndKey};
get_range(Context, _Ascending = false) ->
    StartKey = start_key(Context),
    EndKey = end_key(Context, StartKey),
    {StartKey, EndKey}.

-spec one_of(cb_context:context(), ne_binaries(), integer()) -> integer().
one_of(_, [], Default) -> Default;
one_of(Context, [Value|Values], Default) ->
    case cb_context:req_value(Context, Value) of
        undefined -> one_of(Context, Values, Default);
        ReqValue -> kz_term:to_integer(ReqValue)
    end.

-spec start_key(cb_context:context()) -> integer().
start_key(Context) ->
    Default = kz_time:current_tstamp(),
    one_of(Context, [<<"start_key">>, <<"created_to">>], Default).

-spec end_key(cb_context:context(), integer()) -> integer().
end_key(Context, StartKey) ->
    Default = StartKey - ?MAX_RANGE,
    one_of(Context, [<<"end_key">>, <<"created_from">>], Default).

-spec ascending_start_key(cb_context:context()) -> integer().
ascending_start_key(Context) ->
    Default = kz_time:current_tstamp(),
    one_of(Context, [<<"start_key">>, <<"created_from">>], Default).

-spec ascending_end_key(cb_context:context(), integer()) -> integer().
ascending_end_key(Context, StartKey) ->
    Default = StartKey + ?MAX_RANGE,
    one_of(Context, [<<"end_key">>, <<"created_to">>], Default).

-spec page_size() -> integer().
-spec page_size(cb_context:context()) -> integer().

page_size() -> ?PAGINATION_PAGE_SIZE.
page_size(Context) -> page_size(Context, cb_context:api_version(Context)).
page_size(_Context, ?VERSION_1) -> undefined;
page_size(Context, _Version) ->
    case cb_context:req_value(Context, <<"page_size">>) of
        undefined -> page_size();
        V -> kz_term:to_integer(V)
    end.

-spec add_paging(integer(), integer(), integer(), kz_json:object()) -> kz_json:object().
add_paging(StartKey, PageSize, NextStartKey, JObj) ->
    kz_json:set_values([{<<"start_key">>, StartKey},
                        {<<"page_size">>, PageSize},
                        {<<"next_start_key">>, NextStartKey}
                       ]
                      ,JObj
                      ).

-spec remove_paging(kz_json:object()) -> kz_json:object().
remove_paging(JObj) ->
    kz_json:delete_keys([<<"start_key">>, <<"page_size">>, <<"next_start_key">>], JObj).

-spec format_response(cb_context:context(), pos_integer(), api_integer(), pos_integer(), kz_json:objects()) ->
                             cb_context:context().
format_response(Context, _, undefined, _PageSize, JObjs) ->
    Envelope = remove_paging(cb_context:resp_envelope(Context)),
    crossbar_doc:handle_datamgr_success(JObjs, cb_context:set_resp_envelope(Context, Envelope));
format_response(Context, StartKey, NextStartKey, PageSize, JObjs) ->
    Envelope = add_paging(StartKey, PageSize, NextStartKey, cb_context:resp_envelope(Context)),
    crossbar_doc:handle_datamgr_success(JObjs, cb_context:set_resp_envelope(Context, Envelope)).

-spec build_qs_filter_mapper(cb_context:context()) -> fun((kz_json:object()) -> kz_json:json_term()).
build_qs_filter_mapper(Context) ->
    case crossbar_filter:is_defined(Context) of
        true -> fun(JObjDoc) -> kz_json:get_value(<<"doc">>, JObjDoc) end;
        false -> fun kz_term:identity/1
    end.

-spec build_qs_filter_options(cb_context:context()) -> [include_docs] | [].
build_qs_filter_options(Context) ->
    case crossbar_filter:is_defined(Context) of
        true -> [include_docs];
        false -> []
    end.

-spec build_filter_with_qs(cb_context:context(), fun()) -> filter_fun().
build_filter_with_qs(Context, UserResultMapper) ->
    CtxResultMapper = crossbar_filter:build(Context),
    ResultMapper = build_qs_filter_mapper(Context),
    build_filter_with_qs(ResultMapper, CtxResultMapper, UserResultMapper).

-type filter_fun() :: fun((kz_json:object(), kz_json:objects()) -> kz_json:objects()).
-spec build_filter_with_qs(fun(), fun(), fun()) -> filter_fun().
build_filter_with_qs(ResultMapper, CtxResultMapper, UserResultMapper) when is_function(UserResultMapper, 1) ->
    fun(JObjDoc, Acc) ->
            JObj = ResultMapper(JObjDoc),
            case CtxResultMapper(JObj) of
                false -> Acc;
                true -> [ UserResultMapper(JObjDoc) | Acc ]
            end
    end;
build_filter_with_qs(ResultMapper, CtxResultMapper, UserResultMapper) when is_function(UserResultMapper, 2) ->
    fun(JObjDoc, Acc) ->
            JObj = ResultMapper(JObjDoc),
            case CtxResultMapper(JObj) of
                false -> Acc;
                true -> UserResultMapper(JObjDoc, Acc)
            end
    end.

-spec make_unique(kz_proplist()) -> kz_proplist().
make_unique(Props) ->
    sets:to_list(sets:from_list(Props)).
