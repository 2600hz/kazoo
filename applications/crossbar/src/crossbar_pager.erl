-module(crossbar_pager).
-include("crossbar.hrl").
-export([descending/2, descending/3, ascending/2, ascending/3]).

-define(PAGINATION_PAGE_SIZE, kapps_config:get_integer(?CONFIG_CAT, <<"pagination_page_size">>, 50)).
-define(DEFAULT_RANGE, kapps_config:get_integer(?CONFIG_CAT, <<"maximum_range">>, (?SECONDS_IN_DAY * 31 + ?SECONDS_IN_HOUR))).

id(X) -> X.

descending(Context, View) -> descending(Context, View, fun id/1).
ascending(Context, View) -> ascending(Context, View, fun id/1).

descending(Context, View, Filter) ->
    PageSize = page_size(Context),
    StartKey = start_key(Context),
    EndKey = end_key(Context, StartKey),
    AccountId = cb_context:account_id(Context),
    CtxFilter = build_filter_with_qs(Context, Filter),
    Options = build_qs_filter_options(Context),
    {LastKey, JObjs} = cb_pager:descending(AccountId, View, StartKey, EndKey, PageSize, CtxFilter, Options),
    format_response(Context, StartKey, LastKey, PageSize, JObjs).

ascending(Context, View, Filter) ->
    PageSize = page_size(Context),
    StartKey = ascending_start_key(Context),
    EndKey = ascending_end_key(Context, StartKey),
    AccountId = cb_context:account_id(Context),
    CtxFilter = build_filter_with_qs(Context, Filter),
    Options = build_qs_filter_options(Context),
    {LastKey, JObjs} = cb_pager:ascending(AccountId, View, StartKey, EndKey, PageSize, CtxFilter, Options),
    format_response(Context, StartKey, LastKey, PageSize, JObjs).

one_of(_, [], Default) -> Default;
one_of(Context, [Value|Values], Default) ->
    case cb_context:req_value(Context, Value) of
        'undefined' -> one_of(Context, Values, Default);
        ReqValue -> kz_util:to_integer(ReqValue)
    end.

start_key(Context) ->
    Default = kz_util:current_tstamp(),
    one_of(Context, [<<"start_key">>, <<"created_to">>], Default).

end_key(Context, StartKey) ->
    Default = StartKey - ?DEFAULT_RANGE,
    one_of(Context, [<<"end_key">>, <<"created_from">>], Default).

ascending_start_key(Context) ->
    Default = kz_util:current_tstamp(),
    one_of(Context, [<<"start_key">>, <<"created_from">>], Default).

ascending_end_key(Context, StartKey) ->
    Default = StartKey + ?DEFAULT_RANGE,
    one_of(Context, [<<"end_key">>, <<"created_to">>], Default).

page_size() -> ?PAGINATION_PAGE_SIZE.
page_size(Context) -> page_size(Context, cb_context:api_version(Context)).
page_size(_Context, ?VERSION_1) -> 'undefined';
page_size(Context, _Version) ->
    case cb_context:req_value(Context, <<"page_size">>) of
        'undefined' -> page_size();
        V -> kz_util:to_integer(V)
    end.

add_paging(StartKey, PageSize, NextStartKey, JObj) ->
    kz_json:set_values([
                        {<<"start_key">>, StartKey},
                        {<<"page_size">>, PageSize},
                        {<<"next_start_key">>, NextStartKey}
                       ],
                       JObj).

remove_paging(JObj) ->
    kz_json:delete_keys([<<"start_key">>, <<"page_size">>, <<"next_start_key">>], JObj).

format_response(Context, _, 'undefined', _PageSize, JObjs) ->
    Envelope = remove_paging(cb_context:resp_envelope(Context)),
    crossbar_doc:handle_datamgr_success(JObjs, cb_context:set_resp_envelope(Context, Envelope));
format_response(Context, StartKey, NextStartKey, PageSize, JObjs) ->
    Envelope = add_paging(StartKey, PageSize, NextStartKey, cb_context:resp_envelope(Context)),
    crossbar_doc:handle_datamgr_success(JObjs, cb_context:set_resp_envelope(Context, Envelope)).

build_qs_filter_mapper(Context) ->
    case crossbar_filter:defined(Context) of
        'true' -> fun(JObjDoc) -> kz_json:get_value(<<"doc">>, JObjDoc) end;
        'false' -> fun id/1
    end.

build_qs_filter_options(Context) ->
    case crossbar_filter:defined(Context) of
        'true' -> ['include_docs'];
        'false' -> []
    end.

build_filter_with_qs(Context, UserFilter) ->
    CtxFilter = crossbar_filter:build(Context),
    Mapper = build_qs_filter_mapper(Context),
    build_filter_with_qs(erlang:fun_info(UserFilter, 'arity'), Mapper, CtxFilter, UserFilter).

build_filter_with_qs({'arity',1}, Mapper, CtxFilter, UserFilter) ->
    fun(JObjDoc, Acc) ->
            JObj = Mapper(JObjDoc),
            case CtxFilter(JObj) of
                'false' -> Acc;
                'true' -> [ UserFilter(JObjDoc) | Acc ]
            end
    end;
build_filter_with_qs({'arity',2}, Mapper, CtxFilter, UserFilter) ->
    fun(JObjDoc, Acc) ->
            JObj = Mapper(JObjDoc),
            case CtxFilter(JObj) of
                'false' -> Acc;
                'true' -> UserFilter(JObjDoc, Acc)
            end
    end.
