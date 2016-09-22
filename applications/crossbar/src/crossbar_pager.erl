-module(crossbar_pager).
-include("crossbar.hrl").
-export([descending/2, descending/3, ascending/2, ascending/3]).

-define(PAGINATION_PAGE_SIZE, kapps_config:get_integer(?CONFIG_CAT, <<"pagination_page_size">>, 50)).
-define(DEFAULT_RANGE, kapps_config:get_integer(?CONFIG_CAT, <<"maximum_range">>, (?SECONDS_IN_DAY * 31 + ?SECONDS_IN_HOUR))).

descending(Context, View) -> descending(Context, View, fun id/1).
ascending(Context, View) -> ascending(Context, View, fun id/1).

descending(Context, View, Filter) ->
    PageSize = page_size(Context),
    StartKey = start_key(Context),
    EndKey = end_key(Context, StartKey),
    AccountId = cb_context:account_id(Context),
    JObjs = cb_pager:descending(AccountId, View, StartKey, EndKey, PageSize+1, build_filter(Context, Filter)),
    format_response(Context, StartKey, PageSize, erlang:length(JObjs), JObjs).

ascending(Context, View, Filter) ->
    PageSize = page_size(Context),
    StartKey = start_key(Context),
    EndKey = ascending_end_key(Context, StartKey),
    AccountId = cb_context:account_id(Context),
    JObjs = cb_pager:ascending(AccountId, View, StartKey, EndKey, PageSize+1, build_filter(Context, Filter)),
    format_response(Context, StartKey, PageSize, erlang:length(JObjs), JObjs).

start_key(Context) ->
    cb_context:req_value(Context, <<"start_key">>, kz_util:current_tstamp()).

end_key(Context, StartKey) ->
    cb_context:req_value(Context, <<"end_key">>, StartKey - ?DEFAULT_RANGE).

ascending_end_key(Context, StartKey) ->
    cb_context:req_value(Context, <<"end_key">>, StartKey + ?DEFAULT_RANGE).

page_size() -> ?PAGINATION_PAGE_SIZE.
page_size(Context) -> page_size(Context, cb_context:api_version(Context)).
page_size(_Context, ?VERSION_1) -> undefined;
page_size(Context, _Version) ->
    case cb_context:req_value(Context, <<"page_size">>) of
        undefined -> page_size();
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

format_response(Context, _, PageSize, ResultSize, JObjs) when ResultSize < PageSize; ResultSize == PageSize ->
    Envelope = remove_paging(cb_context:resp_envelope(Context)),
    cb_context:set_resp_envelope(cb_context:set_doc(Context, JObjs), Envelope);
format_response(Context, StartKey, PageSize, _ResultSize, [LastObj, JObjs]) ->
    NextStartKey = kz_json:get_value(<<"key">>, LastObj),
    Envelope = add_paging(StartKey, PageSize, NextStartKey, cb_context:resp_envelope(Context)),
    cb_context:set_resp_envelope(cb_context:set_doc(Context, JObjs), Envelope).

id(X) -> X.

build_filter(Context, UserFilter) ->
    CtxFilter = crossbar_filter:build(Context),
    build_filter(erlang:fun_info(UserFilter, arity), CtxFilter, UserFilter).

build_filter({arity,1}, CtxFilter, UserFilter) ->
    fun(JObj, Acc) ->
        case CtxFilter(JObj) of
            false -> Acc;
            true -> UserFilter(JObj)
        end
    end;
build_filter({arity,2}, CtxFilter, UserFilter) ->
    fun(JObj, Acc) ->
        case CtxFilter(JObj) of
            false -> Acc;
            true -> UserFilter(JObj, Acc)
        end
    end.
