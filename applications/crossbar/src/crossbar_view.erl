%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Roman Galeev
%%%-------------------------------------------------------------------

-module(crossbar_view).
-include("crossbar.hrl").
-export([load/3, load/5]).

-define(PAGINATION_PAGE_SIZE, kapps_config:get_integer(?CONFIG_CAT, <<"pagination_page_size">>, 50)).

-spec load(cb_context:context(), ViewName :: ne_binary(), kz_proplist()) -> cb_context:context().
load(Context, View, Options) ->
    Mapper = props:get_value('mapper', Options, fun id/1),
    CouchOptions = props:get_value('couch_options', Options, []),
    KeyMap = props:get_value('keymap', Options, fun id/1),
    load(Context, View, CouchOptions, Mapper, map_keymap(KeyMap)).

-spec map_keymap(ne_binary() | [ne_binary()] | fun()) -> fun().
map_keymap(K) when is_binary(K) -> fun(Ts) -> [K, Ts] end;
map_keymap(K) when is_list(K) -> fun(Ts) -> K ++ [Ts] end;
map_keymap(K) when is_function(K) -> K.

-spec load(cb_context:context(), ViewName :: ne_binary(), kz_proplist(), Mapper :: fun(), KeyMap :: fun()) -> cb_context:context().
load(Context, View, CouchOptions, Mapper, KeyMap) when is_function(KeyMap) ->
    case is_ascending(Context) of
        'true' ->
            get_results_ascending(Context, View, CouchOptions, Mapper, KeyMap);
        'false' ->
            get_results_descending(Context, View, CouchOptions, Mapper, KeyMap)
    end.

%% impl

-spec id(any()) -> any().
id(X) -> X.

-spec get_results_descending(cb_context:context(), ne_binary(), kz_proplist(), fun(), fun()) -> cb_context:context().
get_results_descending(Context, View, CouchOptions, Mapper, KeyMap) ->
    PageSize = page_size(Context),
    StartKey = KeyMap(start_key(Context)),
    EndKey = KeyMap(end_key(Context, StartKey)),
    AccountId = cb_context:account_id(Context),
    CtxMapper = build_filter_with_qs(Context, Mapper),
    Options = [{mapper, CtxMapper}, {couch_options, make_unique(build_qs_filter_options(Context) ++ CouchOptions)}],
    {LastKey, JObjs} = kazoo_modb_view:get_results(AccountId, View, StartKey, EndKey, PageSize, Options),
    format_response(Context, StartKey, LastKey, PageSize, JObjs).

-spec get_results_ascending(cb_context:context(), ne_binary(), kz_proplist(), fun(), fun()) -> cb_context:context().
get_results_ascending(Context, View, CouchOptions, Mapper, KeyMap) ->
    PageSize = page_size(Context),
    StartKey = KeyMap(ascending_start_key(Context)),
    EndKey = KeyMap(ascending_end_key(Context, StartKey)),
    AccountId = cb_context:account_id(Context),
    CtxMapper = build_filter_with_qs(Context, Mapper),
    Options = [{mapper, CtxMapper}, {couch_options, make_unique(build_qs_filter_options(Context) ++ CouchOptions)}],
    {LastKey, JObjs} = kazoo_modb_view:get_results(AccountId, View, StartKey, EndKey, PageSize, Options),
    format_response(Context, StartKey, LastKey, PageSize, JObjs).

-spec is_ascending(cb_context:context()) -> boolean().
is_ascending(Context) ->
    kz_json:is_true(<<"ascending">>, cb_context:query_string(Context)).

-spec one_of(cb_context:context(), [ne_binary()], integer()) -> integer().
one_of(_, [], Default) -> Default;
one_of(Context, [Value|Values], Default) ->
    case cb_context:req_value(Context, Value) of
        'undefined' -> one_of(Context, Values, Default);
        ReqValue -> kz_util:to_integer(ReqValue)
    end.

-spec start_key(cb_context:context()) -> integer().
start_key(Context) ->
    Default = kz_util:current_tstamp(),
    one_of(Context, [<<"start_key">>, <<"created_to">>], Default).

-spec end_key(cb_context:context(), integer()) -> integer().
end_key(Context, StartKey) ->
    Default = StartKey - ?MAX_RANGE,
    one_of(Context, [<<"end_key">>, <<"created_from">>], Default).

-spec ascending_start_key(cb_context:context()) -> integer().
ascending_start_key(Context) ->
    Default = kz_util:current_tstamp(),
    one_of(Context, [<<"start_key">>, <<"created_from">>], Default).

-spec ascending_end_key(cb_context:context(), integer()) -> integer().
ascending_end_key(Context, StartKey) ->
    Default = StartKey + ?MAX_RANGE,
    one_of(Context, [<<"end_key">>, <<"created_to">>], Default).

-spec page_size() -> integer().
-spec page_size(cb_context:context()) -> integer().

page_size() -> ?PAGINATION_PAGE_SIZE.
page_size(Context) -> page_size(Context, cb_context:api_version(Context)).
page_size(_Context, ?VERSION_1) -> 'undefined';
page_size(Context, _Version) ->
    case cb_context:req_value(Context, <<"page_size">>) of
        'undefined' -> page_size();
        V -> kz_util:to_integer(V)
    end.

-spec add_paging(integer(), integer(), integer(), kz_json:object()) -> kz_json:object().
add_paging(StartKey, PageSize, NextStartKey, JObj) ->
    kz_json:set_values([
                        {<<"start_key">>, StartKey},
                        {<<"page_size">>, PageSize},
                        {<<"next_start_key">>, NextStartKey}
                       ],
                       JObj).

-spec remove_paging(kz_json:object()) -> kz_json:object().
remove_paging(JObj) ->
    kz_json:delete_keys([<<"start_key">>, <<"page_size">>, <<"next_start_key">>], JObj).

-spec format_response(cb_context:context(), integer(), integer()|'undefined', integer(), kz_json:objects()) -> cb_context:context().
format_response(Context, _, 'undefined', _PageSize, JObjs) ->
    Envelope = remove_paging(cb_context:resp_envelope(Context)),
    crossbar_doc:handle_datamgr_success(JObjs, cb_context:set_resp_envelope(Context, Envelope));
format_response(Context, StartKey, NextStartKey, PageSize, JObjs) ->
    Envelope = add_paging(StartKey, PageSize, NextStartKey, cb_context:resp_envelope(Context)),
    crossbar_doc:handle_datamgr_success(JObjs, cb_context:set_resp_envelope(Context, Envelope)).

-spec build_qs_filter_mapper(kz_json:object()) -> fun().
build_qs_filter_mapper(Context) ->
    case crossbar_filter:is_defined(Context) of
        'true' -> fun(JObjDoc) -> kz_json:get_value(<<"doc">>, JObjDoc) end;
        'false' -> fun id/1
    end.

-spec build_qs_filter_options(kz_json:object()) -> list().
build_qs_filter_options(Context) ->
    case crossbar_filter:is_defined(Context) of
        'true' -> ['include_docs'];
        'false' -> []
    end.

-spec build_filter_with_qs(cb_context:context(), fun()) -> fun().
build_filter_with_qs(Context, UserMapper) ->
    CtxMapper = crossbar_filter:build(Context),
    Mapper = build_qs_filter_mapper(Context),
    build_filter_with_qs(Mapper, CtxMapper, UserMapper).

-spec build_filter_with_qs(fun(), fun(), fun()) -> fun().
build_filter_with_qs(Mapper, CtxMapper, UserMapper) when is_function(UserMapper, 1) ->
    fun(JObjDoc, Acc) ->
            JObj = Mapper(JObjDoc),
            case CtxMapper(JObj) of
                'false' -> Acc;
                'true' -> [ UserMapper(JObjDoc) | Acc ]
            end
    end;
build_filter_with_qs(Mapper, CtxMapper, UserMapper) when is_function(UserMapper, 2) ->
    fun(JObjDoc, Acc) ->
            JObj = Mapper(JObjDoc),
            case CtxMapper(JObj) of
                'false' -> Acc;
                'true' -> UserMapper(JObjDoc, Acc)
            end
    end.

-spec make_unique(kz_proplist()) -> kz_proplist().
make_unique(Props) ->
    sets:to_list(sets:from_list(Props)).
