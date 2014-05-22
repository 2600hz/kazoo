%%%----------------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%% Match list module
%%%
%%% Handle client requests for match list documents
%%%
%%% @end
%%% @contributors
%%%   Kozlov Yakov
%%%   SIPLABS LLC (Maksim Krzhemenevskiy)
%%%----------------------------------------------------------------------------
-module(cb_lists).

-export([init/0
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2
         ,resource_exists/0, resource_exists/1, resource_exists/2
         ,validate/1, validate/2, validate/3
         ,post/1, post/2, post/3
         ,put/1, put/2
         ,delete/2, delete/3
        ]).

-include("../crossbar.hrl").

-define(CB_LIST, <<"lists/crossbar_listing">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    Bindings =
        [
         {<<"*.allowed_methods.lists">>, 'allowed_methods'}
         ,{<<"*.resource_exists.lists">>, 'resource_exists'}
         ,{<<"*.validate.lists">>, 'validate'}
         ,{<<"*.execute.put.lists">>, 'put'}
         ,{<<"*.execute.post.lists">>, 'post'}
         ,{<<"*.execute.delete.lists">>, 'delete'}
        ],
    lists:foreach(
      fun({Binding, F}) ->
              crossbar_bindings:bind(Binding, ?MODULE, F)
      end,
      Bindings).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].
allowed_methods(_ListId) ->
    [?HTTP_GET, ?HTTP_PUT, ?HTTP_POST, ?HTTP_DELETE].
allowed_methods(_ListId, _EntryId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_ListId) -> 'true'.
resource_exists(_ListId, _EntryId) -> 'true'.

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
    validate_lists(Context, cb_context:req_verb(Context)).
validate(Context, ListId) ->
    validate_list(Context, ListId, cb_context:req_verb(Context)).
validate(Context, ListId, EntryId) ->
    validate_list_entry(Context, ListId, EntryId, cb_context:req_verb(Context)).

-spec validate_lists(cb_context:context(), http_method()) -> cb_context:context().
validate_lists(Context, ?HTTP_GET) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2);
validate_lists(Context, ?HTTP_PUT) ->
    check_list_schema('undefined', Context).

-spec validate_list(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_list(Context, ListId, ?HTTP_GET) ->
    crossbar_doc:load(ListId, Context);
validate_list(Context, ListId, ?HTTP_POST) ->
    check_list_schema(ListId, Context);
validate_list(Context, ListId, ?HTTP_PUT) ->
    check_list_entry_schema(ListId, 'undefined', Context);
validate_list(Context, ListId, ?HTTP_DELETE) ->
    crossbar_doc:load(ListId, Context).

-spec validate_list_entry(cb_context:context(), path_token(), path_token(), http_method()) -> cb_context:context().
validate_list_entry(Context, ListId, EntryId, ?HTTP_GET) ->
    OnSuccess = fun(C) -> get_entry(C, EntryId) end,
    load_list(Context, ListId, OnSuccess);
validate_list_entry(Context, ListId, EntryId, ?HTTP_POST) ->
    check_list_entry_schema(ListId, EntryId, Context);
validate_list_entry(Context, ListId, EntryId, ?HTTP_DELETE) ->
    OnSuccess = fun(C) -> delete_entry(C, EntryId) end,
    load_list(Context, ListId, OnSuccess).

-spec check_list_schema(atom() | path_token(), cb_context:context()) -> cb_context:context().
check_list_schema(ListId, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(ListId, C) end,
    Req = cb_context:req_data(Context),
    NewReq = wh_json:filter(fun({Key, Val}) ->
        Key =:= <<"listname">> orelse Key =:= <<"entries">>
    end, Req),
    NewContext = cb_context:set_req_data(Context, NewReq),
    cb_context:validate_request_data(<<"lists">>, NewContext, OnSuccess).

-spec on_successful_validation(atom() | path_token(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    Props = [
             {<<"pvt_type">>, <<"list">>}
            ],
    Doc = cb_context:doc(Context),
    NewDoc = wh_json:set_values(Props, Doc),
    cb_context:set_doc(Context, NewDoc);
on_successful_validation(ListId, Context) ->
    crossbar_doc:load_merge(ListId, Context).

-spec check_list_entry_schema(path_token(), path_token(), cb_context:context()) -> cb_context:context().
check_list_entry_schema(ListId, EntryId, Context) ->
    OnSuccess =
        fun(C) ->
                EntryJObj = cb_context:doc(C),
                Pattern = wh_json:get_value(<<"pattern">>, EntryJObj),
                case re:compile(Pattern) of
                    {ok, _CompiledRe} ->
                        on_entry_successful_validation(ListId, EntryId, C);
                    {error, {Reason0, Pos}} ->
                        Reason = io_lib:format("Error: ~s in position ~p", [Reason0, Pos]),
                        BinReason = iolist_to_binary(Reason),
                        cb_context:add_validation_error(
                          <<"pattern">>, <<"type">>, BinReason, C)
                end
        end,
    cb_context:validate_request_data(<<"list_entries">>, Context, OnSuccess).

-spec on_entry_successful_validation(
    path_token(),
    path_token() | atom(),
    cb_context:context()
) -> cb_context:context().
on_entry_successful_validation(ListId, 'undefined', Context) ->
    EntryData = cb_context:doc(Context),
    OnSuccess = fun(C) -> add_entry(C, EntryData) end,
    load_list(Context, ListId, OnSuccess);
on_entry_successful_validation(ListId, EntryId, Context) ->
    EntryData = cb_context:doc(Context),
    OnSuccess = fun(C) -> update_entry(C, EntryId, EntryData) end,
    load_list(Context, ListId, OnSuccess).

-spec load_list(
    cb_context:context(),
    path_token(),
    fun((cb_context:context()) -> cb_context:context())
) -> cb_context:context().
load_list(Context, ListId, OnSuccess) ->
    Context1 = crossbar_doc:load(ListId, Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            OnSuccess(Context1);
        _Status ->
            Context1
    end.

-spec get_entry(
    cb_context:context(),
    ne_binary(),
    fun((cb_context:context(),ne_binary()) -> cb_context:context())
) -> cb_context:context().
get_entry(Context, EntryId, OnSuccess) ->
    Doc = cb_context:doc(Context),
    case wh_json:get_value([<<"entries">>, EntryId], Doc) of
        undefined ->
            lager:debug("operation on entry ~s failed: not_found", [EntryId, cb_context:account_db(Context)]),
            cb_context:add_system_error('bad_identifier', [{'details', EntryId}],  Context);
        EntryData ->
            OnSuccess(Context, EntryData)
    end.

-spec get_entry(cb_context:context(), path_token()) -> cb_context:context().
get_entry(Context, EntryId) ->
    OnSuccess =
        fun(C, EntryData) ->
             NewContext = cb_context:setters(C, [
                 {fun cb_context:store/3, 'entry_id', EntryId}
                 ,{fun cb_context:store/3, 'entry_jobj', EntryData}
             ]),
             handle_entry_success(NewContext)
        end,
    get_entry(Context, EntryId, OnSuccess).

-spec add_entry(cb_context:context(), any()) -> cb_context:context().
add_entry(Context, EntryData) ->
    Doc = cb_context:doc(Context),
    UUID = couch_mgr:get_uuid(),
    Doc1 = wh_json:set_value([<<"entries">>, UUID], EntryData, Doc),
    NewContext = cb_context:setters(Context, [
        {fun cb_context:set_doc/2, Doc1}
        ,{fun cb_context:store/3, 'entry_id', UUID}
        ,{fun cb_context:store/3, 'entry_jobj', EntryData}
    ]),
    crossbar_doc:save(NewContext).

-spec update_entry(cb_context:context(), path_token(), any()) -> cb_context:context().
update_entry(Context, EntryId, EntryData) ->
    OnSuccess =
        fun(C, _OldEntryData) ->
                Doc = cb_context:doc(C),
                Doc1 = wh_json:set_value([<<"entries">>, EntryId], EntryData, Doc),
                NewContext = cb_context:setters(C, [
                    {fun cb_context:set_doc/2, Doc1}
                    ,{fun cb_context:store/3, 'entry_id', EntryId}
                    ,{fun cb_context:store/3, 'entry_jobj', EntryData}
                ]),
                crossbar_doc:save(NewContext)
        end,
    get_entry(Context, EntryId, OnSuccess).

-spec delete_entry(cb_context:context(), path_token()) -> cb_context:context().
delete_entry(Context, EntryId) ->
    OnSuccess =
        fun(C, EntryData) ->
                Doc = cb_context:doc(C),
                Doc1 = wh_json:delete_key([<<"entries">>, EntryId], Doc),
                NewContext = cb_context:setters(C, [
                    {fun cb_context:set_doc/2, Doc1}
                    ,{fun cb_context:store/3, 'entry_id', EntryId}
                    ,{fun cb_context:store/3, 'entry_jobj', EntryData}
                ]),
                crossbar_doc:save(NewContext)
        end,
    get_entry(Context, EntryId, OnSuccess).

-spec post(cb_context:context()) -> cb_context:context().
post(Context) ->
    crossbar_doc:save(Context).
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _ListId) ->
    crossbar_doc:save(Context).
-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
post(Context, _ListId, _EntryId) ->
    handle_entry_success(Context).

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(Context).
-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context, _ListId) ->
    crossbar_doc:save(Context).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _ListId) ->
    crossbar_doc:delete(Context).
-spec delete(cb_context:context(), path_token(), path_token()) -> cb_context:context().
delete(Context, _ListId, _EntryId) ->
    handle_entry_success(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(wh_json:object(), wh_json:objects()) ->
                                    wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

-spec handle_entry_success(cb_context:context()) -> cb_context:context().
handle_entry_success(Context) ->
    EntryId = cb_context:fetch(Context, 'entry_id'),
    EntryJObj = cb_context:fetch(Context, 'entry_jobj'),
    EntryJObj2 = wh_json:set_value(<<"id">>, EntryId, EntryJObj),
    ListJObj = cb_context:doc(Context),
    cb_context:setters(
        Context,
        [
            {fun cb_context:set_resp_status/2, 'success'},
            {fun cb_context:set_resp_data/2, wh_json:public_fields(EntryJObj2)},
            {fun cb_context:set_resp_etag/2, crossbar_doc:rev_to_etag(ListJObj)}
        ]
    ).
