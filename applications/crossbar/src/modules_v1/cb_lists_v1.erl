%%%----------------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%% Match list module
%%%
%%% Handle client requests for match list documents
%%%
%%% @end
%%% @contributors
%%%   Kozlov Yakov
%%%   SIPLABS, LLC (Maksim Krzhemenevskiy, Ilya Ashchepkov)
%%%----------------------------------------------------------------------------
-module(cb_lists_v1).

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
-spec init() -> any().
init() ->
    [crossbar_bindings:bind(Binding, ?MODULE, F)
     || {Binding, F} <- [{<<"v1_resource.allowed_methods.lists">>, 'allowed_methods'}
                         ,{<<"v1_resource.resource_exists.lists">>, 'resource_exists'}
                         ,{<<"v1_resource.validate.lists">>, 'validate'}
                         ,{<<"v1_resource.execute.put.lists">>, 'put'}
                         ,{<<"v1_resource.execute.post.lists">>, 'post'}
                         ,{<<"v1_resource.execute.delete.lists">>, 'delete'}
                        ]
    ].

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
    load_lists(fetch_reduce_limit(), Context);
validate_lists(Context, ?HTTP_PUT) ->
    check_list_schema('undefined', Context).

-spec validate_list(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_list(Context, ListId, ?HTTP_GET) ->
    load_list(fetch_reduce_limit(), Context, ListId);
validate_list(Context, ListId, ?HTTP_POST) ->
    check_list_schema(ListId, Context);
validate_list(Context, ListId, ?HTTP_PUT) ->
    check_list_entry_schema(ListId, 'undefined', Context);
validate_list(Context, ListId, ?HTTP_DELETE) ->
    cb_lists_v2:validate(Context, ListId).

-spec validate_list_entry(cb_context:context(), path_token(), path_token(), http_method()) -> cb_context:context().
validate_list_entry(Context, _ListId, EntryId, ?HTTP_GET) ->
    crossbar_doc:load(EntryId, Context);
validate_list_entry(Context, ListId, EntryId, ?HTTP_POST) ->
    check_list_entry_schema(ListId, EntryId, Context);
validate_list_entry(Context, _ListId, EntryId, ?HTTP_DELETE) ->
    crossbar_doc:load(EntryId, Context).

-spec check_list_schema(api_binary(), cb_context:context()) -> cb_context:context().
check_list_schema(ListId, Context) ->
    case cb_context:req_value(Context, <<"entries">>) of
        'undefined' ->
            OnSuccess = fun(C) -> on_successful_validation(ListId, C) end,
            NewReq = wh_json:filter(fun filter_list_req_data/1
                                    ,cb_context:req_data(Context)
                                   ),
            NewContext = cb_context:set_req_data(Context, NewReq),
            cb_context:validate_request_data(<<"lists">>, NewContext, OnSuccess);
        _ ->
            crossbar_util:response('error', <<"API changed: entries not acceptable">>, 406, Context)
    end.

-spec filter_list_req_data({ne_binary(), _}) -> boolean().
filter_list_req_data({Key, _Val})
  when Key =:= <<"name">>;
       Key =:= <<"org">>;
       Key =:= <<"description">>
       -> 'true';
filter_list_req_data(_) -> 'false'.

-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    Props = [{<<"pvt_type">>, <<"list">>}],
    cb_context:set_doc(Context
                       ,wh_json:set_values(Props, cb_context:doc(Context))
                      );
on_successful_validation(ListId, Context) ->
    crossbar_doc:load_merge(ListId, Context).

-spec check_list_entry_schema(path_token(), api_binary(), cb_context:context()) -> cb_context:context().
check_list_entry_schema(ListId, EntryId, Context) ->
    OnSuccess = fun(C) -> entry_schema_success(C, ListId, EntryId) end,
    ReqData = wh_json:set_value(<<"list">>, ListId, cb_context:req_data(Context)),
    cb_context:validate_request_data(<<"list_entries">>, cb_context:set_req_data(Context, ReqData), OnSuccess).

-spec entry_schema_success(cb_context:context(), ne_binary(), ne_binary()) -> cb_context:context().
entry_schema_success(Context, ListId, EntryId) ->
    Pattern = wh_json:get_value(<<"pattern">>, cb_context:doc(Context)),
    case re:compile(Pattern) of
        {'ok', _CompiledRe} ->
            on_entry_successful_validation(ListId, EntryId, Context);
        {'error', {Reason0, Pos}} ->
            Reason = io_lib:format("Error: ~s in position ~p", [Reason0, Pos]),
            cb_context:add_validation_error(
                <<"pattern">>
                ,<<"type">>
                ,wh_json:from_list([
                    {<<"message">>, iolist_to_binary(Reason)}
                    ,{<<"cause">>, Pattern}
                ])
                ,Context
            )
    end.

-spec on_entry_successful_validation(path_token(), path_token() | 'undefined', cb_context:context()) ->
                                            cb_context:context().
on_entry_successful_validation(_ListId, 'undefined', Context) ->
    cb_context:set_doc(Context
                       ,wh_json:set_values([{<<"pvt_type">>, <<"list_entry">>}]
                                           ,cb_context:doc(Context)));
on_entry_successful_validation(_ListId, EntryId, Context) ->
    crossbar_doc:load_merge(EntryId, Context).

-spec post(cb_context:context()) -> cb_context:context().
-spec post(cb_context:context(), path_token()) -> cb_context:context().
-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
post(Context) ->
    crossbar_doc:save(Context).

post(Context, _ListId) ->
    crossbar_doc:save(Context).

post(Context, _ListId, _EntryId) ->
    crossbar_doc:save(Context).

-spec put(cb_context:context()) -> cb_context:context().
-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(Context).

put(Context, _ListId) ->
    crossbar_doc:save(Context).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, ListId) ->
    cb_lists_v2:delete(Context, ListId).

-spec delete(cb_context:context(), path_token(), path_token()) -> cb_context:context().
delete(Context, _ListId, _EntryId) ->
    crossbar_doc:delete(Context).

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

-spec load_lists(boolean(), cb_context:context()) -> cb_context:context().
load_lists(false, Context) ->
    crossbar_doc:load_view(?CB_LIST
                           ,[{'group_level', 1}]
                           ,Context
                           ,fun normalize_view_results/2);
load_lists(true, Context) ->
    Entries = cb_context:doc(crossbar_doc:load_view(<<"lists/entries">>
                                                    ,[]
                                                    ,Context
                                                    ,fun normalize_view_results/2)),
    crossbar_doc:load_view(<<"lists/crossbar_listing_v2">>
                           ,[]
                           ,Context
                           ,load_entries_and_normalize(Entries)).

-spec load_entries_and_normalize(wh_json:objects()) -> fun((wh_json:object(), wh_json:objects()) -> boolean()).
load_entries_and_normalize(AllEntries) ->
    fun(JObj, Acc) ->
            Val = wh_json:get_value(<<"value">>, JObj),
            ListId = wh_json:get_value(<<"id">>, Val),
            ListEntries = wh_json:from_list(
                            [{wh_json:get_value(<<"_id">>, X), wh_json:public_fields(X)}
                             || X <- lists:filter(filter_entries_by_list_id(ListId), AllEntries)]),
            [wh_json:set_value(<<"entries">>, ListEntries, Val)|Acc]
    end.

-spec filter_entries_by_list_id(ne_binary()) -> fun((wh_json:object()) -> boolean()).
filter_entries_by_list_id(Id) ->
    fun(Entry) ->
            wh_json:get_value(<<"list_id">>, Entry) =:= Id
    end.

-spec load_list(boolean(), cb_context:context(), ne_binary()) -> cb_context:context().
load_list(false, Context, ListId) ->
    crossbar_doc:load_view(?CB_LIST
                           ,[{'group_level', 1}, {'key', ListId}]
                           ,Context
                           ,fun normalize_view_results/2);
load_list(true, Context, ListId) ->
    Entries = cb_context:doc(crossbar_doc:load_view(<<"lists/entries">>
                                                    ,[]
                                                    ,Context
                                                    ,fun normalize_view_results/2)),
    crossbar_doc:load_view(<<"lists/crossbar_listing_v2">>
                           ,[{'key', ListId}]
                           ,Context
                           ,load_entries_and_normalize(Entries)).

-spec fetch_reduce_limit() -> boolean().
fetch_reduce_limit() ->
    {'ok', Config} = couch_mgr:open_doc(<<"_config">>, <<"query_server_config">>),
    wh_util:to_boolean(wh_json:get_binary_boolean(<<"reduce_limit">>, Config)).
