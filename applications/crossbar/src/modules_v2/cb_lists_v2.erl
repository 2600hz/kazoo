%%%----------------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%% Users module
%%%
%%% Handle client requests for match list documents
%%%
%%% @end
%%% @contributors
%%%   Kozlov Yakov
%%%----------------------------------------------------------------------------
-module(cb_lists_v2).

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
    io:format("init cb_cidlistmatch_v2~n"),
    Bindings =
        [
         {<<"v2_resource.allowed_methods.lists">>, 'allowed_methods'}
         ,{<<"v2_resource.resource_exists.lists">>, 'resource_exists'}
         ,{<<"v2_resource.validate.lists">>, 'validate'}
         ,{<<"v2_resource.execute.put.lists">>, 'put'}
         ,{<<"v2_resource.execute.post.lists">>, 'post'}
         ,{<<"v2_resource.execute.delete.lists">>, 'delete'}
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
validate(Context) ->
    validate_list(Context, cb_context:req_verb(Context)).
validate(Context, ListId) ->
    validate_list(Context, ListId, cb_context:req_verb(Context)).
validate(Context, ListId, EntryId) ->
    validate_list(Context, ListId, EntryId, cb_context:req_verb(Context)).

validate_list(Context, ?HTTP_GET) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2);
validate_list(Context, ?HTTP_PUT) ->
    check_list_schema('undefined', Context).

validate_list(Context, ListId, ?HTTP_GET) ->
    crossbar_doc:load(ListId, Context);
validate_list(Context, ListId, ?HTTP_POST) ->
    check_list_schema(ListId, Context);
validate_list(Context, ListId, ?HTTP_PUT) ->
    check_list_entry_schema(ListId, 'undefined', Context);
validate_list(Context, ListId, ?HTTP_DELETE) ->
    crossbar_doc:load(ListId, Context).

validate_list(Context, ListId, EntryId, ?HTTP_GET) ->
    OnSuccess = fun(C) -> get_entry(C, EntryId) end,
    load_list(Context, ListId, OnSuccess);
validate_list(Context, ListId, EntryId, ?HTTP_POST) ->
    check_list_entry_schema(ListId, EntryId, Context);
validate_list(Context, ListId, EntryId, ?HTTP_DELETE) ->
    OnSuccess = fun(C) -> delete_entry(C, EntryId) end,
    load_list(Context, ListId, OnSuccess).

check_list_schema(ListId, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(ListId, C) end,
    cb_context:validate_request_data(<<"lists">>, Context, OnSuccess).

on_successful_validation(undefined, Context) ->
    Props = [
             {<<"pvt_type">>, <<"list">>}
             ,{<<"entries">>, wh_json:new()}
            ],
    Doc = cb_context:doc(Context),
    NewDoc = wh_json:set_values(Props, Doc),
    cb_context:set_doc(Context, NewDoc);
on_successful_validation(ListId, Context) ->
    crossbar_doc:load_full_merge(ListId, Context).

check_list_entry_schema(ListId, EntryId, Context) ->
    OnSuccess = fun(C) -> on_entry_successful_validation(ListId, EntryId, C) end,
    cb_context:validate_request_data(<<"list_entries">>, Context, OnSuccess).

on_entry_successful_validation(ListId, 'undefined', Context) ->
    EntryData = cb_context:doc(Context),
    OnSuccess = fun(C) -> add_entry(C, EntryData) end,
    load_list(Context, ListId, OnSuccess);
on_entry_successful_validation(ListId, EntryId, Context) ->
    EntryData = cb_context:doc(Context),
    OnSuccess = fun(C) -> update_entry(C, EntryId, EntryData) end,
    load_list(Context, ListId, OnSuccess).

load_list(Context, ListId, OnSuccess) ->
    Context1 = crossbar_doc:load(ListId, Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            OnSuccess(Context1);
        _Status ->
            Context1
    end.

get_entry(Context, EntryId, OnSuccess) ->
    Doc = cb_context:doc(Context),
    case wh_json:get_value([<<"entries">>, EntryId], Doc) of
        undefined ->
            lager:debug("operation on entry ~s failed: not_found", [EntryId, cb_context:account_db(Context)]),
            cb_context:add_system_error('bad_identifier', [{'details', EntryId}],  Context);
        EntryData ->
            OnSuccess(Context, EntryData)
    end.

get_entry(Context, EntryId) ->
    OnSuccess =
        fun(C, EntryData) ->
                C1 = cb_context:store(C, entry_id, EntryId),
                C2 = cb_context:store(C1, entry_jobj, EntryData),
                handle_entry_success(C2)
        end,
    get_entry(Context, EntryId, OnSuccess).

add_entry(Context, EntryData) ->
    Doc = cb_context:doc(Context),
    Uuid = couch_mgr:get_uuid(),
    Doc1 = wh_json:set_value([<<"entries">>, Uuid], EntryData, Doc),
    Context1 = cb_context:set_doc(Context, Doc1),
    Context2 = cb_context:store(Context1, entry_id, Uuid),
    Context3 = cb_context:store(Context2, entry_jobj, EntryData),
    crossbar_doc:save(Context3).

update_entry(Context, EntryId, EntryData) ->
    OnSuccess =
        fun(C, _OldEntryData) ->
                Doc = cb_context:doc(C),
                Doc1 = wh_json:set_value([<<"entries">>, EntryId], EntryData, Doc),
                C1 = cb_context:set_doc(C, Doc1),
                C2 = cb_context:store(C1, entry_id, EntryId),
                C3 = cb_context:store(C2, entry_jobj, EntryData),
                crossbar_doc:save(C3)
        end,
    get_entry(Context, EntryId, OnSuccess).

delete_entry(Context, EntryId) ->
    OnSuccess =
        fun(C, EntryData) ->
                Doc = cb_context:doc(C),
                Doc1 = wh_json:delete_key([<<"entries">>, EntryId], Doc),
                C1 = cb_context:set_doc(C, Doc1),
                C2 = cb_context:store(C1, entry_id, EntryId),
                C3 = cb_context:store(C2, entry_jobj, EntryData),
                crossbar_doc:save(C3)
        end,
    get_entry(Context, EntryId, OnSuccess).

post(Context) ->
    crossbar_doc:save(Context).
post(Context, _ListId) ->
    crossbar_doc:save(Context).
post(Context, _ListId, _EntryId) ->
    handle_entry_success(Context).

put(Context) ->
    crossbar_doc:save(Context).
put(Context, _ListId) ->
    crossbar_doc:save(Context).

delete(Context, _ListId) ->
    crossbar_doc:delete(Context).
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

handle_entry_success(Context) ->
    EntryId = cb_context:fetch(Context, entry_id),
    EntryJObj = cb_context:fetch(Context, entry_jobj),
    EntryJObj2 = wh_json:set_value(<<"id">>, EntryId, EntryJObj),
    ListJObj = cb_context:doc(Context),
    lists:foldl(fun fold_over_setters/2
                ,Context
                ,[{fun cb_context:set_resp_status/2, 'success'}
                  ,{fun cb_context:set_resp_data/2, wh_json:public_fields(EntryJObj2)}
                  ,{fun cb_context:set_resp_etag/2, rev_to_etag(ListJObj)}
                 ]).

-spec fold_over_setters({cb_context:setter_fun(), term()}, cb_context:context()) ->
                               cb_context:context().
fold_over_setters({F, D}, C) -> F(C, D).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will attempt to convert a revision tag on the provided
%% document into a usable ETag for the response
%% @end
%%--------------------------------------------------------------------
-spec rev_to_etag(wh_json:object() | wh_json:objects() | ne_binary()) -> 'undefined' | 'automatic' | string().
rev_to_etag([_|_])-> 'automatic';
rev_to_etag([]) -> 'undefined';
rev_to_etag(Rev) when is_binary(Rev) -> wh_util:to_list(Rev);
rev_to_etag(JObj) ->
    case wh_json:get_value(<<"_rev">>, JObj) of
        'undefined' -> 'undefined';
        Rev -> wh_util:to_list(Rev)
    end.
