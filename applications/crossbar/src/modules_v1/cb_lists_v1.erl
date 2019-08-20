%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Match list module
%%% Handle client requests for match list documents
%%%
%%%
%%% @author Kozlov Yakov
%%% @author SIPLABS, LLC (Maksim Krzhemenevskiy, Ilya Ashchepkov)
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_lists_v1).

-export([init/0
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2
        ,resource_exists/0, resource_exists/1, resource_exists/2
        ,validate/1, validate/2, validate/3
        ,post/1, post/2, post/3
        ,put/1, put/2
        ,delete/2, delete/3
        ]).

-include("crossbar.hrl").

-define(CB_LIST, <<"lists/crossbar_listing">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
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

%%------------------------------------------------------------------------------
%% @doc This function determines the verbs that are appropriate for the
%% given Nouns. For example `/accounts/' can only accept `GET' and `PUT'.
%%
%% Failure here returns `405 Method Not Allowed'.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_ListId) ->
    [?HTTP_GET, ?HTTP_PUT, ?HTTP_POST, ?HTTP_DELETE].

-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(_ListId, _EntryId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

%%------------------------------------------------------------------------------
%% @doc This function determines if the provided list of Nouns are valid.
%% Failure here returns `404 Not Found'.
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_ListId) -> 'true'.

-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists(_ListId, _EntryId) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400.
%% @end
%%------------------------------------------------------------------------------

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_lists(Context, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, ListId) ->
    validate_list(Context, ListId, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, ListId, EntryId) ->
    validate_list_entry(Context, ListId, EntryId, cb_context:req_verb(Context)).

-spec validate_lists(cb_context:context(), http_method()) -> cb_context:context().
validate_lists(Context, ?HTTP_GET) ->
    load_lists(Context);
validate_lists(Context, ?HTTP_PUT) ->
    check_list_schema('undefined', Context).

-spec validate_list(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_list(Context, ListId, ?HTTP_GET) ->
    load_list(Context, ListId);
validate_list(Context, ListId, ?HTTP_POST) ->
    check_list_schema(ListId, Context);
validate_list(Context, ListId, ?HTTP_PUT) ->
    check_list_entry_schema(ListId, 'undefined', Context);
validate_list(Context, ListId, ?HTTP_DELETE) ->
    cb_lists_v2:validate(Context, ListId).

-spec validate_list_entry(cb_context:context(), path_token(), path_token(), http_method()) -> cb_context:context().
validate_list_entry(Context, _ListId, EntryId, ?HTTP_GET) ->
    crossbar_doc:load(EntryId, Context, ?TYPE_CHECK_OPTION(<<"list_entry">>));
validate_list_entry(Context, ListId, EntryId, ?HTTP_POST) ->
    check_list_entry_schema(ListId, EntryId, Context);
validate_list_entry(Context, _ListId, EntryId, ?HTTP_DELETE) ->
    crossbar_doc:load(EntryId, Context, ?TYPE_CHECK_OPTION(<<"list_entry">>)).

-spec check_list_schema(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
check_list_schema(ListId, Context) ->
    case cb_context:req_value(Context, <<"entries">>) of
        'undefined' ->
            OnSuccess = fun(C) -> on_successful_validation(ListId, C) end,
            NewReq = kz_json:filter(fun filter_list_req_data/1
                                   ,cb_context:req_data(Context)
                                   ),
            NewContext = cb_context:set_req_data(Context, NewReq),
            cb_context:validate_request_data(<<"lists">>, NewContext, OnSuccess);
        _ ->
            crossbar_util:response('error', <<"API changed: entries not acceptable">>, 406, Context)
    end.

-spec filter_list_req_data({kz_term:ne_binary(), any()}) -> boolean().
filter_list_req_data({Key, _Val})
  when Key =:= <<"name">>;
       Key =:= <<"org">>;
       Key =:= <<"description">>
       -> 'true';
filter_list_req_data(_) -> 'false'.

-spec on_successful_validation(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    Props = [{<<"pvt_type">>, <<"list">>}],
    cb_context:set_doc(Context
                      ,kz_json:set_values(Props, cb_context:doc(Context))
                      );
on_successful_validation(ListId, Context) ->
    crossbar_doc:load_merge(ListId, Context, ?TYPE_CHECK_OPTION(<<"list">>)).

-spec check_list_entry_schema(path_token(), kz_term:api_binary(), cb_context:context()) -> cb_context:context().
check_list_entry_schema(ListId, EntryId, Context) ->
    OnSuccess = fun(C) -> entry_schema_success(C, ListId, EntryId) end,
    ReqData = kz_json:set_value(<<"list_id">>, ListId, cb_context:req_data(Context)),
    cb_context:validate_request_data(<<"list_entries">>, cb_context:set_req_data(Context, ReqData), OnSuccess).

-spec entry_schema_success(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary()) -> cb_context:context().
entry_schema_success(Context, ListId, EntryId) ->
    Pattern = kz_json:get_value(<<"pattern">>, cb_context:doc(Context)),
    case is_binary(Pattern)
        andalso re:compile(Pattern)
    of
        {'ok', _CompiledRe} ->
            on_entry_successful_validation(ListId, EntryId, Context);
        'false' ->
            on_entry_successful_validation(ListId, EntryId, Context);
        {'error', {Reason0, Pos}} ->
            Reason = io_lib:format("Error: ~s in position ~p", [Reason0, Pos]),
            error_in_pattern(Context, Pattern, Reason)
    end.

-spec error_in_pattern(cb_context:context(), binary(), iolist()) ->
                              cb_context:context().
error_in_pattern(Context, Pattern, Reason) ->
    cb_context:add_validation_error(<<"pattern">>
                                   ,<<"type">>
                                   ,kz_json:from_list(
                                      [{<<"message">>, iolist_to_binary(Reason)}
                                      ,{<<"cause">>, Pattern}
                                      ])
                                   ,Context
                                   ).

-spec on_entry_successful_validation(path_token(), path_token() | 'undefined', cb_context:context()) ->
                                            cb_context:context().
on_entry_successful_validation(_ListId, 'undefined', Context) ->
    cb_context:set_doc(Context
                      ,kz_json:set_values([{<<"pvt_type">>, <<"list_entry">>}]
                                         ,cb_context:doc(Context)
                                         )
                      );
on_entry_successful_validation(_ListId, EntryId, Context) ->
    crossbar_doc:load_merge(EntryId, Context, ?TYPE_CHECK_OPTION(<<"list_entry">>)).

-spec post(cb_context:context()) -> cb_context:context().
post(Context) ->
    crossbar_doc:save(Context).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _ListId) ->
    crossbar_doc:save(Context).

-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
post(Context, _ListId, _EntryId) ->
    crossbar_doc:save(Context).

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(Context).

-spec put(cb_context:context(), path_token()) -> cb_context:context().
put(Context, _ListId) ->
    crossbar_doc:save(Context).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, ListId) ->
    cb_lists_v2:delete(Context, ListId).

-spec delete(cb_context:context(), path_token(), path_token()) -> cb_context:context().
delete(Context, _ListId, _EntryId) ->
    crossbar_doc:delete(Context).

%%------------------------------------------------------------------------------
%% @doc Normalizes the results of a view.
%% @end
%%------------------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) ->
                                    kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj)|Acc].

-spec load_lists(cb_context:context()) -> cb_context:context().
load_lists(Context) ->
    Entries = cb_context:doc(crossbar_doc:load_view(<<"lists/entries">>
                                                   ,[]
                                                   ,cb_context:set_query_string(Context, kz_json:new())
                                                   ,fun normalize_view_results/2)),
    crossbar_doc:load_view(?CB_LIST
                          ,[]
                          ,Context
                          ,load_entries_and_normalize(Entries)).

-spec load_entries_and_normalize(kz_json:objects()) -> fun((kz_json:object(), kz_json:objects()) -> boolean()).
load_entries_and_normalize(AllEntries) ->
    fun(JObj, Acc) ->
            Val = kz_json:get_value(<<"value">>, JObj),
            ListId = kz_json:get_value(<<"id">>, Val),
            ListEntries = lists:filter(filter_entries_by_list_id(ListId), AllEntries),
            [kz_json:set_value(<<"entries">>, entries_from_list(ListEntries), Val)|Acc]
    end.

-spec filter_entries_by_list_id(kz_term:ne_binary()) -> fun((kz_json:object()) -> boolean()).
filter_entries_by_list_id(Id) ->
    fun(Entry) ->
            kz_json:get_value(<<"list_id">>, Entry) =:= Id
    end.

-spec entries_from_list(kz_json:objects()) -> kz_json:object().
entries_from_list(Entries) ->
    kz_json:from_list([{kz_json:get_value(<<"_id">>, X), kz_doc:public_fields(X)}
                       || X <- Entries
                      ]).

-spec load_list(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
load_list(Context, ListId) ->
    Entries = cb_context:doc(crossbar_doc:load_view(<<"lists/entries">>
                                                   ,[{'key', ListId}]
                                                   ,Context
                                                   ,fun normalize_view_results/2)),
    Context1 = crossbar_doc:load(ListId, Context, ?TYPE_CHECK_OPTION(<<"list">>)),
    Doc = cb_context:doc(Context1),
    Doc1 = kz_doc:public_fields(kz_json:set_value(<<"entries">>, entries_from_list(Entries), Doc)),
    cb_context:set_resp_data(Context1, Doc1).
