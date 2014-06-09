%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(crossbar_doc).

-export([load/2, load/3
         ,load_from_file/2
         ,load_merge/2, load_merge/3
         ,merge/3
         ,load_view/3, load_view/4, load_view/5, load_view/6
         ,load_attachment/3, load_docs/2
         ,save/1, save/2
         ,delete/1, delete/2
         ,save_attachment/4, save_attachment/5
         ,delete_attachment/3
         ,ensure_saved/1, ensure_saved/2
         ,rev_to_etag/1
         ,current_doc_vsn/0
         ,update_pvt_parameters/2
         ,start_key/1, start_key/2
         ,pagination_page_size/0, pagination_page_size/1
        ]).

-export([handle_json_success/2]).

-include("crossbar.hrl").

-define(CROSSBAR_DOC_VSN, <<"1">>).
-define(PVT_FUNS, [fun add_pvt_vsn/2
                   ,fun add_pvt_account_id/2
                   ,fun add_pvt_account_db/2
                   ,fun add_pvt_created/2
                   ,fun add_pvt_modified/2
                  ]).

-define(PAGINATION_PAGE_SIZE, whapps_config:get_integer(?CONFIG_CAT
                                                        ,<<"pagination_page_size">>
                                                        ,50
                                                       )).

-spec pagination_page_size() -> pos_integer().
-spec pagination_page_size(cb_context:context()) -> pos_integer().
-spec pagination_page_size(cb_context:context(), ne_binary()) -> pos_integer().
pagination_page_size() ->
    ?PAGINATION_PAGE_SIZE.

pagination_page_size(Context) ->
    pagination_page_size(Context, cb_context:api_version(Context)).

pagination_page_size(_Context, <<"v1">>) -> 'undefined';
pagination_page_size(Context, _Version) ->
    case cb_context:req_value(Context, <<"page_size">>) of
        'undefined' -> pagination_page_size();
        V -> wh_util:to_integer(V)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Returns the version number attached to created/updated documents.
%% Indicates what pvt fields are created/updated when saving.
%%
%% Failure here returns 410, 500, or 503
%% @end
%%--------------------------------------------------------------------
-spec current_doc_vsn() -> ne_binary().
current_doc_vsn() -> ?CROSSBAR_DOC_VSN.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function attempts to load the context with account details,
%% including the account db name and the account doc.
%%
%% Failure here returns 410, 500, or 503
%% @end
%%--------------------------------------------------------------------
-spec load(api_binary() | api_binaries(), cb_context:context()) -> cb_context:context().
-spec load(api_binary() | api_binaries(), cb_context:context(), wh_proplist()) -> cb_context:context().

load(DocId, #cb_context{}=Context) -> load(DocId, Context, []).

load(_, #cb_context{resp_status='error'}=Context, _) -> Context;
load(DocId, #cb_context{}=Context, Opts) when is_binary(DocId) ->
    case couch_mgr:open_cache_doc(cb_context:account_db(Context), DocId, Opts) of
        {'error', Error} ->
            handle_couch_mgr_errors(Error, DocId, Context);
        {'ok', JObj} ->
            lager:debug("loaded doc ~s(~s) from ~s"
                        ,[DocId, wh_json:get_value(<<"_rev">>, JObj), cb_context:account_db(Context)]
                       ),
            case wh_json:is_true(<<"pvt_deleted">>, JObj) of
                'true' -> cb_context:add_system_error('bad_identifier', [{'details', DocId}],  Context);
                'false' -> cb_context:store(handle_couch_mgr_success(JObj, Context), 'db_doc', JObj)
            end
    end;
load([], Context, _) -> cb_context:add_system_error('bad_identifier',  Context);
load([_|_]=IDs, Context, Opts) ->
    Opts1 = [{'keys', IDs}, 'include_docs' | Opts],
    case couch_mgr:all_docs(cb_context:account_db(Context), Opts1) of
        {'error', Error} -> handle_couch_mgr_errors(Error, IDs, Context);
        {'ok', JObjs} -> handle_couch_mgr_success(extract_included_docs(JObjs), Context)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% "Load" is a poor word choice given the other function names...
%% This function creates a crossbar document from the contents of
%% a file.
%%
%% Failure here returns 410, 500, or 503
%% @end
%%--------------------------------------------------------------------
-spec load_from_file(ne_binary(), ne_binary()) ->
                            {'ok', wh_json:object()} |
                            {'error', atom()}.
load_from_file(Db, File) -> couch_mgr:load_doc_from_file(Db, 'crossbar', File).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function attempts to merge the submitted data with the private
%% fields of an existing account document, if successful it will
%% load the context with the account details
%%
%% Failure here returns 410, 500, or 503
%% @end
%%--------------------------------------------------------------------
-spec load_merge(ne_binary(), cb_context:context()) ->
                        cb_context:context().
-spec load_merge(ne_binary(), wh_json:object(), cb_context:context()) ->
                        cb_context:context().

load_merge(DocId, #cb_context{}=Context) ->
    load_merge(DocId, cb_context:doc(Context), Context).

load_merge(DocId, DataJObj, #cb_context{load_merge_bypass='undefined'}=Context) ->
    Context1 = load(DocId, Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            lager:debug("loaded doc ~s(~s), merging"
                        ,[DocId, wh_json:get_value(<<"_rev">>, cb_context:doc(Context1))]
                       ),
            merge(DataJObj, cb_context:doc(Context1), Context1);
        _Status -> Context1
    end;
load_merge(_, _, #cb_context{load_merge_bypass=JObj}=Context) ->
    handle_couch_mgr_success(JObj, Context).

-spec merge(wh_json:object(), wh_json:object(), cb_context:context()) ->
                   cb_context:context().
merge(DataJObj, JObj, Context) ->
    PrivJObj = wh_json:private_fields(JObj),
    handle_couch_mgr_success(wh_json:merge_jobjs(PrivJObj, DataJObj), Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function attempts to load the context with the results of a view
%% run against the accounts database.
%%
%% Failure here returns 500 or 503
%% @end
%%--------------------------------------------------------------------
-spec load_view(ne_binary() | 'all_docs', wh_proplist(), cb_context:context()) ->
                           cb_context:context().
-spec load_view(ne_binary() | 'all_docs', wh_proplist(), cb_context:context(), wh_json:json_term() | filter_fun()) ->
                           cb_context:context().
-spec load_view(ne_binary() | 'all_docs', wh_proplist(), cb_context:context(), wh_json:json_term(), pos_integer()) ->
                           cb_context:context().
-spec load_view(ne_binary() | 'all_docs', wh_proplist(), cb_context:context(), wh_json:json_term(), pos_integer(), filter_fun() | 'undefined') ->
                           cb_context:context().
load_view(View, Options, Context) ->
    load_view(View, Options, Context
                  ,start_key(Options, Context)
                  ,pagination_page_size(Context)
                  ,'undefined'
                 ).

load_view(View, Options, Context, FilterFun) when is_function(FilterFun, 2) ->
    load_view(View, Options, Context
                  ,start_key(Options, Context)
                  ,pagination_page_size(Context)
                  ,FilterFun
                 );
load_view(View, Options, Context, StartKey) ->
    load_view(View, Options, Context, StartKey
                  ,pagination_page_size(Context)
                 ).

load_view(View, Options, Context, StartKey, PageSize) ->
    load_view(View, Options, Context, StartKey, PageSize, 'undefined').

load_view(View, Options, Context, StartKey, PageSize, FilterFun) ->
    Db = cb_context:account_db(Context),

    HasFilter = is_function(FilterFun, 2) orelse has_filter(Context),

    DefaultOptions =
        props:filter_undefined(
          [{'startkey', StartKey}
           ,{'limit', limit_by_page_size(PageSize)}
           | props:delete_keys(['startkey', 'limit'], Options)
          ]),

    ViewOptions =
        case HasFilter of
            'true' -> ['include_docs' | props:delete('include_docs', DefaultOptions)];
            'false' -> DefaultOptions
        end,

    case couch_mgr:get_results(Db, View, ViewOptions) of
        {'error', Error} ->
            handle_couch_mgr_errors(Error, View, Context);
        {'ok', JObjs} ->
            lager:debug("paginating view ~s from ~s, starting at ~p", [View, Db, StartKey]),
            handle_couch_mgr_pagination_success(JObjs
                                                ,Context
                                                ,StartKey
                                                ,PageSize
                                                ,FilterFun
                                                ,cb_context:api_version(Context)
                                               )
    end.

-spec limit_by_page_size(api_binary() | pos_integer()) -> pos_integer().
limit_by_page_size('undefined') -> 'undefined';
limit_by_page_size(N) when is_integer(N) -> N+1;
limit_by_page_size(<<_/binary>> = B) -> limit_by_page_size(wh_util:to_integer(B)).

-spec start_key(cb_context:context()) -> wh_json:json_term() | 'undefined'.
-spec start_key(wh_proplist(), cb_context:context()) -> wh_json:json_term() | 'undefined'.
start_key(Context) ->
    cb_context:req_value(Context, <<"start_key">>).

start_key(Options, Context) ->
    case props:get_value('startkey', Options) of
        'undefined' -> cb_context:req_value(Context, <<"start_key">>);
        StartKey -> StartKey
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function attempts to load the context with the results of all the
%% docs in the supplied Db, with the fold function weeding out those not
%% desired by returning 'undefined' or not adding it to the Accumulator
%%
%% Failure here returns 500 or 503
%% @end
%%--------------------------------------------------------------------
-spec load_docs(cb_context:context(), filter_fun()) -> cb_context:context().
load_docs(#cb_context{}=Context, Filter) when is_function(Filter, 2) ->
    case couch_mgr:all_docs(cb_context:account_db(Context)) of
        {'error', Error} -> handle_couch_mgr_errors(Error, <<"all_docs">>, Context);
        {'ok', JObjs} ->
            Filtered = [JObj
                        || JObj <- lists:foldl(Filter, [], JObjs)
                               ,(not wh_util:is_empty(JObj))
                       ],
            handle_couch_mgr_success(Filtered, Context)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function attempts to load the context with the binary payload
%% stored as an attachment
%%
%% Failure here returns 500 or 503
%% @end
%%--------------------------------------------------------------------
-spec load_attachment(ne_binary() | wh_json:object(), ne_binary(), cb_context:context()) -> cb_context:context().
load_attachment(DocId, AName, #cb_context{}=Context) when is_binary(DocId) ->
    case couch_mgr:fetch_attachment(cb_context:account_db(Context), DocId, AName) of
        {'error', Error} -> handle_couch_mgr_errors(Error, DocId, Context);
        {'ok', AttachBin} ->
            lager:debug("loaded attachment ~s from doc ~s from db ~s"
                        ,[AName, DocId, cb_context:account_db(Context)]
                       ),
            #cb_context{resp_status='success'} = Context1 = load(DocId, Context),
            lists:foldl(fun fold_over_setters/2
                        ,Context1
                        ,[{fun cb_context:set_resp_data/2, AttachBin}
                          ,{fun cb_context:set_resp_etag/2, rev_to_etag(cb_context:doc(Context1))}
                         ])
    end;
load_attachment(Doc, AName, #cb_context{}=Context) ->
    load_attachment(find_doc_id(Doc), AName, Context).

-spec fold_over_setters({cb_context:setter_fun(), term()}, cb_context:context()) ->
                               cb_context:context().
fold_over_setters({F, D}, C) -> F(C, D).

-spec find_doc_id(wh_json:object()) -> api_binary().
find_doc_id(JObj) ->
    case wh_json:get_ne_value(<<"_id">>, JObj) of
        'undefined' -> wh_json:get_ne_value(<<"id">>, JObj);
        DocId -> DocId
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function attempts to save the provided document to the accounts
%% database. The result is loaded into the context record.
%%
%% Failure here returns 500 or 503
%% @end
%%--------------------------------------------------------------------
-spec save(cb_context:context()) ->
                  cb_context:context().
-spec save(cb_context:context(), wh_proplist()) ->
                  cb_context:context().
-spec save(cb_context:context(), wh_json:object() | wh_json:objects(), wh_proplist()) ->
                  cb_context:context().

save(Context) ->
    save(Context, []).
save(Context, Options) ->
    save(Context, cb_context:doc(Context), Options).

save(Context, [], _Options) ->
    lager:debug("no docs to save"),
    cb_context:set_resp_status(Context, 'success');
save(Context, [_|_]=JObjs, Options) ->
    JObjs0 = update_pvt_parameters(JObjs, Context),
    case couch_mgr:save_docs(cb_context:account_db(Context), JObjs0, Options) of
        {'error', Error} ->
            IDs = [wh_json:get_value(<<"_id">>, J) || J <- JObjs],
            handle_couch_mgr_errors(Error, IDs, Context);
        {'ok', JObj1} ->
            _ = [couch_mgr:flush_cache_doc(cb_context:account_db(Context), wh_json:get_value(<<"_id">>, JObj))
                 || JObj <- JObjs
                ],
            Context1 = handle_couch_mgr_success(JObj1, Context),
            provisioner_util:maybe_send_contact_list(Context1)
    end;
save(#cb_context{}=Context, JObj, Options) ->
    JObj0 = update_pvt_parameters(JObj, Context),
    case couch_mgr:save_doc(cb_context:account_db(Context), JObj0, Options) of
        {'error', Error} ->
            DocId = wh_json:get_value(<<"_id">>, JObj0),
            handle_couch_mgr_errors(Error, DocId, Context);
        {'ok', JObj1} ->
            couch_mgr:flush_cache_doc(cb_context:account_db(Context), wh_json:get_value(<<"_id">>, JObj1)),
            Context1 = handle_couch_mgr_success(JObj1, Context),
            provisioner_util:maybe_send_contact_list(Context1)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function attempts to save the provided document to the accounts
%% database. The result is loaded into the context record.
%%
%% Failure here returns 500 or 503
%% @end
%%--------------------------------------------------------------------
-spec ensure_saved(cb_context:context()) ->
                          cb_context:context().
-spec ensure_saved(cb_context:context(), wh_proplist()) ->
                          cb_context:context().
-spec ensure_saved(cb_context:context(), wh_json:object() | wh_json:objects(), wh_proplist()) ->
                          cb_context:context().

ensure_saved(#cb_context{}=Context) -> ensure_saved(Context, []).
ensure_saved(#cb_context{}=Context, Options) ->
    ensure_saved(Context, cb_context:doc(Context), Options).

ensure_saved(#cb_context{}=Context, JObj, Options) ->
    JObj0 = update_pvt_parameters(JObj, Context),
    case couch_mgr:ensure_saved(cb_context:account_db(Context), JObj0, Options) of
        {'error', Error} ->
            DocId = wh_json:get_value(<<"_id">>, JObj0),
            handle_couch_mgr_errors(Error, DocId, Context);
        {'ok', JObj1} ->
            Context1 = handle_couch_mgr_success(JObj1, Context),
            provisioner_util:maybe_send_contact_list(Context1)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Save the Contents as an attachment on the document.
%%
%% Failure here returns 500 or 503
%% @end
%%--------------------------------------------------------------------
-spec save_attachment(ne_binary(), ne_binary(), ne_binary(), cb_context:context()) -> cb_context:context().
save_attachment(DocId, AName, Contents, Context) ->
    save_attachment(DocId, AName, Contents, Context, []).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Save the Contents as an attachment on the document with options
%%
%% Failure here returns 500 or 503
%% @end
%%--------------------------------------------------------------------
-spec save_attachment(ne_binary(), ne_binary(), ne_binary(), cb_context:context(), wh_proplist()) ->
                             cb_context:context().
save_attachment(DocId, AName, Contents, #cb_context{}=Context, Options) ->
    Opts1 = case props:get_value('rev', Options) of
                'undefined' ->
                    {'ok', Rev} = couch_mgr:lookup_doc_rev(cb_context:account_db(Context), DocId),
                    lager:debug("looking up rev for ~s: ~s", [DocId, Rev]),
                    [{'rev', Rev} | Options];
                O -> O
            end,

    case couch_mgr:put_attachment(cb_context:account_db(Context), DocId, AName, Contents, Opts1) of
        {'error', Error} ->
            lager:debug("error putting attachment into ~s: ~p"
                        ,[cb_context:account_db(Context), Error]
                       ),
            handle_couch_mgr_errors(Error, AName, Context);
        {'ok', _Res} ->
            lager:debug("saved attachment ~s to doc ~s to db ~s"
                        ,[AName, DocId, cb_context:account_db(Context)]
                       ),
            {'ok', Rev1} = couch_mgr:lookup_doc_rev(cb_context:account_db(Context), DocId),
            lists:foldl(fun fold_over_setters/2
                        ,Context
                        ,[{fun cb_context:set_doc/2, wh_json:new()}
                          ,{fun cb_context:set_resp_status/2, 'success'}
                          ,{fun cb_context:set_resp_data/2, wh_json:new()}
                          ,{fun cb_context:set_resp_etag/2, rev_to_etag(Rev1)}
                         ])
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will attempt to remove an account document from the
%% account database.  This is preformed as a soft-delete and enforced
%% by the views.  Clean up process remove old data based on the delete
%% flag and last modified date
%%
%% Failure here returns 500 or 503
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context()) -> cb_context:context().
-spec delete(cb_context:context(), 'permanent') -> cb_context:context().

delete(#cb_context{}=Context) ->
    JObj0 = cb_context:doc(Context),
    JObj1 = wh_json:set_value(<<"pvt_deleted">>, 'true', update_pvt_parameters(JObj0, Context)),
    case couch_mgr:save_doc(cb_context:account_db(Context), JObj1) of
        {'error', 'not_found'} -> handle_couch_mgr_success(JObj0, Context);
        {'error', Error} ->
            DocId = wh_json:get_value(<<"_id">>, JObj1),
            handle_couch_mgr_errors(Error, DocId, Context);
        {'ok', _} ->
            lager:debug("deleted ~s from ~s", [wh_json:get_value(<<"_id">>, JObj1)
                                               ,cb_context:account_db(Context)
                                              ]),
            Context1 = handle_couch_mgr_success(JObj1, Context),
            provisioner_util:maybe_send_contact_list(Context1)
    end.

delete(#cb_context{}=Context, 'permanent') ->
    JObj0 = cb_context:doc(Context),
    case couch_mgr:del_doc(cb_context:account_db(Context), JObj0) of
        {'error', 'not_found'} -> handle_couch_mgr_success(JObj0, Context);
        {'error', Error} ->
            DocId = wh_json:get_value(<<"_id">>, JObj0),
            handle_couch_mgr_errors(Error, DocId, Context);
        {'ok', _} ->
            lager:debug("permanently deleted ~s from ~s"
                        ,[wh_json:get_value(<<"_id">>, JObj0), cb_context:account_db(Context)]
                       ),
            Context1 = handle_couch_mgr_success(JObj0, Context),
            provisioner_util:maybe_send_contact_list(Context1)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will attempt to remove an attachment from a document.
%% Unlike the delete function, this is NOT a soft-delete.
%%
%% Failure here returns 500 or 503
%% @end
%%--------------------------------------------------------------------
-spec delete_attachment(ne_binary(), ne_binary(), cb_context:context()) ->
                               cb_context:context().
delete_attachment(DocId, AName, #cb_context{}=Context) ->
    case couch_mgr:delete_attachment(cb_context:account_db(Context), DocId, AName) of
        {'error', 'not_found'} -> handle_couch_mgr_success(wh_json:new(), Context);
        {'error', Error} ->
            lager:debug("failed to delete attachment: ~p", [Error]),
            handle_couch_mgr_errors(Error, AName, Context);
        {'ok', _} ->
            lager:debug("deleted attachment ~s from doc ~s from ~s"
                        ,[AName, DocId, cb_context:account_db(Context)]
                       ),
            handle_couch_mgr_success(wh_json:new(), Context)
    end.

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

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_pagination_envelope_params(cb_context:context(), pos_integer(), pos_integer()) -> cb_context:context().
update_pagination_envelope_params(Context, StartKey, PageSize) ->
    update_pagination_envelope_params(Context, StartKey, PageSize, 'undefined').

-spec update_pagination_envelope_params(cb_context:context(), pos_integer(), pos_integer(), api_binary()) -> cb_context:context().
update_pagination_envelope_params(Context, StartKey, PageSize, NextStartKey) ->
    cb_context:set_resp_envelope(Context
                                 ,wh_json:set_values(
                                    props:filter_undefined(
                                      [{<<"start_key">>, StartKey}
                                       ,{<<"page_size">>, PageSize}
                                       ,{<<"next_start_key">>, NextStartKey}
                                      ])
                                    ,cb_context:resp_envelope(Context)
                                   )).
-spec handle_couch_mgr_pagination_success(wh_json:objects(), cb_context:context(), pos_integer(), pos_integer(), function(), ne_binary()) -> cb_context:context().
handle_couch_mgr_pagination_success(JObjs, Context, 'undefined', _PageSize, FilterFun, <<"v1">>) ->
    handle_couch_mgr_success(apply_filter(FilterFun, JObjs, Context), Context);
handle_couch_mgr_pagination_success(JObjs, Context, _StartKey, 'undefined', FilterFun, <<"v1">>) ->
    handle_couch_mgr_success(apply_filter(FilterFun, JObjs, Context), Context);
handle_couch_mgr_pagination_success([], Context, StartKey, _PageSize, _FilterFun, _Version) ->
    handle_couch_mgr_success([], update_pagination_envelope_params(Context, StartKey, 0));
handle_couch_mgr_pagination_success([_|_]=JObjs, Context, StartKey, PageSize, FilterFun, _Version) ->
    try lists:split(PageSize, JObjs) of
        {Results, []} ->
            lager:debug("no next results"),
            handle_couch_mgr_success(apply_filter(FilterFun, Results, Context)
                                     ,update_pagination_envelope_params(Context, StartKey, PageSize)
                                    );

        {Results, [NextJObj]} ->
            NextStartKey = wh_json:get_value(<<"key">>, NextJObj),
            lager:debug("next start key: ~p", [NextStartKey]),
            handle_couch_mgr_success(apply_filter(FilterFun, Results, Context)
                                     ,update_pagination_envelope_params(Context, StartKey, PageSize, NextStartKey)
                                    )
    catch
        'error':'badarg' ->
            lager:debug("recv less than ~p results", [PageSize]),
            Filtered = apply_filter(FilterFun, JObjs, Context),
            handle_couch_mgr_success(Filtered
                                     ,update_pagination_envelope_params(Context, StartKey, length(Filtered))
                                    )
    end.

-type filter_fun() :: fun((wh_json:object(), wh_json:objects()) -> wh_json:objects()).

-spec apply_filter('undefined' | filter_fun(), wh_json:objects(), cb_context:context()) -> wh_json:objects().
apply_filter(FilterFun, JObjs, _Context) when is_function(FilterFun, 2) ->
    [JObj
     || JObj <- lists:foldl(FilterFun, [], JObjs),
        (not wh_util:is_empty(JObj))
    ];
apply_filter(_, JObjs, Context) ->
    lager:debug("no filter fun, checking if should filter doc"),
    [JObj
     || JObj <- JObjs,
        filter_doc(wh_json:get_value(<<"doc">>, JObj), Context)
    ].


-spec handle_couch_mgr_success(wh_json:object() | wh_json:objects(), cb_context:context()) -> cb_context:context().
handle_couch_mgr_success([], Context) ->
    lists:foldl(fun fold_over_setters/2
                ,Context
                ,[{fun cb_context:set_doc/2, []}
                  ,{fun cb_context:set_resp_status/2, 'success'}
                  ,{fun cb_context:set_resp_data/2, []}
                  ,{fun cb_context:set_resp_etag/2, 'undefined'}
                 ]);
handle_couch_mgr_success([JObj|_]=JObjs, Context) ->
    case wh_json:is_json_object(JObj) of
        'true' -> handle_json_success(JObjs, Context);
        'false' -> handle_thing_success(JObjs, Context)
    end;
handle_couch_mgr_success(JObj, Context) ->
    case wh_json:is_json_object(JObj) of
        'true' -> handle_json_success(JObj, Context);
        'false' -> handle_thing_success(JObj, Context)
    end.

-spec handle_thing_success(any(), cb_context:context()) -> cb_context:context().
handle_thing_success(Thing, Context) ->
    lists:foldl(fun fold_over_setters/2
                ,Context
                ,[{fun cb_context:set_doc/2, Thing}
                  ,{fun cb_context:set_resp_status/2, 'success'}
                  ,{fun cb_context:set_resp_data/2, Thing}
                  ,{fun cb_context:set_resp_etag/2, 'undefined'}
                 ]).

-spec handle_json_success(wh_json:object() | wh_json:objects(), cb_context:context()) -> cb_context:context().
handle_json_success([_|_]=JObjs, #cb_context{req_verb = ?HTTP_PUT}=Context) ->
    RespData = [wh_json:public_fields(JObj)
                || JObj <- JObjs,
                   wh_json:is_false(<<"pvt_deleted">>, JObj, 'true')
               ],
    RespHeaders = [{<<"Location">>, wh_json:get_value(<<"_id">>, JObj)}
                   || JObj <- JObjs
                  ] ++ cb_context:resp_headers(Context),
    lists:foldl(fun fold_over_setters/2
                ,Context
                ,[{fun cb_context:set_doc/2, JObjs}
                  ,{fun cb_context:set_resp_status/2, 'success'}
                  ,{fun cb_context:set_resp_data/2, RespData}
                  ,{fun cb_context:set_resp_etag/2, rev_to_etag(JObjs)}
                  ,{fun cb_context:set_resp_headers/2, RespHeaders}
                 ]);
handle_json_success([_|_]=JObjs, Context) ->
    RespData = [wh_json:public_fields(JObj)
                || JObj <- JObjs,
                   wh_json:is_false(<<"pvt_deleted">>, JObj, 'true')
               ],
    lists:foldl(fun fold_over_setters/2
                ,Context
                ,[{fun cb_context:set_doc/2, JObjs}
                  ,{fun cb_context:set_resp_status/2, 'success'}
                  ,{fun cb_context:set_resp_data/2, RespData}
                  ,{fun cb_context:set_resp_etag/2, rev_to_etag(JObjs)}
                 ]);
handle_json_success(JObj, #cb_context{req_verb = ?HTTP_PUT}=Context) ->
    RespHeaders = [{<<"Location">>, wh_json:get_value(<<"_id">>, JObj)}
                   | cb_context:resp_headers(Context)
                  ],
    lists:foldl(fun fold_over_setters/2
                ,Context
                ,[{fun cb_context:set_doc/2, JObj}
                  ,{fun cb_context:set_resp_status/2, 'success'}
                  ,{fun cb_context:set_resp_data/2, wh_json:public_fields(JObj)}
                  ,{fun cb_context:set_resp_etag/2, rev_to_etag(JObj)}
                  ,{fun cb_context:set_resp_headers/2, RespHeaders}
                 ]);
handle_json_success(JObj, Context) ->
    lists:foldl(fun fold_over_setters/2
                ,Context
                ,[{fun cb_context:set_doc/2, JObj}
                  ,{fun cb_context:set_resp_status/2, 'success'}
                  ,{fun cb_context:set_resp_data/2, wh_json:public_fields(JObj)}
                  ,{fun cb_context:set_resp_etag/2, rev_to_etag(JObj)}
                 ]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_couch_mgr_errors(couch_util:couchbeam_errors(), api_binary() | api_binaries(), cb_context:context()) ->
                                           cb_context:context().
handle_couch_mgr_errors('invalid_db_name', _, #cb_context{}=Context) ->
    lager:debug("datastore ~s not_found", [cb_context:account_db(Context)]),
    cb_context:add_system_error('datastore_missing', [{'details', cb_context:account_db(Context)}], Context);
handle_couch_mgr_errors('db_not_reachable', _DocId, #cb_context{}=Context) ->
    lager:debug("operation on doc ~s from ~s failed: db_not_reachable", [_DocId, cb_context:account_db(Context)]),
    cb_context:add_system_error('datastore_unreachable', Context);
handle_couch_mgr_errors('not_found', DocId, #cb_context{}=Context) ->
    lager:debug("operation on doc ~s from ~s failed: not_found", [DocId, cb_context:account_db(Context)]),
    cb_context:add_system_error('bad_identifier', [{'details', DocId}],  Context);
handle_couch_mgr_errors('conflict', DocId, #cb_context{}=Context) ->
    lager:debug("failed to update doc ~s in ~s: conflicts", [DocId, cb_context:account_db(Context)]),
    cb_context:add_system_error('datastore_conflict', Context);
handle_couch_mgr_errors('invalid_view_name', View, #cb_context{}=Context) ->
    lager:debug("loading view ~s from ~s failed: invalid view", [View, cb_context:account_db(Context)]),
    cb_context:add_system_error('datastore_missing_view', [{'details', wh_util:to_binary(View)}], Context);
handle_couch_mgr_errors(Else, _, Context) ->
    lager:debug("operation failed: ~p", [Else]),
    try wh_util:to_binary(Else) of
        Reason -> cb_context:add_system_error('datastore_fault', [{'details', Reason}], Context)
    catch
        _:_ -> cb_context:add_system_error('datastore_fault', Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is used to update the private timestamps, and db
%% parameters on all crossbar documents
%% @end
%%--------------------------------------------------------------------
-spec update_pvt_parameters(wh_json:object() | wh_json:objects(), cb_context:context()) ->
                                         wh_json:object() | wh_json:objects().
update_pvt_parameters(JObjs, Context) when is_list(JObjs) ->
    [update_pvt_parameters(JObj, Context) || JObj <- JObjs];
update_pvt_parameters(JObj0, Context) ->
    lists:foldl(fun(Fun, JObj) -> Fun(JObj, Context) end, JObj0, ?PVT_FUNS).

-spec add_pvt_vsn(wh_json:object(), cb_context:context()) -> wh_json:object().
add_pvt_vsn(JObj, _) ->
    case wh_json:get_value(<<"pvt_vsn">>, JObj) of
        'undefined' -> wh_json:set_value(<<"pvt_vsn">>, ?CROSSBAR_DOC_VSN, JObj);
        _ -> JObj
    end.

-spec add_pvt_account_db(wh_json:object(), cb_context:context()) -> wh_json:object().
add_pvt_account_db(JObj, Context) ->
    case wh_json:get_value(<<"pvt_account_db">>, JObj) of
        'undefined' ->
            wh_json:set_value(<<"pvt_account_db">>, cb_context:account_db(Context), JObj);
        _Else -> JObj
    end.

-spec add_pvt_account_id(wh_json:object(), cb_context:context()) -> wh_json:object().
add_pvt_account_id(JObj, Context) ->
    case wh_json:get_value(<<"pvt_account_id">>, JObj) of
        'undefined' ->
            wh_json:set_value(<<"pvt_account_id">>, cb_context:account_id(Context), JObj);
        _Else -> JObj
    end.

-spec add_pvt_created(wh_json:object(), cb_context:context()) -> wh_json:object().
add_pvt_created(JObj, _) ->
    case wh_json:get_value(<<"_rev">>, JObj) of
        'undefined' ->
            Timestamp = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
            wh_json:set_value(<<"pvt_created">>, Timestamp, JObj);
        _ ->
            JObj
    end.

-spec add_pvt_modified(wh_json:object(), cb_context:context()) -> wh_json:object().
add_pvt_modified(JObj, _) ->
    Timestamp = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    wh_json:set_value(<<"pvt_modified">>, Timestamp, JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec extract_included_docs(wh_json:objects()) -> wh_json:objects().
extract_included_docs(JObjs) ->
    [wh_json:get_value(<<"doc">>, JObj) || JObj <- JObjs].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given a context or query parameter json object determines if the
%% request has a filter defined
%% @end
%%--------------------------------------------------------------------
-spec has_filter(cb_context:context()) -> boolean().
has_filter(Context) ->
    lists:any(fun is_filter_key/1, wh_json:to_proplist(cb_context:query_string(Context))).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given a proplist element from the query string, determines if it
%% represents a filter parameter
%% @end
%%--------------------------------------------------------------------
-spec is_filter_key({binary(), term()}) -> boolean().
is_filter_key({<<"filter_", _/binary>>, _}) -> 'true';
is_filter_key({<<"has_key", _/binary>>, _}) -> 'true';
is_filter_key({<<"key_missing", _/binary>>, _}) -> 'true';
is_filter_key({<<"has_value", _/binary>>, _}) -> 'true';
is_filter_key({<<"created_from">>, _}) -> 'true';
is_filter_key({<<"created_to">>, _}) -> 'true';
is_filter_key({<<"modified_from">>, _}) -> 'true';
is_filter_key({<<"modified_to">>, _}) -> 'true';
is_filter_key(_) -> 'false'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns 'true' if all of the requested props are found, 'false' if one is not found
%% @end
%%--------------------------------------------------------------------
-spec filter_doc(wh_json:object(), cb_context:context()) -> boolean().
filter_doc('undefined', _Context) -> 'true';
filter_doc(Doc, Context) ->
    wh_json:all(fun({K, V}) -> filter_prop(Doc, K, V) end
                ,cb_context:query_string(Context)
               ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns 'true' or 'false' if the prop is found inside the doc
%% @end
%%--------------------------------------------------------------------
-spec filter_prop(wh_json:object(), ne_binary(), term()) -> boolean().
filter_prop(Doc, <<"filter_not_", Key/binary>>, Val) ->
    not (wh_json:get_binary_value(binary:split(Key, <<".">>), Doc, <<>>) =:= wh_util:to_binary(Val));
filter_prop(Doc, <<"filter_", Key/binary>>, Val) ->
    wh_json:get_binary_value(binary:split(Key, <<".">>), Doc, <<>>) =:= wh_util:to_binary(Val);
filter_prop(Doc, <<"has_key">>, Key) ->
    wh_json:get_value(binary:split(Key, <<".">>), Doc) =/= 'undefined';
filter_prop(Doc, <<"key_missing">>, Key) ->
    wh_json:get_value(binary:split(Key, <<".">>), Doc) =:= 'undefined';
filter_prop(Doc, <<"has_value">>, Key) ->
    wh_json:get_ne_value(binary:split(Key, <<".">>), Doc) =/= 'undefined';
filter_prop(Doc, <<"created_from">>, Val) ->
    wh_util:to_integer(wh_json:get_value(<<"pvt_created">>, Doc)) >= wh_util:to_integer(Val);
filter_prop(Doc, <<"created_to">>, Val) ->
    wh_util:to_integer(wh_json:get_value(<<"pvt_created">>, Doc)) =< wh_util:to_integer(Val);
filter_prop(Doc, <<"modified_from">>, Val) ->
    wh_util:to_integer(wh_json:get_value(<<"pvt_modified">>, Doc)) >= wh_util:to_integer(Val);
filter_prop(Doc, <<"modified_to">>, Val) ->
    wh_util:to_integer(wh_json:get_value(<<"pvt_modified">>, Doc)) =< wh_util:to_integer(Val);
filter_prop(_, _, _) ->
    'true'.
%% ADD Unit Tests for private/public field filtering and merging
