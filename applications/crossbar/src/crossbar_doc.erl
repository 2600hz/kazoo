%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz
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
         ,patch_and_validate/3
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
         ,has_qs_filter/1
         ,filtered_doc_by_qs/3
        ]).

-export([handle_json_success/2]).
-export([handle_couch_mgr_success/2
         ,handle_couch_mgr_errors/3
        ]).

-ifdef(TEST).
-export([filter_doc_by_querystring/2]).
-endif.

-export_type([view_options/0
              ,startkey/0
             ]).

-include("crossbar.hrl").

-define(CROSSBAR_DOC_VSN, <<"1">>).
-define(PVT_FUNS, [fun add_pvt_vsn/2
                   ,fun add_pvt_account_id/2
                   ,fun add_pvt_account_db/2
                   ,fun add_pvt_created/2
                   ,fun add_pvt_modified/2
                   ,fun add_pvt_request_id/2
                  ]).

-define(PAGINATION_PAGE_SIZE
        ,whapps_config:get_integer(?CONFIG_CAT
                                   ,<<"pagination_page_size">>
                                   ,50
                                  )
       ).

-type direction() :: 'ascending' | 'descending'.

-type startkey() :: wh_json:json_term() | 'undefined'.

-type startkey_fun() :: 'undefined' |
                        fun((cb_context:context()) -> startkey()) |
                        fun((wh_proplist(), cb_context:context()) -> startkey()).

-type view_options() :: couch_util:view_options() |
                        [{'databases', ne_binaries()} |
                         {'startkey_fun', startkey_fun()}
                        ].

-record(load_view_params, {view :: api_binary()
                           ,view_options = [] :: view_options()
                           ,context :: cb_context:context()
                           ,start_key :: startkey()
                           ,page_size :: non_neg_integer() | api_binary()
                           ,filter_fun :: filter_fun()
                           ,dbs = [] :: ne_binaries()
                           ,direction = 'ascending' :: direction()
                          }).
-type load_view_params() :: #load_view_params{}.

-spec pagination_page_size() -> pos_integer().
-spec pagination_page_size(cb_context:context()) -> 'undefined' | pos_integer().
-spec pagination_page_size(cb_context:context(), ne_binary()) -> 'undefined' | pos_integer().
pagination_page_size() ->
    ?PAGINATION_PAGE_SIZE.

pagination_page_size(Context) ->
    pagination_page_size(Context, cb_context:api_version(Context)).

pagination_page_size(_Context, ?VERSION_1) -> 'undefined';
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
-spec load(api_binary() | api_binaries(), cb_context:context()) ->
                  cb_context:context().
-spec load(api_binary() | api_binaries(), cb_context:context(), wh_proplist()) ->
                  cb_context:context().
-spec load(api_binary() | api_binaries(), cb_context:context(), wh_proplist(), crossbar_status()) ->
                  cb_context:context().

load(DocId, Context) ->
    load(DocId, Context, []).

load(DocId, Context, Options) ->
    load(DocId, Context, Options, cb_context:resp_status(Context)).

load(_DocId, Context, _Options, 'error') -> Context;
load(DocId, Context, Opts, _RespStatus) when is_binary(DocId) ->
    OpenFun = case props:get_is_true('use_cache', Opts, 'true') of
                  'true' ->  fun couch_mgr:open_cache_doc/3;
                  'false' -> fun couch_mgr:open_doc/3
              end,
    case OpenFun(cb_context:account_db(Context), DocId, Opts) of
        {'error', Error} ->
            handle_couch_mgr_errors(Error, DocId, Context);
        {'ok', JObj} ->
            handle_successful_load(Context, JObj)
    end;
load([], Context, _Options, _RespStatus) ->
    cb_context:add_system_error('bad_identifier',  Context);
load([_|_]=IDs, Context, Opts, _RespStatus) ->
    Opts1 = [{'keys', IDs}, 'include_docs' | Opts],
    case couch_mgr:all_docs(cb_context:account_db(Context), Opts1) of
        {'error', Error} -> handle_couch_mgr_errors(Error, IDs, Context);
        {'ok', JObjs} ->
            Docs = extract_included_docs(JObjs),
            cb_context:store(handle_couch_mgr_success(Docs, Context)
                             ,'db_doc'
                             ,Docs
                            )
    end.

-spec handle_successful_load(cb_context:context(), wh_json:object()) -> cb_context:context().
-spec handle_successful_load(cb_context:context(), wh_json:object(), boolean()) -> cb_context:context().
handle_successful_load(Context, JObj) ->
    handle_successful_load(Context, JObj, wh_doc:is_soft_deleted(JObj)).

handle_successful_load(Context, JObj, 'true') ->
    lager:debug("doc ~s(~s) is soft-deleted, returning bad_identifier"
                ,[wh_doc:id(JObj), wh_doc:revision(JObj)]
               ),
    cb_context:add_system_error('bad_identifier'
                                ,wh_json:from_list([{<<"cause">>, wh_doc:id(JObj)}])
                                ,Context
                               );
handle_successful_load(Context, JObj, 'false') ->
    lager:debug("loaded doc ~s(~s) from ~s"
                ,[wh_doc:id(JObj), wh_doc:revision(JObj), cb_context:account_db(Context)]
               ),
    cb_context:store(handle_couch_mgr_success(JObj, Context), 'db_doc', JObj).

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
load_from_file(Db, File) ->
    couch_mgr:load_doc_from_file(Db, 'crossbar', File).

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
-spec load_merge(ne_binary(), wh_json:object(), cb_context:context(), api_object()) ->
                        cb_context:context().

load_merge(DocId, Context) ->
    load_merge(DocId, cb_context:doc(Context), Context).

load_merge(DocId, DataJObj, Context) ->
    load_merge(DocId, DataJObj, Context, cb_context:load_merge_bypass(Context)).

load_merge(DocId, DataJObj, Context, 'undefined') ->
    Context1 = load(DocId, Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            lager:debug("loaded doc ~s(~s), merging"
                        ,[DocId, wh_json:get_value(<<"_rev">>, cb_context:doc(Context1))]
                       ),
            merge(DataJObj, cb_context:doc(Context1), Context1);
        _Status -> Context1
    end;
load_merge(_DocId, _DataJObj, Context, BypassJObj) ->
    handle_couch_mgr_success(BypassJObj, Context).

-spec merge(wh_json:object(), wh_json:object(), cb_context:context()) ->
                   cb_context:context().
merge(DataJObj, JObj, Context) ->
    PrivJObj = wh_json:private_fields(JObj),
    handle_couch_mgr_success(wh_json:merge_jobjs(PrivJObj, DataJObj), Context).

-type validate_fun() :: fun((ne_binary(), cb_context:context()) -> cb_context:context()).

-spec patch_and_validate(ne_binary(), cb_context:context(), validate_fun()) ->
                                cb_context:context().
patch_and_validate(Id, Context, ValidateFun) ->
    Context1 = crossbar_doc:load(Id, Context),
    Context2 = case cb_context:resp_status(Context1) of
                   'success' ->
                       PubJObj = wh_doc:public_fields(cb_context:req_data(Context)),
                       PatchedJObj = wh_json:merge_jobjs(PubJObj, cb_context:doc(Context1)),
                       cb_context:set_req_data(Context, PatchedJObj);
                   _Status ->
                       Context1
               end,
    ValidateFun(Id, Context2).

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
    load_view(#load_view_params{view=View
                                ,view_options=Options
                                ,context=cb_context:set_doc(Context, [])
                                ,start_key=StartKey
                                ,page_size=PageSize
                                ,filter_fun=FilterFun
                                ,dbs=lists:filter(fun couch_mgr:db_exists/1
                                                  ,props:get_value('databases', Options, [cb_context:account_db(Context)])
                                                 )
                                ,direction=view_sort_direction(Options)
                               }).

-spec view_sort_direction(wh_proplist()) -> direction().
view_sort_direction(Options) ->
    case props:get_value('descending', Options) of
        'true' -> 'descending';
        'undefined' -> 'ascending'
    end.

load_view(#load_view_params{dbs=[]
                            ,context=Context
                           }) ->
    case cb_context:resp_status(Context) of
        'success' ->
            lager:debug("databases exhausted"),
            handle_couch_mgr_success(cb_context:doc(Context), Context);
        _Status -> Context
    end;
load_view(#load_view_params{page_size=PageSize
                            ,context=Context
                           }) when PageSize =< 0 ->
    lager:debug("page_size exhausted: ~p", [PageSize]),
    case cb_context:resp_status(Context) of
        'success' -> handle_couch_mgr_success(cb_context:doc(Context), Context);
        _Status -> Context
    end;
load_view(#load_view_params{view=View
                            ,view_options=Options
                            ,context=Context
                            ,start_key=StartKey
                            ,page_size=PageSize
                            ,dbs=[Db|Dbs]
                            ,direction=_Direction
                           }=LVPs) ->
    Limit = limit_by_page_size(Context, PageSize),

    lager:debug("limit: ~p page_size: ~p dir: ~p", [Limit, PageSize, _Direction]),

    DefaultOptions =
        props:filter_undefined(
          [{'startkey', StartKey}
           ,{'limit', Limit}
           | props:delete_keys(['startkey', 'limit'], Options)
          ]),

    IncludeOptions =
        case has_qs_filter(Context) of
            'true' -> props:insert_value('include_docs', DefaultOptions);
            'false' -> DefaultOptions
        end,

    ViewOptions =
        case props:get_first_defined(['reduce', 'group', 'group_level'], IncludeOptions) of
            'undefined' -> IncludeOptions;
            'false' -> IncludeOptions;
            _V -> props:delete('include_docs', IncludeOptions)
        end,

    lager:debug("couch_mgr:get_results(~s, ~s, ~p)", [Db, View, ViewOptions]),

    case couch_mgr:get_results(Db, View, ViewOptions) of
        % There were more dbs, so move to the next one
        {'error', 'not_found'} ->
            lager:debug("either the db ~s or view ~s was not found", [Db, View]),
            load_view(LVPs#load_view_params{dbs=Dbs});
        {'error', Error} ->
            handle_couch_mgr_errors(Error, View, Context);
        {'ok', JObjs} ->
            lager:debug("paginating view '~s' from '~s', starting at '~p'", [View, Db, StartKey]),
            handle_couch_mgr_pagination_success(JObjs
                                                ,if is_integer(Limit) -> PageSize; 'true' -> Limit end
                                                ,cb_context:api_version(Context)
                                                ,LVPs#load_view_params{dbs=Dbs
                                                                       ,context=cb_context:set_resp_status(Context, 'success')
                                                                      }
                                               )
    end.

-spec limit_by_page_size(api_binary() | pos_integer()) ->
                                'undefined' | pos_integer().
-spec limit_by_page_size(cb_context:context(), api_binary() | pos_integer()) ->
                                'undefined' | pos_integer().
-spec limit_by_page_size(cb_context:context(), api_binary() | pos_integer(), api_boolean()) ->
                                'undefined' | pos_integer().
limit_by_page_size('undefined') -> 'undefined';
limit_by_page_size(N) when is_integer(N) -> N+1;
limit_by_page_size(<<_/binary>> = B) -> limit_by_page_size(wh_util:to_integer(B)).

limit_by_page_size(Context, PageSize) ->
    case cb_context:api_version(Context) of
        ?VERSION_1 ->
            lager:debug("pagination not supported in this version"),
            'undefined';
        _Version ->
            limit_by_page_size(Context, PageSize, cb_context:req_value(Context, <<"paginate">>))
    end.

limit_by_page_size(Context, PageSize, 'undefined') ->
    lager:debug("pagination enabled by default, checking filters"),
    maybe_disable_page_size(Context, PageSize);
limit_by_page_size(_Context, PageSize, ShouldEnable) ->
    case wh_util:is_true(ShouldEnable) of
        'true' ->
            lager:debug("pagination explicitly enabled, getting page size from ~p", [PageSize]),
            limit_by_page_size(PageSize);
        'false' ->
            lager:debug("pagination disabled by request or version"),
            'undefined'
    end.

-spec maybe_disable_page_size(cb_context:context(), pos_integer() | api_binary()) ->
                                     'undefined' | pos_integer().
maybe_disable_page_size(Context, PageSize) ->
    case has_qs_filter(Context) of
        'true' ->
            lager:debug("request has a query string filter, disabling pagination"),
            'undefined';
        'false' ->
            lager:debug("no query string filter, getting page size from ~p", [PageSize]),
            limit_by_page_size(PageSize)
    end.

-spec start_key(cb_context:context()) -> wh_json:json_term() | 'undefined'.
-spec start_key(wh_proplist(), cb_context:context()) -> wh_json:json_term() | 'undefined'.
start_key(Context) ->
    cb_context:req_value(Context, <<"start_key">>).

start_key(Options, Context) ->
    case props:get_value('startkey_fun', Options) of
        'undefined' -> start_key_fun(Options, Context);
        Fun when is_function(Fun, 2) -> Fun(Options, Context);
        Fun when is_function(Fun, 1) -> Fun(Context)
    end.

-spec start_key_fun(wh_proplist(), cb_context:context()) -> wh_json:json_term() | 'undefined'.
start_key_fun(Options, Context) ->
    case props:get_value('startkey', Options) of
        'undefined' ->
            lager:debug("getting start_key from request: ~p", [ cb_context:req_value(Context, <<"start_key">>)]),
            cb_context:req_value(Context, <<"start_key">>);
        StartKey ->
            lager:debug("getting start_key from options: ~p", [StartKey]),
            StartKey
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
load_docs(Context, Filter) when is_function(Filter, 2) ->
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
-spec load_attachment(ne_binary() | wh_json:object(), ne_binary(), cb_context:context()) ->
                             cb_context:context().
load_attachment(DocId, AName, Context) when is_binary(DocId) ->
    case couch_mgr:fetch_attachment(cb_context:account_db(Context), DocId, AName) of
        {'error', Error} -> handle_couch_mgr_errors(Error, DocId, Context);
        {'ok', AttachBin} ->
            lager:debug("loaded attachment ~s from doc ~s from db ~s"
                        ,[AName, DocId, cb_context:account_db(Context)]
                       ),
            Context1 = load(DocId, Context),
            'success' = cb_context:resp_status(Context1),
            cb_context:setters(Context1
                               ,[{fun cb_context:set_resp_data/2, AttachBin}
                                 ,{fun cb_context:set_resp_etag/2, rev_to_etag(cb_context:doc(Context1))}
                                ])
    end;
load_attachment(Doc, AName, Context) ->
    load_attachment(find_doc_id(Doc), AName, Context).

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
            IDs = [wh_doc:id(JObj) || JObj <- JObjs],
            handle_couch_mgr_errors(Error, IDs, Context);
        {'ok', JObj1} ->
            Context1 = handle_couch_mgr_success(JObj1, Context),
            _ = wh_util:spawn('provisioner_util', 'maybe_send_contact_list', [Context1]),
            Context1
    end;
save(Context, JObj, Options) ->
    JObj0 = update_pvt_parameters(JObj, Context),
    case couch_mgr:save_doc(cb_context:account_db(Context), JObj0, Options) of
        {'error', Error} ->
            DocId = wh_doc:id(JObj0),
            handle_couch_mgr_errors(Error, DocId, Context);
        {'ok', JObj1} ->
            Context1 = handle_couch_mgr_success(JObj1, Context),
            _ = wh_util:spawn('provisioner_util', 'maybe_send_contact_list', [Context1]),
            Context1
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

ensure_saved(Context) ->
    ensure_saved(Context, []).

ensure_saved(Context, Options) ->
    ensure_saved(Context, cb_context:doc(Context), Options).

ensure_saved(Context, JObj, Options) ->
    JObj0 = update_pvt_parameters(JObj, Context),
    case couch_mgr:ensure_saved(cb_context:account_db(Context), JObj0, Options) of
        {'error', Error} ->
            DocId = wh_json:get_value(<<"_id">>, JObj0),
            handle_couch_mgr_errors(Error, DocId, Context);
        {'ok', JObj1} ->
            Context1 = handle_couch_mgr_success(JObj1, Context),
            _ = wh_util:spawn('provisioner_util', 'maybe_send_contact_list', [Context1]),
            Context1
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
save_attachment(DocId, Name, Contents, Context, Options) ->
    Opts1 = case props:get_value('rev', Options) of
                'undefined' ->
                    {'ok', Rev} = couch_mgr:lookup_doc_rev(cb_context:account_db(Context), DocId),
                    lager:debug("looking up rev for ~s: ~s", [DocId, Rev]),
                    [{'rev', Rev} | Options];
                _O -> Options
            end,

    AName = wh_util:clean_binary(Name),

    case couch_mgr:put_attachment(cb_context:account_db(Context), DocId, AName, Contents, Opts1) of
        {'error', 'conflict'=Error} ->
            lager:debug("saving attachment resulted in a conflict, checking for validity"),
            Context1 = load(DocId, Context, [{'use_cache', 'false'}]),
            case wh_doc:attachment(cb_context:doc(Context1), AName) of
                'undefined' ->
                    lager:debug("attachment does appear to be missing, reporting error"),
                    _ = maybe_delete_doc(Context, DocId),
                    handle_couch_mgr_errors(Error, AName, Context);
                _Attachment ->
                    lager:debug("attachment ~s was in _attachments, considering it successful", [AName]),
                    {'ok', Rev1} = couch_mgr:lookup_doc_rev(cb_context:account_db(Context), DocId),
                    cb_context:setters(Context
                                       ,[{fun cb_context:set_doc/2, wh_json:new()}
                                         ,{fun cb_context:set_resp_status/2, 'success'}
                                         ,{fun cb_context:set_resp_data/2, wh_json:new()}
                                         ,{fun cb_context:set_resp_etag/2, rev_to_etag(Rev1)}
                                        ])
            end;
        {'error', Error} ->
            lager:debug("error putting attachment into ~s: ~p"
                        ,[cb_context:account_db(Context), Error]
                       ),
            _ = maybe_delete_doc(Context, DocId),
            handle_couch_mgr_errors(Error, AName, Context);
        {'ok', _Res} ->
            lager:debug("saved attachment ~s to doc ~s to db ~s"
                        ,[AName, DocId, cb_context:account_db(Context)]
                       ),
            {'ok', Rev1} = couch_mgr:lookup_doc_rev(cb_context:account_db(Context), DocId),
            cb_context:setters(Context
                               ,[{fun cb_context:set_doc/2, wh_json:new()}
                                 ,{fun cb_context:set_resp_status/2, 'success'}
                                 ,{fun cb_context:set_resp_data/2, wh_json:new()}
                                 ,{fun cb_context:set_resp_etag/2, rev_to_etag(Rev1)}
                                ])
    end.

-spec maybe_delete_doc(cb_context:context(), ne_binary()) ->
                              {'ok', _} |
                              {'error', _}.
maybe_delete_doc(Context, DocId) ->
    AccountDb = cb_context:account_db(Context),
    case couch_mgr:open_doc(AccountDb, DocId) of
        {'error', _}=Error -> Error;
        {'ok', JObj} ->
            case wh_doc:attachments(JObj) of
                'undefined' -> couch_mgr:del_doc(AccountDb, JObj);
                _Attachments -> {'ok', 'non_empty'}
            end
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
-spec delete(cb_context:context(), 'permanent' | 'soft') -> cb_context:context().

delete(Context) ->
    delete(Context, 'soft').

delete(Context, 'soft') ->
    case couch_mgr:lookup_doc_rev(cb_context:account_db(Context)
                                  ,wh_doc:id(cb_context:doc(Context))
                                 )
    of
        {'ok', Rev} ->
            soft_delete(Context, Rev);
        {'error', _E} ->
            soft_delete(Context, wh_doc:revision(cb_context:doc(Context)))
    end;
delete(Context, 'permanent') ->
    JObj = cb_context:doc(Context),
    Del = wh_json:from_list([{<<"_id">>, wh_doc:id(JObj)}
                             ,{<<"_rev">>, wh_doc:revision(JObj)}
                            ]),
    do_delete(Context, Del, fun couch_mgr:del_doc/2).

-spec soft_delete(cb_context:context(), api_binary()) -> cb_context:context().
soft_delete(Context, Rev) ->
    lager:debug("soft deleting with rev ~s", [Rev]),
    JObj1 = lists:foldl(fun({F, V}, J) -> F(J, V) end
                        ,update_pvt_parameters(cb_context:doc(Context), Context)
                        ,[{fun wh_doc:set_soft_deleted/2, 'true'}
                          ,{fun wh_doc:set_revision/2, Rev}
                         ]),
    do_delete(Context, JObj1, fun couch_mgr:save_doc/2).

-type delete_fun() :: fun((ne_binary(), wh_json:object() | ne_binary()) ->
                                 {'ok', wh_json:object() | wh_json:objects()} |
                                 couch_mgr:couchbeam_error()).

-spec do_delete(cb_context:context(), wh_json:object(), delete_fun()) ->
                       cb_context:context().
do_delete(Context, JObj, CouchFun) ->
    case CouchFun(cb_context:account_db(Context), JObj) of
        {'error', 'not_found'} ->
            lager:debug("doc ~s wasn't found in ~s, not deleting"
                        ,[wh_doc:id(JObj), cb_context:account_db(Context)]
                       ),
            handle_couch_mgr_success(JObj, Context);
        {'error', Error} ->
            DocId = wh_doc:id(JObj),
            handle_couch_mgr_errors(Error, DocId, Context);
        {'ok', _} ->
            lager:debug("'deleted' ~s from ~s"
                        ,[wh_doc:id(JObj), cb_context:account_db(Context)]
                       ),
            Context1 = handle_couch_mgr_success(JObj, Context),
            _ = wh_util:spawn('provisioner_util', 'maybe_send_contact_list', [Context1]),
            Context1
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
delete_attachment(DocId, AName, Context) ->
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
-spec rev_to_etag(wh_json:object() | wh_json:objects() | ne_binary()) ->
                         'undefined' | 'automatic' | string().
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
-spec update_pagination_envelope_params(cb_context:context(), term(), non_neg_integer() | 'undefined') ->
                                               cb_context:context().
update_pagination_envelope_params(Context, StartKey, PageSize) ->
    update_pagination_envelope_params(Context, StartKey, PageSize, 'undefined').

-spec update_pagination_envelope_params(cb_context:context(), term(), non_neg_integer() | 'undefined', api_binary()) ->
                                               cb_context:context().
update_pagination_envelope_params(Context, StartKey, PageSize, NextStartKey) ->
    CurrentPageSize = wh_json:get_value(<<"page_size">>, cb_context:resp_envelope(Context), 0),
    cb_context:set_resp_envelope(Context
                                 ,wh_json:set_values(
                                    props:filter_undefined(
                                      [{<<"start_key">>, StartKey}
                                       ,{<<"page_size">>, PageSize + CurrentPageSize}
                                       ,{<<"next_start_key">>, NextStartKey}
                                      ])
                                    ,cb_context:resp_envelope(Context)
                                   )).

-spec handle_couch_mgr_pagination_success(wh_json:objects(), pos_integer() | 'undefined', ne_binary(), load_view_params()) ->
                                                 cb_context:context().
handle_couch_mgr_pagination_success(JObjs
                                    ,_PageSize
                                    ,?VERSION_1
                                    ,#load_view_params{context=Context
                                                       ,filter_fun=FilterFun
                                                       ,direction=Direction
                                                      }=LVPs
                                   ) ->
    load_view(LVPs#load_view_params{
                context=cb_context:set_doc(Context
                                           ,apply_filter(FilterFun, JObjs, Context, Direction)
                                           ++ cb_context:doc(Context)
                                          )
               });

handle_couch_mgr_pagination_success([]
                                    ,_PageSize
                                    ,_Version
                                    ,#load_view_params{context=Context
                                                       ,start_key=StartKey
                                                      }=LVPs
                                   ) ->
    load_view(LVPs#load_view_params{context=update_pagination_envelope_params(Context, StartKey, 0)});

handle_couch_mgr_pagination_success([_|_]=JObjs
                                    ,'undefined'
                                    ,_Version
                                    ,#load_view_params{context=Context
                                                       ,start_key=StartKey
                                                       ,filter_fun=FilterFun
                                                       ,page_size=PageSize
                                                       ,direction=Direction
                                                      }=LVPs
                                   ) ->
    Filtered = apply_filter(FilterFun, JObjs, Context, Direction),
    FilteredCount = length(Filtered),

    load_view(LVPs#load_view_params{context=
                                        update_pagination_envelope_params(
                                          cb_context:set_doc(Context, Filtered ++ cb_context:doc(Context))
                                          ,StartKey
                                          ,FilteredCount
                                         )
                                    ,page_size=PageSize-FilteredCount
                                   });

handle_couch_mgr_pagination_success([_|_]=JObjs
                                    ,PageSize
                                    ,_Version
                                    ,#load_view_params{context=Context
                                                       ,start_key=StartKey
                                                       ,filter_fun=FilterFun
                                                       ,direction=Direction
                                                      }=LVPs
                                   ) ->
    try lists:split(PageSize, JObjs) of
        {Results, []} ->
            Filtered = apply_filter(FilterFun, Results, Context, Direction),

            load_view(LVPs#load_view_params{
                        context=
                            cb_context:set_doc(
                              update_pagination_envelope_params(Context, StartKey, PageSize)
                              ,Filtered ++ cb_context:doc(Context)
                             )
                        ,page_size=0
                       });
        {Results, [NextJObj]} ->
            NextStartKey = wh_json:get_value(<<"key">>, NextJObj),
            Filtered = apply_filter(FilterFun, Results, Context, Direction),
            lager:debug("next start key: ~p", [NextStartKey]),

            load_view(LVPs#load_view_params{
                        context=
                            cb_context:set_doc(
                              update_pagination_envelope_params(Context, StartKey, PageSize, NextStartKey)
                              ,Filtered ++ cb_context:doc(Context)
                             )
                        ,page_size=0
                       })
    catch
        'error':'badarg' ->
            Filtered = apply_filter(FilterFun, JObjs, Context, Direction),
            FilteredCount = length(Filtered),

            lager:debug("recv less than ~p results: ~p", [PageSize, FilteredCount]),

            load_view(LVPs#load_view_params{
                        context=
                            cb_context:set_doc(
                              update_pagination_envelope_params(Context, StartKey, FilteredCount)
                              ,Filtered ++ cb_context:doc(Context)
                             )
                        ,page_size=PageSize - FilteredCount
                       })
    end.

-type filter_fun() :: fun((wh_json:object(), wh_json:objects()) -> wh_json:objects()).

-spec apply_filter('undefined' | filter_fun(), wh_json:objects(), cb_context:context(), direction()) ->
                          wh_json:objects().
-spec apply_filter('undefined' | filter_fun(), wh_json:objects(), cb_context:context(), direction(), boolean()) ->
                          wh_json:objects().
apply_filter(FilterFun, JObjs, Context, Direction) ->
    apply_filter(FilterFun, JObjs, Context, Direction, has_qs_filter(Context)).

apply_filter(FilterFun, JObjs, Context, Direction, HasQSFilter) ->
    lager:debug("applying filter fun: ~p, qs filter: ~p to dir ~p", [FilterFun, HasQSFilter, Direction]),

    Filtered =
        maybe_apply_custom_filter(FilterFun
                                  ,[JObj
                                    || JObj <- JObjs,
                                       filtered_doc_by_qs(JObj, HasQSFilter, Context)
                                   ]),
    case Direction of
        'ascending' -> Filtered;
        'descending' -> lists:reverse(Filtered)
    end.

-spec maybe_apply_custom_filter('undefined' | filter_fun(), wh_json:objects()) -> wh_json:objects().
maybe_apply_custom_filter('undefined', JObjs) -> JObjs;
maybe_apply_custom_filter(FilterFun, JObjs) ->
    [JObj
     || JObj <- lists:foldl(FilterFun, [], JObjs),
        (not wh_util:is_empty(JObj))
    ].

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec filtered_doc_by_qs(wh_json:object(), boolean(), cb_context:context()) -> boolean().
filtered_doc_by_qs(_JObj, 'false', _Context) -> 'true';
filtered_doc_by_qs(JObj, 'true', Context) ->
    filter_doc(wh_json:get_value(<<"doc">>, JObj), Context).

-spec handle_couch_mgr_success(wh_json:object() | wh_json:objects(), cb_context:context()) -> cb_context:context().
handle_couch_mgr_success([], Context) ->
    cb_context:setters(Context
                ,[{fun cb_context:set_doc/2, []}
                  ,{fun cb_context:set_resp_status/2, 'success'}
                  ,{fun cb_context:set_resp_data/2, []}
                  ,{fun cb_context:set_resp_etag/2, 'undefined'}
                  | version_specific_success([], Context)
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
    cb_context:setters(Context
                       ,[{fun cb_context:set_doc/2, Thing}
                         ,{fun cb_context:set_resp_status/2, 'success'}
                         ,{fun cb_context:set_resp_data/2, Thing}
                         ,{fun cb_context:set_resp_etag/2, 'undefined'}
                        ]).

-spec handle_json_success(wh_json:object() | wh_json:objects(), cb_context:context()) ->
                                 cb_context:context().
-spec handle_json_success(wh_json:object() | wh_json:objects(), cb_context:context(), http_method()) ->
                                 cb_context:context().
handle_json_success(JObj, Context) ->
    handle_json_success(JObj, Context, cb_context:req_verb(Context)).

handle_json_success([_|_]=JObjs, Context, ?HTTP_PUT) ->
    RespData = [wh_json:public_fields(JObj)
                || JObj <- JObjs,
                   not wh_doc:is_soft_deleted(JObj)
               ],
    RespHeaders = [{<<"Location">>, wh_json:get_value(<<"_id">>, JObj)}
                   || JObj <- JObjs
                  ] ++ cb_context:resp_headers(Context),
    cb_context:setters(Context
                       ,[{fun cb_context:set_doc/2, JObjs}
                         ,{fun cb_context:set_resp_status/2, 'success'}
                         ,{fun cb_context:set_resp_data/2, RespData}
                         ,{fun cb_context:set_resp_etag/2, rev_to_etag(JObjs)}
                         ,{fun cb_context:set_resp_headers/2, RespHeaders}
                        ]);
handle_json_success([_|_]=JObjs, Context, _Verb) ->
    RespData = [wh_json:public_fields(JObj)
                || JObj <- JObjs,
                   not wh_doc:is_soft_deleted(JObj)
               ],
    cb_context:setters(Context
                       ,[{fun cb_context:set_doc/2, JObjs}
                         ,{fun cb_context:set_resp_status/2, 'success'}
                         ,{fun cb_context:set_resp_data/2, RespData}
                         ,{fun cb_context:set_resp_etag/2, rev_to_etag(JObjs)}
                         | version_specific_success(JObjs, Context)
                        ]);
handle_json_success(JObj, Context, ?HTTP_PUT) ->
    RespHeaders = [{<<"Location">>, wh_json:get_value(<<"_id">>, JObj)}
                   | cb_context:resp_headers(Context)
                  ],
    cb_context:setters(Context
                       ,[{fun cb_context:set_doc/2, JObj}
                         ,{fun cb_context:set_resp_status/2, 'success'}
                         ,{fun cb_context:set_resp_data/2, wh_json:public_fields(JObj)}
                         ,{fun cb_context:set_resp_etag/2, rev_to_etag(JObj)}
                         ,{fun cb_context:set_resp_headers/2, RespHeaders}
                        ]);
handle_json_success(JObj, Context, _Verb) ->
    cb_context:setters(Context
                       ,[{fun cb_context:set_doc/2, JObj}
                         ,{fun cb_context:set_resp_status/2, 'success'}
                         ,{fun cb_context:set_resp_data/2, wh_json:public_fields(JObj)}
                         ,{fun cb_context:set_resp_etag/2, rev_to_etag(JObj)}
                        ]).

-spec version_specific_success(wh_json:objects(), cb_context:context()) -> list().
version_specific_success(JObjs, Context) ->
    version_specific_success(JObjs, Context, cb_context:api_version(Context)).
version_specific_success(_JObjs, _Context, ?VERSION_1) ->
    [];
version_specific_success(JObjs, Context, _Version) ->
    [{fun cb_context:set_resp_envelope/2
      ,wh_json:set_value(<<"page_size">>, length(JObjs), cb_context:resp_envelope(Context))
     }
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_couch_mgr_errors(couch_util:couchbeam_errors(), api_binary() | api_binaries(), cb_context:context()) ->
                                     cb_context:context().
handle_couch_mgr_errors('invalid_db_name', _, Context) ->
    lager:debug("datastore ~s not_found", [cb_context:account_db(Context)]),
    cb_context:add_system_error('datastore_missing', wh_json:from_list([{<<"cause">>, cb_context:account_db(Context)}]), Context);
handle_couch_mgr_errors('db_not_reachable', _DocId, Context) ->
    lager:debug("operation on doc ~s from ~s failed: db_not_reachable", [_DocId, cb_context:account_db(Context)]),
    cb_context:add_system_error('datastore_unreachable', Context);
handle_couch_mgr_errors('not_found', DocId, Context) ->
    lager:debug("operation on doc ~s from ~s failed: not_found", [DocId, cb_context:account_db(Context)]),
    cb_context:add_system_error('bad_identifier', wh_json:from_list([{<<"cause">>, DocId}]),  Context);
handle_couch_mgr_errors('conflict', DocId, Context) ->
    lager:debug("failed to update doc ~s in ~s: conflicts", [DocId, cb_context:account_db(Context)]),
    cb_context:add_system_error('datastore_conflict', Context);
handle_couch_mgr_errors('invalid_view_name', View, Context) ->
    lager:debug("loading view ~s from ~s failed: invalid view", [View, cb_context:account_db(Context)]),
    cb_context:add_system_error('datastore_missing_view', wh_json:from_list([{<<"cause">>, wh_util:to_binary(View)}]), Context);
handle_couch_mgr_errors(Else, _View, Context) ->
    lager:debug("operation failed: ~p on ~p", [Else, _View]),
    try wh_util:to_binary(Else) of
        Reason -> cb_context:add_system_error('datastore_fault', wh_json:from_list([{<<"cause">>, Reason}]), Context)
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
            case cb_context:account_db(Context) of
                'undefined' -> JObj;
                AccountDb -> wh_json:set_value(<<"pvt_account_db">>, AccountDb, JObj)
            end;
        _Else -> JObj
    end.

-spec add_pvt_account_id(wh_json:object(), cb_context:context()) -> wh_json:object().
add_pvt_account_id(JObj, Context) ->
    case wh_json:get_value(<<"pvt_account_id">>, JObj) of
        'undefined' ->
            case cb_context:account_id(Context) of
                'undefined' -> JObj;
                AccountId -> wh_json:set_value(<<"pvt_account_id">>, AccountId, JObj)
            end;
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

-spec add_pvt_request_id(wh_json:object(), cb_context:context()) -> wh_json:object().
add_pvt_request_id(JObj, Context) ->
    RequestId = cb_context:req_id(Context),
    wh_json:set_value(<<"pvt_request_id">>, RequestId, JObj).

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
%% @public
%% @doc
%% Given a context or query parameter json object determines if the
%% request has a filter defined
%% @end
%%--------------------------------------------------------------------
-spec has_qs_filter(cb_context:context()) -> boolean().
has_qs_filter(Context) ->
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
-spec filter_doc(api_object(), cb_context:context()) -> boolean().
filter_doc('undefined', _Context) ->
    lager:debug("no doc was returned (no include_docs?)"),
    'true';
filter_doc(Doc, Context) ->
    filter_doc_by_querystring(Doc, cb_context:query_string(Context)).

-spec filter_doc_by_querystring(wh_json:object(), wh_json:object()) -> boolean().
filter_doc_by_querystring(Doc, QueryString) ->
    wh_json:all(fun({K, V}) ->
                        should_filter_doc(Doc, K, V)
                end
                ,QueryString
               ).

-spec should_filter_doc(wh_json:object(), ne_binary(), wh_json:json_term()) -> boolean().
should_filter_doc(Doc, K, V) ->
    try filter_prop(Doc, K, V) of
        'undefined' -> 'true';
        Bool ->
            lager:debug("doc filtered by ~s(~p): ~s", [K, V, Bool]),
            Bool
    catch
        _E:_R ->
            lager:debug("failed to process filter ~s: ~s:~p", [K, _E, _R]),
            'false'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns 'true' or 'false' if the prop is found inside the doc
%% @end
%%--------------------------------------------------------------------
-spec filter_prop(wh_json:object(), ne_binary(), term()) -> api_boolean().
filter_prop(Doc, <<"filter_not_", Key/binary>>, Val) ->
    not should_filter(Doc, Key, Val);
filter_prop(Doc, <<"filter_", Key/binary>>, Val) ->
    should_filter(Doc, Key, Val);
filter_prop(Doc, <<"has_key">>, Key) ->
    has_key(Doc, Key);
filter_prop(Doc, <<"key_missing">>, Key) ->
    not has_key(Doc, Key);
filter_prop(Doc, <<"has_value">>, Key) ->
    has_value(Doc, Key);
filter_prop(Doc, <<"created_from">>, Val) ->
    lowerbound(wh_doc:created(Doc), wh_util:to_integer(Val));
filter_prop(Doc, <<"created_to">>, Val) ->
    upperbound(wh_doc:created(Doc), wh_util:to_integer(Val));
filter_prop(Doc, <<"modified_from">>, Val) ->
    lowerbound(wh_doc:modified(Doc), wh_util:to_integer(Val));
filter_prop(Doc, <<"modified_to">>, Val) ->
    upperbound(wh_doc:modified(Doc), wh_util:to_integer(Val));
filter_prop(_, _, _) ->
    'undefined'.

-spec upperbound(integer(), integer()) -> boolean().
upperbound(DocTimestamp, QSTimestamp) ->
    QSTimestamp >= DocTimestamp.

-spec lowerbound(integer(), integer()) -> boolean().
lowerbound(DocTimestamp, QSTimestamp) ->
    QSTimestamp =< DocTimestamp.

-spec should_filter(wh_json:object(), ne_binary(), wh_json:json_term()) -> boolean().
should_filter(Doc, Key, Val) ->
    Keys = binary:split(Key, <<".">>, ['global']),
    wh_json:get_binary_value(Keys, Doc, <<>>) =:= wh_util:to_binary(Val).

-spec has_key(wh_json:object(), ne_binary()) -> boolean().
has_key(Doc, Key) ->
    Keys = binary:split(Key, <<".">>, ['global']),
    wh_json:get_value(Keys, Doc) =/= 'undefined'.

-spec has_value(wh_json:object(), ne_binary()) -> boolean().
has_value(Doc, Key) ->
    Keys = binary:split(Key, <<".">>, ['global']),
    wh_json:get_ne_value(Keys, Doc) =/= 'undefined'.
