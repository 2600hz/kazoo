%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(crossbar_doc).

-export([load/2, load/3
        ,load_merge/2, load_merge/3, load_merge/4
        ,patch_and_validate/3, patch_and_validate/4
        ,load_view/3, load_view/4, load_view/5, load_view/6
        ,load_attachment/4, load_docs/2
        ,save/1, save/2, save/3
        ,update/3, update/4
        ,delete/1, delete/2
        ,save_attachment/4, save_attachment/5
        ,delete_attachment/3
        ,rev_to_etag/1
        ,current_doc_vsn/0
        ,update_pvt_parameters/2, add_pvt_auth/3, pvt_updates/2
        ,start_key/1, start_key/2
        ]).

-export([handle_json_success/2]).
-export([handle_datamgr_success/2
        ,handle_datamgr_errors/3
        ]).

-ifdef(TEST).
-export([patch_the_doc/2
        ]).
-endif.

-export_type([view_options/0
             ,load_options/0
             ,startkey/0
             ]).

-include("crossbar.hrl").

-define(CROSSBAR_DOC_VSN, <<"1">>).
-define(PVT_FUNS, [fun add_pvt_vsn/3
                  ,fun add_pvt_account_id/3
                  ,fun add_pvt_account_db/3
                  ,fun add_pvt_created/3
                  ,fun add_pvt_modified/3
                  ,fun add_pvt_request_id/3
                  ,fun add_pvt_auth/3
                  ,fun add_pvt_alphanum_name/3
                  ]).

-type direction() :: 'ascending' | 'descending'.

-type startkey() :: kz_json:api_json_term().

-type startkey_fun() :: 'undefined' |
                        fun((cb_context:context()) -> startkey()) |
                        fun((kz_term:proplist(), cb_context:context()) -> startkey()).

-type view_options() :: kazoo_data:view_options() |
                        [{'databases', kz_term:ne_binaries()} |
                         {'startkey_fun', startkey_fun()}
                        ].

-type load_option() :: {?OPTION_EXPECTED_TYPE, kz_term:ne_binary()} |
                       {'use_cache', boolean()}.
-type load_options() :: kazoo_data:view_options() |
                        [load_option()].

-record(load_view_params, {view :: kz_term:api_binary()
                          ,view_options = [] :: view_options()
                          ,context :: cb_context:context()
                          ,start_key :: startkey()
                          ,should_paginate :: boolean()
                          ,page_size :: non_neg_integer() | kz_term:api_binary()
                          ,filter_fun :: filter_fun()
                          ,dbs = [] :: kz_term:ne_binaries()
                          ,direction = 'ascending' :: direction()
                          }).
-type load_view_params() :: #load_view_params{}.

%%------------------------------------------------------------------------------
%% @doc Returns the version number attached to created/updated documents.
%% Indicates what private fields are created/updated when saving.
%% @end
%%------------------------------------------------------------------------------
-spec current_doc_vsn() -> kz_term:ne_binary().
current_doc_vsn() -> ?CROSSBAR_DOC_VSN.

%% equiv load(DocId, Context, [])

-spec load(kazoo_data:docid() | kazoo_data:docids(), cb_context:context()) ->
          cb_context:context().
load({DocType, DocId}, Context) ->
    load(DocId, Context, [{'doc_type', DocType}]);
load(DocId, Context) ->
    load(DocId, Context, []).

%%------------------------------------------------------------------------------
%% @doc This function attempts to load the context with account details,
%% including the account db name and the account doc.
%%
%% Failure here returns `404 Not Found', `409 Conflict'
%% or `503 Service Unavailable'.
%% @end
%%------------------------------------------------------------------------------

-spec load(kazoo_data:docid() | kazoo_data:docids(), cb_context:context(), load_options()) ->
          cb_context:context().
load({DocType, DocId}, Context, Options) ->
    load(DocId, Context, [{'doc_type', DocType} | Options]);
load(DocId, Context, Options) ->
    load(DocId, Context, Options, cb_context:resp_status(Context)).

-spec load(kz_term:ne_binary() | kz_term:ne_binaries(), cb_context:context(), load_options(), crossbar_status()) ->
          cb_context:context().
load(_DocId, Context, _Options, 'error') -> Context;
load(DocId, Context, Options, _RespStatus) when is_binary(DocId) ->
    case maybe_open_cache_doc(cb_context:db_name(Context), DocId, Options) of
        {'error', Error} ->
            handle_datamgr_errors(Error, DocId, Context);
        {'ok', JObj} ->
            case check_document_type(Context, JObj, Options) of
                'true' -> handle_successful_load(Context, JObj);
                'false' ->
                    ErrorCause = kz_json:from_list([{<<"cause">>, DocId}]),
                    cb_context:add_system_error('bad_identifier', ErrorCause, Context)
            end
    end;

load([], Context, _Options, _RespStatus) ->
    cb_context:add_system_error('bad_identifier',  Context);
load([_|_]=IDs, Context, Options, _RespStatus) ->
    case maybe_open_cache_docs(cb_context:db_name(Context), IDs, Options) of
        {'error', Error} -> handle_datamgr_errors(Error, IDs, Context);
        {'ok', JObjs} ->
            {Docs, Context1} = extract_included_docs(Context, JObjs),
            case check_document_type(Context1, Docs, Options) of
                'true' ->
                    cb_context:store(handle_datamgr_success(Docs, Context1), 'db_doc', Docs);
                'false' ->
                    ErrorCause = kz_json:from_list([{<<"cause">>, IDs}]),
                    cb_context:add_system_error('bad_identifier', ErrorCause, Context1)
            end
    end.

-spec maybe_open_cache_doc(kz_term:ne_binary(), kazoo_data:docid(), kz_term:proplist()) ->
          {'ok', kz_json:object()} |
          kz_datamgr:data_error().
maybe_open_cache_doc(DbName, DocId, Options) ->
    case props:get_is_true('use_cache', Options, 'true') of
        'true' -> kz_datamgr:open_cache_doc(DbName, DocId, Options);
        'false' -> kz_datamgr:open_doc(DbName, DocId, Options)
    end.

-spec maybe_open_cache_docs(kz_term:ne_binary(), kazoo_data:docids(), kz_term:proplist()) ->
          {'ok', kz_json:objects()} |
          kz_datamgr:data_error().
maybe_open_cache_docs(DbName, DocIds, Options) ->
    case props:get_is_true('use_cache', Options, 'true') of
        'true' -> kz_datamgr:open_cache_docs(DbName, DocIds, Options);
        'false' -> kz_datamgr:open_docs(DbName, DocIds, Options)
    end.

%%------------------------------------------------------------------------------
%% @doc Returns `true' or `false' if the requested document type is matched
%% against the actual document type or the name of the last resource
%% that request it. It first checks expected type is matched with document
%% type, if it fails it checks document type with the name of the
%% last resource. If the document doesn't have a `pvt_type'
%% property or the resource requested that expected type to be `any',
%% it will return `true'.
%% @end
%%------------------------------------------------------------------------------
-spec check_document_type(cb_context:context(), kz_json:object() | kz_json:objects(), kz_term:proplist()) ->
          boolean().
check_document_type(_Context, [], _Options) -> 'true';
check_document_type(Context, [_|_]=JObjs, Options) ->
    F = fun(JObj) -> check_document_type(Context, JObj, Options) end,
    lists:all(F, JObjs);
check_document_type(Context, JObj, Options) ->
    JObjType = kz_doc:type(JObj),
    ExpectedType = props:get_value(?OPTION_EXPECTED_TYPE, Options),
    [{Noun, _}| _] = cb_context:req_nouns(Context),
    ReqType = normalize_requested_resource_name(Noun),
    document_type_match(JObjType, ExpectedType, ReqType).

-spec document_type_match(kz_term:api_binary(), kz_term:api_binary(), kz_term:ne_binary()) -> boolean().
document_type_match('undefined', _ExpectedType, _ReqType) ->
    lager:debug("document doesn't have type, requested type is ~p", [_ReqType]),
    'true';
document_type_match(_JObjType, <<"any">>, _) -> 'true';
document_type_match(_JObjType, 'undefined', _) -> 'true';
document_type_match(ExpectedType, ExpectedTypes, _)
  when is_list(ExpectedTypes) ->
    lists:member(ExpectedType, ExpectedTypes);
document_type_match(ExpectedType, ExpectedType, _) -> 'true';
document_type_match(ReqType, _, ReqType) ->
    lager:debug("expected type is not specified, checking against requested type ~p", [ReqType]),
    'true';
document_type_match(_JObjType, _ExpectedType, _ReqType) ->
    lager:warning("the document type ~s does not match the expected type ~s nor the requested type ~s"
                 ,[_JObjType, _ExpectedType, _ReqType]
                 ),
    'false'.

-spec normalize_requested_resource_name(kz_term:ne_binary()) -> kz_term:ne_binary().
normalize_requested_resource_name(Name) ->
    case props:get_value(Name, ?SPECIAL_EXPECTED_TYPE) of
        'undefined' -> depluralize_resource_name(Name);
        Special -> Special
    end.

-spec depluralize_resource_name(kz_term:ne_binary()) -> kz_term:ne_binary().
depluralize_resource_name(Name) ->
    Size = byte_size(Name) - 1,
    case Name of
        <<Bin:Size/binary, "s">> -> Bin;
        Bin -> Bin
    end.

-spec handle_successful_load(cb_context:context(), kz_json:object()) -> cb_context:context().
handle_successful_load(Context, JObj) ->
    handle_successful_load(Context, JObj, kz_doc:is_soft_deleted(JObj)).

-spec handle_successful_load(cb_context:context(), kz_json:object(), boolean()) -> cb_context:context().
handle_successful_load(Context, JObj, 'true') ->
    lager:debug("doc ~s(~s) is soft-deleted, returning bad_identifier"
               ,[kz_doc:id(JObj), kz_doc:revision(JObj)]
               ),
    cb_context:add_system_error('bad_identifier'
                               ,kz_json:from_list([{<<"cause">>, kz_doc:id(JObj)}])
                               ,Context
                               );
handle_successful_load(Context, JObj, 'false') ->
    lager:debug("loaded doc ~s(~s) from ~s"
               ,[kz_doc:id(JObj), kz_doc:revision(JObj), cb_context:db_name(Context)]
               ),
    cb_context:store(handle_datamgr_success(JObj, Context), 'db_doc', JObj).

%% @equiv load_merge(DocId, cb_context:doc(Context), Context, [])

-spec load_merge(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
load_merge(DocId, Context) ->
    load_merge(DocId, cb_context:doc(Context), Context, []).

%% @equiv load_merge(DocId, cb_context:doc(Context), Context, Options)

-spec load_merge(kz_term:ne_binary(), cb_context:context(), kz_term:proplist()) -> cb_context:context().
load_merge(DocId, Context, Options) ->
    load_merge(DocId, cb_context:doc(Context), Context, Options).

%%------------------------------------------------------------------------------
%% @doc This function attempts to merge the submitted data with the private
%% fields of an existing account document. If successful it will
%% load the context with the account details.
%%
%% Failure here returns `404 Not Found', `409 Conflict'
%% or `503 Service Unavailable'.
%% @end
%%------------------------------------------------------------------------------

-spec load_merge(kz_term:ne_binary(), kz_json:object(), cb_context:context(), kz_term:proplist()) ->
          cb_context:context().
load_merge(DocId, DataJObj, Context, Options) ->
    load_merge(DocId, DataJObj, Context, Options, cb_context:load_merge_bypass(Context)).

-spec load_merge(kz_term:ne_binary(), kz_json:object(), cb_context:context(), kz_term:proplist(), kz_term:api_object()) ->
          cb_context:context().
load_merge(DocId, DataJObj, Context, Options, 'undefined') ->
    Context1 = load(DocId, Context, Options),
    case 'success' =:= cb_context:resp_status(Context1) of
        'false' -> Context1;
        'true' ->
            lager:debug("loaded doc ~s(~s), merging", [DocId, kz_doc:revision(cb_context:doc(Context1))]),
            Merged = kz_json:merge_jobjs(kz_doc:private_fields(cb_context:doc(Context1)), DataJObj),
            handle_datamgr_success(Merged, Context1)
    end;
load_merge(_DocId, _DataJObj, Context, _Options, BypassJObj) ->
    handle_datamgr_success(BypassJObj, Context).

-type validate_fun() :: fun((kz_term:ne_binary(), cb_context:context()) -> cb_context:context()).

-spec patch_and_validate(kz_term:ne_binary(), cb_context:context(), validate_fun()) ->
          cb_context:context().
patch_and_validate(Id, Context, ValidateFun) ->
    patch_and_validate(Id, Context, ValidateFun, ?TYPE_CHECK_OPTION_ANY).

-spec patch_and_validate(kz_term:ne_binary(), cb_context:context(), validate_fun(), load_options()) ->
          cb_context:context().
patch_and_validate(Id, Context, ValidateFun, LoadOptions) ->
    Context1 = load(Id, Context, LoadOptions),
    patch_and_validate_doc(Id, Context1, ValidateFun, cb_context:resp_status(Context1)).

-spec patch_and_validate_doc(kz_term:ne_binary(), cb_context:context(), validate_fun(), crossbar_status()) ->
          cb_context:context().
patch_and_validate_doc(Id, Context, ValidateFun, 'success') ->
    PatchedJObj = patch_the_doc(cb_context:req_data(Context), cb_context:doc(Context)),
    Context1 = cb_context:set_req_data(Context, PatchedJObj),
    ValidateFun(Id, Context1);
patch_and_validate_doc(Id, Context, ValidateFun, _RespStatus) ->
    ValidateFun(Id, Context).

-spec patch_the_doc(kz_json:object(), kz_json:object()) -> kz_json:object().
patch_the_doc(RequestData, ExistingDoc) ->
    PubJObj = kz_doc:public_fields(RequestData),
    kz_json:merge(fun kz_json:merge_left/2, PubJObj, ExistingDoc).

-spec load_view(kz_term:ne_binary() | 'all_docs', kz_term:proplist(), cb_context:context()) ->
          cb_context:context().
load_view(View, Options, Context) ->
    load_view(View, Options, Context
             ,start_key(Options, Context)
             ,cb_context:pagination_page_size(Context)
             ,'undefined'
             ).

-spec load_view(kz_term:ne_binary() | 'all_docs', kz_term:proplist(), cb_context:context(), kz_json:json_term() | filter_fun()) ->
          cb_context:context().
load_view(View, Options, Context, FilterFun)
  when is_function(FilterFun, 2);
       is_function(FilterFun, 3) ->
    load_view(View, Options, Context
             ,start_key(Options, Context)
             ,cb_context:pagination_page_size(Context)
             ,FilterFun
             );
load_view(View, Options, Context, StartKey) ->
    load_view(View, Options, Context, StartKey
             ,cb_context:pagination_page_size(Context)
             ).
%% @equiv load_view(View, Options, Context, StartKey, PageSize, 'undefined')

-spec load_view(kz_term:ne_binary() | 'all_docs', kz_term:proplist(), cb_context:context(), kz_json:json_term(), pos_integer()) ->
          cb_context:context().
load_view(View, Options, Context, StartKey, PageSize) ->
    load_view(View, Options, Context, StartKey, PageSize, 'undefined').

%%------------------------------------------------------------------------------
%% @doc This function attempts to load the context with the results of a view
%% run against the accounts database.
%%
%% Failure here returns 500 or 503.
%% @end
%%------------------------------------------------------------------------------

-spec load_view(kz_term:ne_binary() | 'all_docs', kz_term:proplist(), cb_context:context(), kz_json:json_term(), pos_integer(), filter_fun()) ->
          cb_context:context().
load_view(View, Options, Context, StartKey, PageSize, FilterFun) ->
    load_view(
      #load_view_params{view = View
                       ,view_options = Options
                       ,context = cb_context:set_doc(Context, [])
                       ,start_key = StartKey
                       ,should_paginate = cb_context:should_paginate(Context)
                       ,page_size = PageSize
                       ,filter_fun = FilterFun
                       ,dbs = [Db || Db <- props:get_value('databases', Options, [cb_context:db_name(Context)]),
                                     kz_datamgr:db_exists(Db, View)
                              ]
                       ,direction = view_sort_direction(Options)
                       }).

-spec view_sort_direction(kz_term:proplist()) -> direction().
view_sort_direction(Options) ->
    case props:get_value('descending', Options) of
        'true' -> 'descending';
        'undefined' -> 'ascending'
    end.

load_view(#load_view_params{dbs = []
                           ,context = Context
                           }) ->
    case cb_context:resp_status(Context) of
        'success' ->
            lager:debug("databases exhausted"),
            handle_datamgr_success(cb_context:doc(Context), Context);
        _Status -> Context
    end;
load_view(#load_view_params{page_size = PageSize
                           ,context = Context
                           ,should_paginate = 'true'
                           })
  when is_integer(PageSize)
       andalso PageSize =< 0 ->
    lager:debug("page_size exhausted: ~p", [PageSize]),
    case 'success' =:= cb_context:resp_status(Context) of
        'false' -> Context;
        'true' -> handle_datamgr_success(cb_context:doc(Context), Context)
    end;
load_view(#load_view_params{view = View
                           ,view_options = Options
                           ,context = Context
                           ,start_key = StartKey
                           ,page_size = PageSize
                           ,dbs = [Db|RestDbs]=Dbs
                           ,direction = _Direction
                           } = LVPs) ->
    Limit = PageSize + 1,

    DefaultOptions =
        props:filter_undefined(
          [{'startkey', StartKey}
          ,{'limit', Limit}
           | props:delete_keys(['startkey', 'startkey_fun', 'limit', 'databases'], Options)
          ]),

    IncludeOptions =
        case crossbar_filter:is_defined(Context) of
            'true' -> ['include_docs' | props:delete('include_docs', DefaultOptions)];
            'false' -> DefaultOptions
        end,

    ViewOptions =
        case props:get_first_defined(['reduce', 'group', 'group_level'], IncludeOptions) of
            'undefined' -> IncludeOptions;
            'false' -> IncludeOptions;
            _V -> props:delete('include_docs', IncludeOptions)
        end,

    lager:debug("kz_datamgr:get_results(~p, ~p, ~p)", [Db, View, ViewOptions]),
    case kz_datamgr:get_results(Db, View, ViewOptions) of
        %% There were more dbs, so move to the next one
        {'error', 'not_found'} when [] =:= RestDbs ->
            lager:debug("either the db ~s or view ~s was not found", [Db, View]),
            crossbar_util:response_missing_view(Context);
        {'error', 'not_found'} ->
            lager:debug("either the db ~s or view ~s was not found", [Db, View]),
            load_view(LVPs#load_view_params{dbs = RestDbs});
        {'error', Error} ->
            handle_datamgr_errors(Error, View, Context);
        {'ok', JObjs} ->
            lager:debug("paginating view '~s' from '~s', starting at '~p'", [View, Db, StartKey]),
            handle_query_results(JObjs
                                ,LVPs#load_view_params{dbs = Dbs
                                                      ,context = cb_context:set_resp_status(Context, 'success')
                                                      }
                                )
    end.

handle_query_results(JObjs, LVPs) ->
    [{'memory', End}] = process_info(self(), ['memory']),
    MemoryLimit = kapps_config:get_integer(?CONFIG_CAT, <<"request_memory_limit">>),
    handle_query_results(JObjs, LVPs, End, MemoryLimit).

handle_query_results(JObjs, LVPs, _MemoryUsed, 'undefined') ->
    handle_datamgr_pagination_success(JObjs, LVPs);
handle_query_results(JObjs, LVPs, MemoryUsed, MemoryLimit) when MemoryUsed < MemoryLimit ->
    lager:debug("under memory cap of ~p: ~p used", [MemoryLimit, MemoryUsed]),
    handle_datamgr_pagination_success(JObjs, LVPs);
handle_query_results(_JObjs, #load_view_params{context=Context}, _MemoryUsed, _MemoryLimit) ->
    lager:warning("memory used ~p exceeds limit ~p", [_MemoryUsed, _MemoryLimit]),
    crossbar_util:response_range_not_satisfiable(Context).

%% @equiv cb_context:req_value(Context, <<"start_key">>)
-spec start_key(cb_context:context()) -> kz_json:api_json_term().
start_key(Context) ->
    cb_context:req_value(Context, <<"start_key">>).

-spec start_key(kz_term:proplist(), cb_context:context()) -> kz_json:api_json_term().
start_key(Options, Context) ->
    case props:get_value('startkey_fun', Options) of
        'undefined' -> start_key_fun(Options, Context);
        Fun when is_function(Fun, 2) -> Fun(Options, Context);
        Fun when is_function(Fun, 1) -> Fun(Context)
    end.

-spec start_key_fun(kz_term:proplist(), cb_context:context()) -> kz_json:api_json_term().
start_key_fun(Options, Context) ->
    case props:get_value('startkey', Options) of
        'undefined' ->
            StartKey = start_key(Context),
            lager:debug("got start_key from request: ~p", [StartKey]),
            StartKey;
        StartKey ->
            lager:debug("getting start_key from options: ~p", [StartKey]),
            StartKey
    end.

%%------------------------------------------------------------------------------
%% @doc This function attempts to load the context with the results of all the
%% docs in the supplied Db, with the fold function weeding out those not
%% desired by returning `undefined' or not adding it to the Accumulator.
%%
%% Failure here returns 500 or 503.
%% @end
%%------------------------------------------------------------------------------
-spec load_docs(cb_context:context(), filter_fun()) -> cb_context:context().
load_docs(Context, Filter)
  when is_function(Filter, 2);
       is_function(Filter, 3) ->
    Fun = case is_function(Filter, 2) of
              'true' -> Filter;
              'false' -> fun(J, Acc) -> Filter(Context, J, Acc) end
          end,
    case kz_datamgr:all_docs(cb_context:db_name(Context)) of
        {'error', Error} -> handle_datamgr_errors(Error, <<"all_docs">>, Context);
        {'ok', JObjs} ->
            Filtered = [JObj
                        || JObj <- lists:foldl(Fun, [], JObjs)
                               ,(not kz_term:is_empty(JObj))
                       ],
            handle_datamgr_success(Filtered, Context)
    end.

%%------------------------------------------------------------------------------
%% @doc This function attempts to load the context with the binary payload
%% stored as an attachment.
%%
%% Failure here returns 500 or 503.
%% @end
%%------------------------------------------------------------------------------
-spec load_attachment({kz_term:ne_binary(), kz_term:ne_binary()} | kazoo_data:docid() | kz_json:object(), kz_term:ne_binary(), kz_term:proplist(), cb_context:context()) ->
          cb_context:context().
load_attachment({DocType, DocId}, AName, Options, Context) ->
    load_attachment(DocId, AName, [{'doc_type', DocType} | Options], Context);
load_attachment(<<_/binary>>=DocId, AName, Options, Context) ->
    case kz_datamgr:fetch_attachment(cb_context:db_name(Context), DocId, AName, Options) of
        {'error', Error} -> handle_datamgr_errors(Error, DocId, Context);
        {'ok', AttachBin} ->
            lager:debug("loaded attachment ~s from doc ~s from db ~s"
                       ,[AName, DocId, cb_context:db_name(Context)]
                       ),
            Context1 = load(DocId, Context, Options),
            'success' = cb_context:resp_status(Context1),

            CT = kz_doc:attachment_content_type(cb_context:doc(Context1), AName, <<"application/octet-stream">>),
            lager:debug("adding content type ~s from attachment ~s", [CT, AName]),

            cb_context:setters(Context1
                              ,[{fun cb_context:set_resp_data/2, AttachBin}
                               ,{fun cb_context:set_resp_etag/2, rev_to_etag(cb_context:doc(Context1))}
                               ,{fun cb_context:add_resp_headers/2, #{<<"content-type">> => CT}}
                               ])
    end;
load_attachment(Doc, AName, Options, Context) ->
    load_attachment({kz_doc:type(Doc), kz_doc:id(Doc)}, AName, Options, Context).

%% @equiv save(Context, [])

-spec save(cb_context:context()) -> cb_context:context().
save(Context) ->
    save(Context, []).

%% @equiv save(Context, cb_context:doc(Context), Options)

-spec save(cb_context:context(), kz_term:proplist()) -> cb_context:context().
save(Context, Options) ->
    save(Context, cb_context:doc(Context), Options).

%%------------------------------------------------------------------------------
%% @doc This function attempts to save the provided document to the accounts
%% database. The result is loaded into the context record.
%%
%% Failure here returns 500 or 503.
%% @end
%%------------------------------------------------------------------------------

-spec save(cb_context:context(), kz_json:object() | kz_json:objects(), kz_term:proplist()) ->
          cb_context:context().
save(Context, [], _Options) ->
    lager:debug("no docs to save"),
    cb_context:set_resp_status(Context, 'success');
save(Context, [_|_]=JObjs, Options) ->
    JObjs0 = update_pvt_parameters(JObjs, Context),
    case crossbar_services:maybe_dry_run(Context, JObjs0) of
        {'allowed', Context1} -> save_jobjs(Context1, JObjs0, Options);
        {'denied', Context1} -> Context1
    end;
save(Context, JObj, Options) ->
    JObj0 = update_pvt_parameters(JObj, Context),
    case crossbar_services:maybe_dry_run(Context, JObj0) of
        {'allowed', Context1} -> save_jobj(Context1, JObj0, Options);
        {'denied', Context1} -> Context1
    end.

-spec save_jobjs(cb_context:context(), kz_json:object() | kz_json:objects(), kz_term:proplist()) ->
          cb_context:context().
save_jobjs(Context, JObjs0, Options) ->
    case kz_datamgr:save_docs(cb_context:db_name(Context), JObjs0, Options) of
        {'error', Error} ->
            IDs = [kz_doc:id(JObj) || JObj <- JObjs0],
            handle_datamgr_errors(Error, IDs, Context);
        {'ok', JObjs1} ->
            Context1 = handle_datamgr_success(JObjs1, Context),
            maybe_send_contact_list(Context1),
            maybe_spawn_service_updates(Context1, JObjs0, cb_context:req_param(Context1, <<"wait_for_service_update">>, 'false')),
            Context1
    end.

-spec maybe_send_contact_list(cb_context:context()) -> 'ok'.
maybe_send_contact_list(Context) ->
    case cb_context:resp_status(Context) of
        'success' ->
            _ = kz_process:spawn(fun provisioner_util:maybe_send_contact_list/4
                                ,[cb_context:account_id(Context)
                                 ,cb_context:auth_token(Context)
                                 ,cb_context:doc(Context)
                                 ,cb_context:fetch(Context, 'db_doc')
                                 ]),
            'ok';
        _Status -> 'ok'
    end.

-spec maybe_spawn_service_updates(cb_context:context(), kz_json:object() | kz_json:objects(), boolean()) -> 'ok'.
maybe_spawn_service_updates(Context, JObjs, 'false') ->
    _ = kz_process:spawn(fun crossbar_services:update_subscriptions/2, [Context, JObjs]),
    lager:debug("executing service subscriptions update in the background");
maybe_spawn_service_updates(Context, JObjs, 'true') ->
    lager:debug("executing service subscriptions update in the foreground, this will take a while"),
    _ = crossbar_services:update_subscriptions(Context, JObjs),
    'ok'.

-spec save_jobj(cb_context:context(), kz_json:object() | kz_json:objects(), kz_term:proplist()) ->
          cb_context:context().
save_jobj(Context, JObj0, Options) ->
    case kz_datamgr:save_doc(cb_context:db_name(Context), JObj0, Options) of
        {'error', Error} ->
            DocId = kz_doc:id(JObj0),
            handle_datamgr_errors(Error, DocId, Context);
        {'ok', JObj1} ->
            Context1 = handle_datamgr_success(JObj1, Context),
            maybe_send_contact_list(Context1),
            maybe_spawn_service_updates(Context1, JObj0, cb_context:req_param(Context1, <<"wait_for_service_update">>, 'false')),
            Context1
    end.

-spec update(cb_context:context(), kz_json:key(), kz_json:flat_proplist()) ->
          cb_context:context().
update(Context, DocId, Updates) ->
    update(Context, DocId, Updates, []).

-spec update(cb_context:context(), kz_json:key(), kz_json:flat_proplist(), kz_json:flat_proplist()) ->
          cb_context:context().
update(Context, DocId, Updates, Creates) ->
    UpdateOptions = [{'update', Updates}
                    ,{'create', Creates}
                    ,{'ensure_saved', 'true'}
                    ],
    case kz_datamgr:update_doc(cb_context:db_name(Context), DocId, UpdateOptions) of
        {'error', Error} ->
            handle_datamgr_errors(Error, DocId, Context);
        {'ok', Saved} ->
            Context1 = handle_datamgr_success(Saved, Context),
            maybe_send_contact_list(Context1),
            maybe_spawn_service_updates(Context1, Saved, cb_context:req_param(Context1, <<"wait_for_service_update">>, 'false')),
            Context1
    end.

%% @equiv save_attachment(DocId, AName, Contents, Context, [])
-spec save_attachment(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
save_attachment(DocId, AName, Contents, Context) ->
    save_attachment(DocId, AName, Contents, Context, []).

%%------------------------------------------------------------------------------
%% @doc Save the Contents as an attachment on the document.
%% @end
%%------------------------------------------------------------------------------
-spec save_attachment(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), cb_context:context(), kz_term:proplist()) ->
          cb_context:context().
save_attachment(DocId, Name, Contents, Context, Options) ->
    Opts1 = case props:get_value('rev', Options) of
                'undefined' ->
                    {'ok', Rev} = kz_datamgr:lookup_doc_rev(cb_context:db_name(Context), DocId),
                    lager:debug("looking up rev for ~s: ~s", [DocId, Rev]),
                    [{'rev', Rev} | Options];
                _O -> Options
            end,

    AName = kz_binary:clean(Name),

    case kz_datamgr:put_attachment(cb_context:db_name(Context), DocId, AName, Contents, Opts1) of
        {'error', 'conflict'=Error} ->
            lager:debug("saving attachment resulted in a conflict, checking for validity"),
            Context1 = load(DocId, Context, [{'use_cache', 'false'} | Options]),
            case kz_doc:attachment(cb_context:doc(Context1), AName) of
                'undefined' ->
                    lager:debug("attachment does appear to be missing, reporting error"),
                    _ = maybe_delete_doc(Context, DocId),
                    handle_datamgr_errors(Error, AName, Context);
                _Attachment ->
                    lager:debug("attachment ~s was in _attachments, considering it successful", [AName]),
                    {'ok', Rev1} = kz_datamgr:lookup_doc_rev(cb_context:db_name(Context), DocId),
                    cb_context:setters(Context
                                      ,[{fun cb_context:set_doc/2, kz_json:new()}
                                       ,{fun cb_context:set_resp_status/2, 'success'}
                                       ,{fun cb_context:set_resp_data/2, kz_json:new()}
                                       ,{fun cb_context:set_resp_etag/2, rev_to_etag(Rev1)}
                                       ])
            end;
        {'error', Error} ->
            lager:debug("error putting attachment into ~s: ~p"
                       ,[cb_context:db_name(Context), Error]
                       ),
            _ = maybe_delete_doc(Context, DocId),
            handle_datamgr_errors(Error, AName, Context);
        {'ok', _Res, _Params} ->
            lager:debug("saved attachment ~s to doc ~s to db ~s"
                       ,[AName, DocId, cb_context:db_name(Context)]
                       ),
            lager:debug("attachment params: ~p", [_Params]),
            handle_saved_attachment(Context, DocId);
        {'ok', _Res} ->
            lager:debug("saved attachment ~s to doc ~s to db ~s"
                       ,[AName, DocId, cb_context:db_name(Context)]
                       ),
            handle_saved_attachment(Context, DocId)
    end.

handle_saved_attachment(Context, DocId) ->
    load(DocId, Context).

-spec maybe_delete_doc(cb_context:context(), kz_term:ne_binary()) ->
          {'ok', _} |
          {'error', any()}.
maybe_delete_doc(Context, DocId) ->
    AccountDb = cb_context:db_name(Context),
    case kz_datamgr:open_doc(AccountDb, DocId) of
        {'error', _}=Error -> Error;
        {'ok', JObj} ->
            case kz_doc:attachments(JObj) of
                'undefined' -> kz_datamgr:del_doc(AccountDb, JObj);
                _Attachments -> {'ok', 'non_empty'}
            end
    end.

%% @equiv delete(Context, cb_context:should_soft_delete(Context))
-spec delete(cb_context:context()) -> cb_context:context().
delete(Context) ->
    delete(Context, cb_context:should_soft_delete(Context)).

%%------------------------------------------------------------------------------
%% @doc This function will attempt to remove a document from the database.
%% If the second argument is `true' this is preformed as a soft-delete and enforced
%% by the views. Clean up process remove old data based on the delete
%% flag and last modified date.
%% If the second argument is `false', the document is hard delete from database.
%%
%% By default `should_soft_delete' field in context record is set to `true', the
%% document is soft-deleted.
%% @end
%%------------------------------------------------------------------------------
-spec delete(cb_context:context(), boolean()) -> cb_context:context().
delete(Context, ?SOFT_DELETE) ->
    Doc = cb_context:doc(Context),
    lager:info("soft-deleting doc ~s", [kz_doc:id(Doc)]),
    case kz_datamgr:lookup_doc_rev(cb_context:db_name(Context), kz_doc:id(Doc)) of
        {'ok', Rev}   -> soft_delete(Context, Rev);
        {'error', _E} -> soft_delete(Context, kz_doc:revision(Doc))
    end;
delete(Context, ?HARD_DELETE) ->
    Doc = cb_context:doc(Context),
    lager:info("hard-deleting doc ~s", [kz_doc:id(Doc)]),
    do_delete(Context, Doc, fun kz_datamgr:del_doc/2).

-spec soft_delete(cb_context:context(), kz_term:api_binary()) -> cb_context:context().
soft_delete(Context, Rev) ->
    lager:debug("soft deleting with rev ~s", [Rev]),
    JObj1 = lists:foldl(fun({F, V}, J) -> F(J, V) end
                       ,update_pvt_parameters(cb_context:doc(Context), Context)
                       ,[{fun kz_doc:set_soft_deleted/2, 'true'}
                        ,{fun kz_doc:set_revision/2, Rev}
                        ]),
    do_delete(Context, JObj1, fun kz_datamgr:save_doc/2).

-type delete_fun() :: fun((kz_term:ne_binary(), kz_json:object() | kz_term:ne_binary()) ->
                                 {'ok', kz_json:object() | kz_json:objects()} |
                                 kz_datamgr:data_error()).

-spec do_delete(cb_context:context(), kz_json:object(), delete_fun()) ->
          cb_context:context().
do_delete(Context, JObj, CouchFun) ->
    case CouchFun(cb_context:db_name(Context), JObj) of
        {'error', 'not_found'} ->
            lager:debug("doc ~s wasn't found in ~s, not deleting"
                       ,[kz_doc:id(JObj), cb_context:db_name(Context)]
                       ),
            handle_datamgr_success(JObj, Context);
        {'error', Error} ->
            DocId = kz_doc:id(JObj),
            handle_datamgr_errors(Error, DocId, Context);
        {'ok', _} ->
            lager:debug("'deleted' ~s from ~s using ~p"
                       ,[kz_doc:id(JObj), cb_context:db_name(Context), CouchFun]
                       ),
            Context1 = handle_datamgr_success(JObj, Context),
            _ = case kz_doc:type(JObj) =/= <<"account">> of
                    'true' ->
                        maybe_send_contact_list(Context1),
                        maybe_spawn_service_updates(Context1, [], cb_context:req_param(Context1, <<"wait_for_service_update">>, 'false'));
                    'false' -> lager:debug("not calling services/provisioner routines for deleted account")
                end,
            Context1
    end.

%%------------------------------------------------------------------------------
%% @doc This function will attempt to remove an attachment from a document.
%% Unlike the delete function, this is NOT a soft-delete.
%%
%% Failure here returns 500 or 503.
%% @end
%%------------------------------------------------------------------------------
-spec delete_attachment(kz_term:ne_binary(), kz_term:ne_binary(), cb_context:context()) ->
          cb_context:context().
delete_attachment(DocId, AName, Context) ->
    case kz_datamgr:delete_attachment(cb_context:db_name(Context), DocId, AName) of
        {'error', 'not_found'} -> handle_datamgr_success(kz_json:new(), Context);
        {'error', Error} ->
            lager:debug("failed to delete attachment: ~p", [Error]),
            handle_datamgr_errors(Error, AName, Context);
        {'ok', _} ->
            lager:debug("deleted attachment ~s from doc ~s from ~s"
                       ,[AName, DocId, cb_context:db_name(Context)]
                       ),
            handle_datamgr_success(kz_json:new(), Context)
    end.

%%------------------------------------------------------------------------------
%% @doc This function will attempt to convert a revision tag on the provided
%% document into a usable `ETag' for the response.
%% @end
%%------------------------------------------------------------------------------
-spec rev_to_etag(kz_json:object() | kz_json:objects() | kz_term:ne_binary()) ->
          'automatic' | kz_term:api_string().
rev_to_etag([_|_])-> 'automatic';
rev_to_etag([]) -> 'undefined';
rev_to_etag(Rev) when is_binary(Rev) -> kz_term:to_list(Rev);
rev_to_etag(JObj) ->
    case kz_doc:revision(JObj) of
        'undefined' -> 'undefined';
        Rev -> kz_term:to_list(Rev)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update_pagination_envelope_params(cb_context:context(), any(), kz_term:api_non_neg_integer()) ->
          cb_context:context().
update_pagination_envelope_params(Context, StartKey, PageSize) ->
    update_pagination_envelope_params(Context
                                     ,StartKey
                                     ,PageSize
                                     ,'undefined'
                                     ,cb_context:should_paginate(Context)
                                     ).

-spec update_pagination_envelope_params(cb_context:context(), any(), kz_term:api_non_neg_integer(), kz_term:api_binary()) ->
          cb_context:context().
update_pagination_envelope_params(Context, StartKey, PageSize, NextStartKey) ->
    update_pagination_envelope_params(Context
                                     ,StartKey
                                     ,PageSize
                                     ,NextStartKey
                                     ,cb_context:should_paginate(Context)
                                     ).

-spec update_pagination_envelope_params(cb_context:context(), any(), kz_term:api_non_neg_integer(), kz_term:api_binary(), boolean()) ->
          cb_context:context().
update_pagination_envelope_params(Context, _StartKey, _PageSize, _NextStartKey, 'false') ->
    lager:debug("pagination disabled, removing resp envelope keys"),
    RespEnvelope = kz_json:delete_keys([<<"start_key">>
                                       ,<<"page_size">>
                                       ,<<"next_start_key">>
                                       ]
                                      ,cb_context:resp_envelope(Context)
                                      ),
    cb_context:set_resp_envelope(Context, RespEnvelope);

update_pagination_envelope_params(Context, StartKey, PageSize, NextStartKey, 'true') ->
    RespEnvelope = cb_context:resp_envelope(Context),
    CurrentPageSize = kz_json:get_integer_value(<<"page_size">>, RespEnvelope, 0),
    NewRespEnvelope = kz_json:set_values(props:filter_undefined(
                                           [{<<"start_key">>, StartKey}
                                           ,{<<"page_size">>, PageSize + CurrentPageSize}
                                           ,{<<"next_start_key">>, NextStartKey}
                                           ])
                                        ,RespEnvelope
                                        ),
    cb_context:set_resp_envelope(Context, NewRespEnvelope).

-spec handle_datamgr_pagination_success(kz_json:objects(), load_view_params()) ->
          cb_context:context().
%% If v1, just append results and try next database
handle_datamgr_pagination_success([]
                                 ,#load_view_params{dbs=[_Db|Dbs]
                                                   ,should_paginate='false'
                                                   } = LVPs
                                 ) ->
    lager:info("no results left, trying next DB"),
    load_view(LVPs#load_view_params{dbs=Dbs});

%% if no results from this db, go to next db (if any)
handle_datamgr_pagination_success([]
                                 ,#load_view_params{context = Context
                                                   ,start_key = StartKey
                                                   ,dbs=[_Db|Dbs]
                                                   } = LVPs
                                 ) ->
    load_view(LVPs#load_view_params{context = update_pagination_envelope_params(Context, StartKey, 0)
                                   ,dbs=Dbs
                                   });

handle_datamgr_pagination_success([_|_]=JObjs
                                 ,#load_view_params{context = Context
                                                   ,page_size = CurrentPageSize
                                                   ,should_paginate = ShouldPaginate
                                                   ,start_key = StartKey
                                                   ,filter_fun = FilterFun
                                                   ,direction = Direction
                                                   ,dbs = [_|Dbs]
                                                   } = LVPs
                                 ) ->
    PageSize = length(JObjs),
    try lists:split(CurrentPageSize, JObjs) of
        {Results, []} ->
            %% exhausted this db, but may need more from Dbs to fulfill PageSize
            Filtered = apply_filter(FilterFun, Results, Context, Direction),
            UpdatedContext = update_pagination_envelope_params(Context, StartKey, PageSize),
            NewContext = cb_context:set_doc(UpdatedContext, Filtered ++ cb_context:doc(Context)),
            load_view(LVPs#load_view_params{context = NewContext
                                           ,page_size = next_page_size(CurrentPageSize, PageSize, ShouldPaginate)
                                           ,dbs = Dbs
                                           });
        {Results, [NextJObj]} ->
            %% Current db may have more results to give
            NextStartKey = kz_json:get_value(<<"key">>, NextJObj),
            Filtered = apply_filter(FilterFun, Results, Context, Direction),
            FilteredCount = length(Filtered),
            lager:debug("next start key: ~p", [NextStartKey]),
            lager:debug("page size: ~p filtered: ~p", [PageSize, FilteredCount]),
            UpdatedContext = update_pagination_envelope_params(Context, StartKey, PageSize, NextStartKey),
            NewContext = cb_context:set_doc(UpdatedContext, Filtered ++ cb_context:doc(Context)),
            load_view(LVPs#load_view_params{context = NewContext
                                           ,page_size = next_page_size(CurrentPageSize, FilteredCount, ShouldPaginate)
                                           ,start_key = NextStartKey
                                           })
    catch
        'error':'badarg' ->
            Filtered = apply_filter(FilterFun, JObjs, Context, Direction),
            FilteredCount = length(Filtered),
            lager:debug("page size: ~p filtered: ~p", [PageSize, FilteredCount]),
            UpdatedContext = update_pagination_envelope_params(Context, StartKey, FilteredCount),
            NewContext = cb_context:set_doc(UpdatedContext, Filtered ++ cb_context:doc(Context)),
            load_view(LVPs#load_view_params{context = NewContext
                                           ,page_size = next_page_size(CurrentPageSize, FilteredCount, ShouldPaginate)
                                           ,dbs = Dbs
                                           })
    end.

next_page_size(CurrentPageSize, _FetchedPageSize, 'false') ->
    CurrentPageSize;
next_page_size(CurrentPageSize, FetchedPageSize, 'true') ->
    CurrentPageSize - FetchedPageSize.

-type filter_fun() :: fun((kz_json:object(), kz_json:objects()) -> kz_json:objects()) |
                      fun((cb_context:context(), kz_json:object(), kz_json:objects()) -> kz_json:objects()) |
                      'undefined'.

-spec apply_filter(filter_fun(), kz_json:objects(), cb_context:context(), direction()) ->
          kz_json:objects().
apply_filter(FilterFun, JObjs, Context, Direction) ->
    apply_filter(FilterFun, JObjs, Context, Direction, crossbar_filter:is_defined(Context)).

-spec apply_filter(filter_fun(), kz_json:objects(), cb_context:context(), direction(), boolean()) ->
          kz_json:objects().
apply_filter(FilterFun, JObjs, Context, Direction, HasQSFilter) ->
    lager:debug("applying filter fun: ~p, qs filter: ~p to dir ~p", [FilterFun, HasQSFilter, Direction]),
    Filtered0 = [JObj
                 || JObj <- JObjs,
                    crossbar_filter:by_doc(kz_json:get_value(<<"doc">>, JObj), Context, HasQSFilter)
                ],
    Filtered = maybe_apply_custom_filter(Context, FilterFun, Filtered0),
    lager:debug("filter resulted in ~p out of ~p objects", [length(Filtered), length(JObjs)]),
    case Direction of
        'ascending' -> Filtered;
        'descending' -> lists:reverse(Filtered)
    end.

-spec maybe_apply_custom_filter(cb_context:context(), filter_fun(), kz_json:objects()) -> kz_json:objects().
maybe_apply_custom_filter(_Context, 'undefined', JObjs) -> JObjs;
maybe_apply_custom_filter(Context, FilterFun, JObjs) ->
    Fun = case is_function(FilterFun, 2) of
              'true' -> FilterFun;
              'false' -> fun(J, Acc) -> FilterFun(Context, J, Acc) end
          end,
    [JObj
     || JObj <- lists:foldl(Fun, [], JObjs),
        not kz_term:is_empty(JObj)
    ].

-spec handle_datamgr_success(kz_json:json_term(), cb_context:context()) -> cb_context:context().
handle_datamgr_success([], Context) ->
    RespEnv = kz_json:set_value(<<"page_size">>, 0, cb_context:resp_envelope(Context)),
    handle_thing_success([], cb_context:set_resp_envelope(Context, RespEnv));
handle_datamgr_success([JObj|_]=JObjs, Context) ->
    case kz_json:is_json_object(JObj) of
        'true' -> handle_json_success(JObjs, Context);
        'false' -> handle_thing_success(JObjs, Context)
    end;
handle_datamgr_success(JObj, Context) ->
    case kz_json:is_json_object(JObj) of
        'true' -> handle_json_success(JObj, Context);
        'false' -> handle_thing_success(JObj, Context)
    end.

-spec handle_thing_success(kz_json:json_term(), cb_context:context()) -> cb_context:context().
handle_thing_success(Thing, Context) ->
    cb_context:setters(Context
                      ,[{fun cb_context:set_doc/2, Thing}
                       ,{fun cb_context:set_resp_status/2, 'success'}
                       ,{fun cb_context:set_resp_data/2, Thing}
                       ,{fun cb_context:set_resp_etag/2, 'undefined'}
                       ]).

-spec handle_json_success(kz_json:object() | kz_json:objects(), cb_context:context()) ->
          cb_context:context().
handle_json_success(JObj, Context) ->
    handle_json_success(JObj, Context, cb_context:req_verb(Context)).

-spec public_and_read_only(kz_json:object()) -> kz_json:object().
public_and_read_only(JObj) ->
    Public = kz_doc:public_fields(JObj),
    case kz_json:get_json_value(<<"_read_only">>, JObj) of
        'undefined' -> Public;
        ReadOnly -> kz_json:set_value(<<"_read_only">>, ReadOnly, Public)
    end.

-spec add_location_header(kz_json:object(), map()) -> map().
add_location_header(JObj, RHs) ->
    maps:put(<<"location">>, kz_doc:id(JObj), RHs).

-spec handle_json_success(kz_json:object() | kz_json:objects(), cb_context:context(), http_method()) ->
          cb_context:context().
handle_json_success(JObjs, Context, ?HTTP_PUT) when is_list(JObjs) ->
    RespData = [public_and_read_only(JObj)
                || JObj <- JObjs,
                   not kz_doc:is_soft_deleted(JObj)
               ],
    RespHeaders = lists:foldl(fun add_location_header/2
                             ,cb_context:resp_headers(Context)
                             ,JObjs
                             ),
    cb_context:setters(Context
                      ,[{fun cb_context:set_doc/2, JObjs}
                       ,{fun cb_context:set_resp_status/2, 'success'}
                       ,{fun cb_context:set_resp_data/2, RespData}
                       ,{fun cb_context:set_resp_etag/2, rev_to_etag(JObjs)}
                       ,{fun cb_context:set_resp_headers/2, RespHeaders}
                       ]);
handle_json_success(JObjs, Context, _Verb) when is_list(JObjs) ->
    RespData = [public_and_read_only(JObj)
                || JObj <- JObjs,
                   not kz_doc:is_soft_deleted(JObj)
               ],
    RespEnv = kz_json:set_value(<<"page_size">>, length(JObjs), cb_context:resp_envelope(Context)),
    cb_context:setters(Context
                      ,[{fun cb_context:set_doc/2, JObjs}
                       ,{fun cb_context:set_resp_status/2, 'success'}
                       ,{fun cb_context:set_resp_data/2, RespData}
                       ,{fun cb_context:set_resp_etag/2, rev_to_etag(JObjs)}
                       ,{fun cb_context:set_resp_envelope/2, RespEnv}
                       ]);
handle_json_success(JObj, Context, ?HTTP_PUT) ->
    RespHeaders = add_location_header(JObj, cb_context:resp_headers(Context)),
    cb_context:setters(Context
                      ,[{fun cb_context:set_doc/2, JObj}
                       ,{fun cb_context:set_resp_status/2, 'success'}
                       ,{fun cb_context:set_resp_data/2, public_and_read_only(JObj)}
                       ,{fun cb_context:set_resp_etag/2, rev_to_etag(JObj)}
                       ,{fun cb_context:set_resp_headers/2, RespHeaders}
                       ]);
handle_json_success(JObj, Context, ?HTTP_DELETE) ->
    Public = public_and_read_only(JObj),
    RespJObj = kz_json:set_value([<<"_read_only">>, <<"deleted">>], 'true', Public),
    cb_context:setters(Context
                      ,[{fun cb_context:set_doc/2, JObj}
                       ,{fun cb_context:set_resp_status/2, 'success'}
                       ,{fun cb_context:set_resp_data/2, RespJObj}
                       ,{fun cb_context:set_resp_etag/2, rev_to_etag(JObj)}
                       ]);
handle_json_success(JObj, Context, _Verb) ->
    cb_context:setters(Context
                      ,[{fun cb_context:set_doc/2, JObj}
                       ,{fun cb_context:set_resp_status/2, 'success'}
                       ,{fun cb_context:set_resp_data/2, public_and_read_only(JObj)}
                       ,{fun cb_context:set_resp_etag/2, rev_to_etag(JObj)}
                       ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_datamgr_errors(kz_datamgr:data_errors(), kz_term:api_ne_binary() | kz_term:api_ne_binaries(), cb_context:context()) ->
          cb_context:context().
handle_datamgr_errors('invalid_db_name', _, Context) ->
    lager:debug("datastore ~s not_found", [cb_context:db_name(Context)]),
    cb_context:add_system_error('datastore_missing', kz_json:from_list([{<<"cause">>, cb_context:db_name(Context)}]), Context);
handle_datamgr_errors('db_not_reachable', _DocId, Context) ->
    lager:debug("operation on doc ~s from ~s failed: db_not_reachable", [_DocId, cb_context:db_name(Context)]),
    cb_context:add_system_error('datastore_unreachable', Context);
handle_datamgr_errors('not_found', DocId, Context) ->
    lager:debug("operation on doc ~s from ~s failed: not_found", [DocId, cb_context:db_name(Context)]),
    cb_context:add_system_error('bad_identifier', kz_json:from_list([{<<"cause">>, DocId}]),  Context);
handle_datamgr_errors('conflict', DocId, Context) ->
    lager:debug("failed to update doc ~s in ~s: conflicts", [DocId, cb_context:db_name(Context)]),
    cb_context:add_system_error('datastore_conflict', Context);
handle_datamgr_errors('invalid_view_name', View, Context) ->
    lager:debug("loading view ~s from ~s failed: invalid view", [View, cb_context:db_name(Context)]),
    cb_context:add_system_error('datastore_missing_view', kz_json:from_list([{<<"cause">>, kz_term:to_binary(View)}]), Context);
handle_datamgr_errors(Else, _View, Context) ->
    lager:debug("operation failed: ~p on ~p", [Else, _View]),
    try kz_term:to_binary(Else) of
        Reason -> cb_context:add_system_error('datastore_fault', kz_json:from_list([{<<"cause">>, Reason}]), Context)
    catch
        _:_ -> cb_context:add_system_error('datastore_fault', Context)
    end.

%%------------------------------------------------------------------------------
%% @doc This function is used to update the private timestamps, and db
%% parameters on all crossbar documents.
%% @end
%%------------------------------------------------------------------------------
-spec update_pvt_parameters(kz_json:object() | kz_json:objects(), cb_context:context()) ->
          kz_json:object() | kz_json:objects().
update_pvt_parameters(JObjs, Context) when is_list(JObjs) ->
    [update_pvt_parameters(JObj, Context) || JObj <- JObjs];
update_pvt_parameters(JObj0, Context) ->
    Updates = pvt_updates(JObj0, Context),
    kz_json:set_values(Updates, JObj0).

-spec pvt_updates(kz_json:object(), cb_context:context()) -> kz_json:json_proplist().
pvt_updates(JObj, Context) ->
    F = fun(Fun, Updates) -> apply_pvt_fun(Fun, JObj, Updates, Context) end,
    lists:foldl(F, [], ?PVT_FUNS).

-type pvt_fun() :: fun((kz_json:object(), kz_json:json_proplist(), cb_context:context()) -> kz_json:json_proplist()).
-spec apply_pvt_fun(pvt_fun(), kz_json:object(), kz_json:json_proplist(), cb_context:context()) -> kz_json:json_proplist().
apply_pvt_fun(Fun, JObj, Updates, Context) ->
    Fun(JObj, Updates, Context).

-spec add_pvt_vsn(kz_json:object(), kz_json:json_proplist(), cb_context:context()) -> kz_json:json_proplist().
add_pvt_vsn(JObj, Updates, _) ->
    case kz_doc:vsn(JObj) of
        'undefined' -> [{kz_doc:path_vsn(), ?CROSSBAR_DOC_VSN} | Updates];
        _ -> Updates
    end.

-spec add_pvt_account_db(kz_json:object(), kz_json:json_proplist(), cb_context:context()) ->
          kz_json:json_proplist().
add_pvt_account_db(JObj, Updates, Context) ->
    %% if no account db and the request is for an account, set db
    case kz_doc:account_db(JObj) =:= 'undefined' of
        'false' -> Updates;
        'true' ->
            case cb_context:db_name(Context) of
                'undefined' -> Updates;
                AccountDb -> [{kz_doc:path_account_db(), AccountDb} | Updates]
            end
    end.

-spec add_pvt_account_id(kz_json:object(), kz_json:json_proplist(), cb_context:context()) ->
          kz_json:json_proplist().
add_pvt_account_id(JObj, Updates, Context) ->
    case kz_doc:account_id(JObj) =:= 'undefined' of
        'false' -> Updates;
        'true' ->
            case cb_context:account_id(Context) of
                'undefined' -> Updates;
                AccountId -> [{kz_doc:path_account_id(), AccountId} | Updates]
            end
    end.

-spec add_pvt_created(kz_json:object(), kz_json:json_proplist(), cb_context:context()) ->
          kz_json:json_proplist().
add_pvt_created(JObj, Updates, _Context) ->
    case kz_doc:revision(JObj) =:= 'undefined' of
        'true' -> [{kz_doc:path_created(), kz_time:now_s()} | Updates];
        'false' -> Updates
    end.

-spec add_pvt_modified(kz_json:object(), kz_json:json_proplist(), cb_context:context()) ->
          kz_json:json_proplist().
add_pvt_modified(_JObj, Updates, _Context) ->
    [{kz_doc:path_modified(), kz_time:now_s()} | Updates].

-spec add_pvt_request_id(kz_json:object(), kz_json:json_proplist(), cb_context:context()) ->
          kz_json:json_proplist().
add_pvt_request_id(_JObj, Updates, Context) ->
    [{<<"pvt_request_id">>, cb_context:req_id(Context)} | Updates].

%%------------------------------------------------------------------------------
%% @doc This function is used to update the private to set account ID, user ID
%% and original account ID and user ID (if the authenticated user is masqueraded).
%%
%% This is do the system administrator can track who has changed the
%% document recently.
%% @end
%%------------------------------------------------------------------------------
-spec add_pvt_auth(kz_json:object(), kz_json:json_proplist(), cb_context:context()) ->
          kz_json:json_proplist().
add_pvt_auth(_JObj, Updates, Context) ->
    case cb_context:is_authenticated(Context) of
        'false' ->
            [{<<"pvt_is_authenticated">>, 'false'} | Updates];
        'true' ->
            AuthDoc = cb_context:auth_doc(Context),
            [{<<"pvt_is_authenticated">>, 'true'}
            ,{<<"pvt_auth_account_id">>, cb_context:auth_account_id(Context)}
            ,{<<"pvt_auth_user_id">>, cb_context:auth_user_id(Context)}
            ,{<<"pvt_original_auth_account_id">>, kz_json:get_value(<<"original_account_id">>, AuthDoc)}
            ,{<<"pvt_original_auth_owner_id">>, kz_json:get_value(<<"original_owner_id">>, AuthDoc)}
             | Updates
            ]
    end.

-spec add_pvt_alphanum_name(kz_json:object(), kz_json:json_proplist(), cb_context:context()) ->
          kz_json:json_proplist().
add_pvt_alphanum_name(JObj, Updates, Context) ->
    add_pvt_alphanum_name(JObj, Updates, Context, kz_json:get_value(<<"name">>, JObj), kz_doc:type(JObj)).

-spec add_pvt_alphanum_name(kz_json:object(), kz_json:json_proplist(), cb_context:context(), kz_term:api_binary(), kz_term:api_ne_binary()) ->
          kz_json:json_proplist().
add_pvt_alphanum_name(JObj, Updates, Context, 'undefined', <<"user">>) ->
    Name = case {kz_json:get_ne_binary_value(<<"first_name">>, JObj)
                ,kz_json:get_ne_binary_value(<<"last_name">>, JObj)
                }
           of
               {'undefined', 'undefined'} -> 'undefined';
               {'undefined', LastName} -> LastName;
               {FirstName, 'undefined'} -> FirstName;
               {FirstName, LastName} -> <<FirstName/binary, LastName/binary>>
           end,
    add_pvt_alphanum_name(JObj, Updates, Context, Name, 'undefined');
add_pvt_alphanum_name(_JObj, Updates, _Context, 'undefined', _Type) ->
    Updates;
add_pvt_alphanum_name(_JObj, Updates, _Context, Name, _Type)
  when is_binary(Name) ->
    [{<<"pvt_alphanum_name">>, cb_modules_util:normalize_alphanum_name(Name)}
     | Updates
    ];
add_pvt_alphanum_name(_JObj, Updates, _Context, _Name, _Type) ->
    Updates.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec extract_included_docs(cb_context:context(), kz_json:objects()) ->
          {kz_json:objects(), cb_context:context()}.
extract_included_docs(Context, JObjs) ->
    lists:foldl(fun extract_included_docs_fold/2, {[], Context}, JObjs).

extract_included_docs_fold(JObj, {Docs, Context}) ->
    case kz_json:get_ne_value(<<"doc">>, JObj) of
        'undefined' ->
            Reason = kz_json:get_ne_value(<<"error">>, JObj),
            ID = kz_json:get_ne_value(<<"key">>, JObj),
            {Docs, handle_datamgr_errors(kz_term:to_atom(Reason, 'true'), ID, Context)};
        Doc ->
            {[Doc|Docs], Context}
    end.
