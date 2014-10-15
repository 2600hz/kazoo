%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors:
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_notifications).

-export([init/0
         ,authorize/1
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,content_types_provided/2
         ,content_types_accepted/2
         ,validate/1, validate/2
         ,put/1
         ,post/2
         ,delete/2
        ]).

-include("../crossbar.hrl").

-define(NOTIFICATION_MIME_TYPES, [{<<"text">>, <<"html">>}
                                  ,{<<"text">>, <<"plain">>}
                                 ]).
-define(CB_LIST, <<"notifications/crossbar_listing">>).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Initializes the bindings this module will respond to.
%% @end
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.notifications">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.notifications">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.notifications">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.content_types_accepted.notifications">>, ?MODULE, 'content_types_accepted'),
    _ = crossbar_bindings:bind(<<"*.validate.notifications">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.notifications">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.notifications">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.notifications">>, ?MODULE, 'delete').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    authorize(Context, cb_context:auth_account_id(Context), cb_context:req_nouns(Context)).

authorize(_Context, AuthAccountId, [{<<"notifications">>, _Id}
                                    ,{<<"accounts">>, AccountId}
                                   ]) ->
    lager:debug("maybe authz for ~s to modify ~s in ~s", [AuthAccountId, _Id, AccountId]),
    wh_services:is_reseller(AuthAccountId) andalso
        wh_util:is_in_account_hierarchy(AuthAccountId, AccountId, 'true');
authorize(Context, AuthAccountId, [{<<"notifications">>, []}]) ->
    lager:debug("checking authz on system request to /"),
    {'ok', MasterAccountId} = whapps_util:get_master_account_id(),
    cb_context:req_verb(Context) =:= ?HTTP_GET
        orelse AuthAccountId =:= MasterAccountId;
authorize(_Context, AuthAccountId, [{<<"notifications">>, _Id}]) ->
    lager:debug("maybe authz for system notification ~s", [_Id]),
    {'ok', MasterAccountId} = whapps_util:get_master_account_id(),
    AuthAccountId =:= MasterAccountId;
authorize(_Context, _AuthAccountId, _Nouns) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /notifications => []
%%    /notifications/foo => [<<"foo">>]
%%    /notifications/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_Id) -> 'true'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add content types accepted and provided by this module
%%
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided(cb_context:context(), path_token()) ->
                                    cb_context:context().
-spec content_types_provided(cb_context:context(), path_token(), http_method()) ->
                                    cb_context:context().
content_types_provided(Context, Id) ->
    content_types_provided(Context, db_id(Id), cb_context:req_verb(Context)).

content_types_provided(Context, Id, ?HTTP_GET) ->
    Context1 = read(Context, Id),
    case cb_context:resp_status(Context1) of
        'success' ->
            case wh_json:get_value(<<"_attachments">>, cb_context:doc(Context1)) of
                'undefined' -> Context;
                Attachments ->
                    ContentTypes =
                        wh_json:foldl(fun(_Name, Attachment, Acc) ->
                                         [list_to_tuple(
                                            binary:split(wh_json:get_value(<<"content_type">>, Attachment), <<"/">>)
                                           )
                                          | Acc
                                         ]
                                      end, [], Attachments),
                    cb_context:set_content_types_provided(Context, [{'to_binary', ContentTypes}
                                                                    ,{'to_json', ?JSON_CONTENT_TYPES}
                                                                   ])
            end;
        _Status -> Context1
    end;
content_types_provided(Context, _Id, _Verb) ->
    Context.

-spec content_types_accepted_for_upload(cb_context:context(), http_method()) ->
                                               cb_context:context().
content_types_accepted(Context, _Id) ->
    content_types_accepted_for_upload(Context, cb_context:req_verb(Context)).

content_types_accepted_for_upload(Context, ?HTTP_POST) ->
    CTA = [{'from_binary', ?NOTIFICATION_MIME_TYPES}
           ,{'from_json', ?JSON_CONTENT_TYPES}
          ],
    cb_context:set_content_types_accepted(Context, CTA);
content_types_accepted_for_upload(Context, _Verb) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /notifications mights load a list of skel objects
%% /notifications/123 might load the skel object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context) ->
    validate_notifications(Context, cb_context:req_verb(Context)).

validate(Context, Id) ->
    validate_notification(Context, db_id(Id), cb_context:req_verb(Context)).

-spec validate_notifications(cb_context:context(), http_method()) -> cb_context:context().
validate_notifications(Context, ?HTTP_GET) ->
    summary(Context);
validate_notifications(Context, ?HTTP_PUT) ->
    create(Context).

-spec validate_notification(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_notification(Context, Id, ?HTTP_GET) ->
    maybe_read(Context, Id);
validate_notification(Context, Id, ?HTTP_POST) ->
    maybe_update(Context, Id);
validate_notification(Context, Id, ?HTTP_DELETE) ->
    read(Context, Id).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    Context1 = crossbar_doc:save(Context),
    case cb_context:resp_status(Context1) of
        'success' -> leak_doc_id(Context1);
        _Status -> Context1
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, Id) ->
    case cb_context:req_files(Context) of
        [] -> do_post(Context);
        [{_FileName, FileJObj}] ->
            lager:debug("POST is for an attachment on ~s(~s)", [Id, db_id(Id)]),
            update_template(Context, db_id(Id), FileJObj)
    end.

do_post(Context) ->
    Context1 = crossbar_doc:save(Context),
    case cb_context:resp_status(Context1) of
        'success' -> leak_doc_id(Context1);
        _Status -> Context1
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _Id) ->
    Context1 = crossbar_doc:delete(Context),
    case cb_context:resp_status(Context1) of
        'success' -> leak_doc_id(Context1);
        _Status -> Context1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create(cb_context:context()) -> cb_context:context().
create(Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"notifications">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
maybe_read(Context, Id) ->
    case props:get_value(<<"accept">>, cb_context:req_headers(Context)) of
        'undefined' -> read(Context, Id);
        <<"application/json">> -> read(Context, Id);
        <<"application/x-json">> -> read(Context, Id);
        Accept -> maybe_read_template(read(Context, Id), Id, Accept)
    end.

-spec read(cb_context:context(), ne_binary()) -> cb_context:context().
read(Context, Id) ->
    Context1 =
        case cb_context:account_db(Context) of
            'undefined' ->
                {'ok', MasterAccountDb} = whapps_util:get_master_account_db(),
                crossbar_doc:load(Id, cb_context:set_account_db(Context, MasterAccountDb));
            _AccountDb -> crossbar_doc:load(Id, Context)
        end,
    case cb_context:resp_status(Context1) of
        'success' -> read_success(Context1);
        _Status -> Context1
    end.

-spec read_success(cb_context:context()) -> cb_context:context().
read_success(Context) ->
    leak_attachments(
      leak_doc_id(Context)
     ).

-spec maybe_read_template(cb_context:context(), ne_binary(), ne_binary()) -> cb_context:context().
maybe_read_template(Context, Id, Accept) ->
    case cb_context:resp_status(Context) of
        'success' -> read_template(Context, Id, Accept);
        _Status -> Context
    end.

-spec read_template(cb_context:context(), ne_binary(), ne_binary()) -> cb_context:context().
read_template(Context, Id, Accept) ->
    Doc = cb_context:fetch(Context, 'db_doc'),
    AttachmentName = attachment_name_by_accept(Accept),
    case wh_json:get_value([<<"_attachments">>, AttachmentName], Doc) of
        'undefined' ->
            lager:debug("failed to find attachment ~s in ~s", [AttachmentName, Id]),
            crossbar_util:response_faulty_request(Context);
        Meta ->
            lager:debug("found attachment ~s in ~s", [AttachmentName, Id]),

            cb_context:add_resp_headers(
              crossbar_doc:load_attachment(Id, AttachmentName, Context)
              ,[{<<"Content-Disposition">>, [<<"attachment; filename=">>, resp_id(Id), $., cb_modules_util:content_type_to_extension(Accept)]}
                ,{<<"Content-Type">>, wh_json:get_value(<<"content_type">>, Meta)}
                ,{<<"Content-Length">>, wh_json:get_value(<<"length">>, Meta)}
               ])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing menu document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec maybe_update(cb_context:context(), ne_binary()) -> cb_context:context().
maybe_update(Context, Id) ->
    case cb_context:req_files(Context) of
        [] -> update(Context, Id);
        [{_FileName, FileJObj}] ->
            lager:debug("recv template upload of ~s: ~p", [_FileName, FileJObj]),
            read(Context, Id)
    end.

-spec update(cb_context:context(), ne_binary()) -> cb_context:context().
update(Context, Id) ->
    OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
    cb_context:validate_request_data(<<"notifications">>, Context, OnSuccess).

-spec update_template(cb_context:context(), path_token(), wh_json:object()) ->
                             cb_context:context().
update_template(Context, Id, FileJObj) ->
    Contents = wh_json:get_value(<<"contents">>, FileJObj),
    CT = wh_json:get_value([<<"headers">>, <<"content_type">>], FileJObj),
    lager:debug("file content type for ~s: ~s", [Id, CT]),
    Opts = [{'headers', [{'content_type', wh_util:to_list(CT)}]}],

    Context1 =
        case cb_context:account_db(Context) of
            'undefined' ->
                {'ok', MasterAccountDb} = whapps_util:get_master_account_db(),
                cb_context:set_account_db(Context, MasterAccountDb);
            _AccountDb -> Context
        end,

    AttachmentName = attachment_name_by_content_type(CT),

    crossbar_doc:save_attachment(
      Id
      ,AttachmentName
      ,Contents
      ,Context1
      ,Opts
     ).

-spec attachment_name_by_content_type(ne_binary()) -> ne_binary().
attachment_name_by_content_type(CT) ->
    <<"template.", (cow_qs:urlencode(CT))/binary>>.

-spec attachment_name_by_accept(ne_binary()) -> ne_binary().
attachment_name_by_accept(CT) ->
    <<"template.", CT/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    case cb_context:account_db(Context) of
        'undefined' -> summary_available(Context);
        _AccountDb ->
            crossbar_doc:load_view(?CB_LIST
                                   ,[]
                                   ,Context
                                   ,fun normalize_view_results/2
                                  )
    end.

-spec summary_available(cb_context:context()) -> cb_context:context().
summary_available(Context) ->
    {'ok', MasterAccountDb} = whapps_util:get_master_account_db(),
    crossbar_doc:load_view(?CB_LIST
                           ,[]
                           ,cb_context:set_account_db(Context, MasterAccountDb)
                           ,fun normalize_available/2
                          ).

-spec normalize_available(wh_json:object(), ne_binaries()) -> ne_binaries().
normalize_available(JObj, Acc) ->
    [wh_json:get_value(<<"key">>, JObj) | Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    DocId = db_id(wh_json:get_value(<<"id">>, cb_context:doc(Context))),
    AccountDb = cb_context:account_db(Context),

    {'ok', MasterAccountDb} = whapps_util:get_master_account_db(),

    case couch_mgr:open_cache_doc(MasterAccountDb, DocId) of
        {'ok', _JObj} ->
            Doc = wh_json:set_values([{<<"pvt_type">>, <<"notification">>}
                                      ,{<<"_id">>, DocId}
                                     ], cb_context:doc(Context)),
            cb_context:set_doc(Context, Doc);
        {'error', 'not_found'} when AccountDb =/= 'undefined', AccountDb =/= MasterAccountDb ->
            lager:debug("doc ~s does not exist in ~s, not letting ~s create it", [DocId, MasterAccountDb, AccountDb]),
            crossbar_util:response_bad_identifier(resp_id(DocId), Context);
        {'error', 'not_found'} ->
            lager:debug("this will create a new template in ~s", [MasterAccountDb]),
            Doc = wh_json:set_values([{<<"pvt_type">>, <<"notification">>}
                                      ,{<<"_id">>, DocId}
                                     ], cb_context:doc(Context)),
            cb_context:setters(Context
                               ,[{fun cb_context:set_doc/2, Doc}
                                 ,{fun cb_context: set_account_db/2, MasterAccountDb}
                                ]);
        {'error', _E} ->
            lager:debug("error fetching ~s from ~s: ~p", [DocId, MasterAccountDb, _E]),
            crossbar_util:response_db_fatal(Context)
    end;
on_successful_validation(Id, Context) ->
    crossbar_doc:load_merge(Id, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec db_id(ne_binary()) -> ne_binary().
db_id(<<"notification.", _/binary>> = Id) -> Id;
db_id(Id) -> <<"notification.", Id/binary>>.

-spec resp_id(ne_binary()) -> ne_binary().
resp_id(<<"notification.", Id/binary>>) -> Id;
resp_id(Id) -> Id.

-spec leak_doc_id(cb_context:context()) -> cb_context:context().
leak_doc_id(Context) ->
    RespData = cb_context:resp_data(Context),
    DocId = wh_json:get_first_defined([<<"_id">>, <<"id">>], RespData),
    cb_context:set_resp_data(Context, wh_json:set_value(<<"id">>, resp_id(DocId), RespData)).

-spec leak_attachments(cb_context:context()) -> cb_context:context().
leak_attachments(Context) ->
    Attachments = wh_json:get_value(<<"_attachments">>, cb_context:fetch(Context, 'db_doc'), wh_json:new()),
    Templates =
        wh_json:foldl(fun(_Attachment, Props, Acc) ->
                              [wh_json:get_value(<<"content_type">>, Props) | Acc]
                      end, [], Attachments),
    cb_context:set_resp_data(Context
                             ,wh_json:set_value(<<"templates">>, Templates, cb_context:resp_data(Context))
                            ).
