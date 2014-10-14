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
    _ = init_db(),

    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.notifications">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.notifications">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.notifications">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.content_types_accepted.notifications">>, ?MODULE, 'content_types_accepted'),
    _ = crossbar_bindings:bind(<<"*.validate.notifications">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.notifications">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.notifications">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.notifications">>, ?MODULE, 'delete').

init_db() ->
    _ = couch_mgr:db_create(?KZ_NOTIFICATIONS_DB),
    couch_mgr:revise_doc_from_file(?KZ_NOTIFICATIONS_DB, 'crossbar', <<"account/notifications.json">>).

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
authorize(Context, _AuthAccountId, [{<<"notifications">>, []}]) ->
    lager:debug("checking authz on system request to /"),
    cb_context:req_verb(Context) =:= ?HTTP_GET
        orelse cb_modules_util:is_superduper_admin(Context);
authorize(Context, _AuthAccountId, [{<<"notifications">>, _Id}]) ->
    lager:debug("maybe authz for system notification ~s", [_Id]),
    cb_modules_util:is_superduper_admin(Context);
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
resource_exists(_) -> 'true'.

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
            case wh_json:get_keys(<<"_attachments">>, cb_context:doc(Context1)) of
                [] -> Context;
                Attachments ->
                    ContentTypes = [list_to_tuple(
                                      binary:split(wh_json:get_value(<<"content_type">>, Attachment), <<"/">>)
                                     )
                                    || Attachment <- Attachments
                                   ],
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

%% validate(Context, Id, ?HTML) ->
%%     case is_authorized(Context) of
%%         'true' ->
%%             lager:debug("uploading hml template to '~s'", [Id]),
%%             validate_template(Context, fix_id(Id), cb_context:req_verb(Context), cb_context:req_files(Context));
%%     'false' ->
%%             cb_context:add_system_error('forbidden', Context)
%%     end;
%% validate(Context, Id, ?TXT) ->
%%     case is_authorized(Context) of
%%         'true' ->
%%             lager:debug("uploading txt template data to '~s'", [Id]),
%%             validate_template(Context, fix_id(Id), cb_context:req_verb(Context), cb_context:req_files(Context));
%%     'false' ->
%%             cb_context:add_system_error('forbidden', Context)
%%     end.

-spec validate_notifications(cb_context:context(), http_method()) -> cb_context:context().
validate_notifications(Context, ?HTTP_GET) ->
    summary(Context);
validate_notifications(Context, ?HTTP_PUT) ->
    create(Context).

-spec validate_notification(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_notification(Context, Id, ?HTTP_GET) ->
    read(Context, Id);
validate_notification(Context, Id, ?HTTP_POST) ->
    update(Context, Id);
validate_notification(Context, Id, ?HTTP_DELETE) ->
    read(Context, Id).

%% -spec validate_template(cb_context:context(), ne_binary(), http_method(), wh_proplist()) -> cb_context:context().
%% validate_template(Context, Id, ?HTTP_GET, _Files) ->
%%     lager:debug("fetch template contents for '~s'", [Id]),
%%     load_template(Id, Context);
%% validate_template(Context, _Id, ?HTTP_POST, []) ->
%%     Message = <<"please provide an template file">>,
%%     cb_context:add_validation_error(<<"file">>, <<"required">>, Message, Context);
%% validate_template(Context, Id, ?HTTP_POST, [{_Filename, File}]) ->
%%     case test_compile_template(File) of
%%         'error' ->
%%             crossbar_util:response('error', <<"Invalid template">>, 400, Context);
%%         'ok' ->
%%             Context1 = read(Id, Context),
%%             case cb_context:resp_status(Context1) of
%%                 'success' ->
%%                     lager:debug("loaded media meta for '~s'", [Id]),
%%                     Context1;
%%                 _Status -> Context1
%%             end
%%     end.

%% -spec test_compile_template(wh_json:object()) -> 'ok' | 'error'.
%% test_compile_template(File) ->
%%     Template = wh_json:get_value(<<"contents">>, File),
%%     % Atom leak !!!!
%%     {_, _, Now} = erlang:now(),
%%     Name = wh_util:to_atom(Now, 'true'),
%%     case erlydtl:compile_template(Template, Name, [{'out_dir', 'false'}]) of
%%         {'ok', CustomTemplate} ->
%%             lager:debug("template compiled successfuly, purging now"),
%%             code:purge(CustomTemplate),
%%             code:delete(CustomTemplate),
%%             'ok';
%%         {'error', _R} ->
%%             lager:error("fail to compile template: ~p", [_R]),
%%             'error';
%%         _E ->
%%             lager:error("fail to compile template"),
%%             'error'
%%     end.

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
        'success' -> put_success(Context1);
        _Status -> Context1
    end.

put_success(Context) ->
    leak_doc_id(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _Id) ->
    Context1 = crossbar_doc:save(Context),
    case cb_context:resp_status(Context1) of
        'success' -> leak_doc_id(Context1);
        _Status -> Context1
    end.

%% post(Context, Id, Format) ->
%%     update_template(Context, fix_id(Id), Format, cb_context:req_files(Context)).

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
-spec read(cb_context:context(), ne_binary()) -> cb_context:context().
read(Context, Id) ->
    Context1 =
        case cb_context:account_db(Context) of
            'undefined' -> crossbar_doc:load(Id, cb_context:set_account_db(Context, ?KZ_NOTIFICATIONS_DB));
            _AccountDb -> crossbar_doc:load(Id, Context)
        end,
    case cb_context:resp_status(Context1) of
        'success' -> read_success(Context1);
        _Status -> Context1
    end.

-spec read_success(cb_context:context()) -> cb_context:context().
read_success(Context) ->
    leak_doc_id(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing menu document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update(cb_context:context(), ne_binary()) -> cb_context:context().
update(Context, Id) ->
    OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
    cb_context:validate_request_data(<<"notifications">>, Context, OnSuccess).

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
    crossbar_doc:load_view(?CB_LIST
                           ,[]
                           ,cb_context:set_account_db(Context, ?KZ_NOTIFICATIONS_DB)
                           ,fun normalize_available/2
                          ).

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

    case couch_mgr:open_cache_doc(?KZ_NOTIFICATIONS_DB, DocId) of
        {'ok', _JObj} ->
            Doc = wh_json:set_values([{<<"pvt_type">>, <<"notification">>}
                                      ,{<<"_id">>, DocId}
                                     ], cb_context:doc(Context)),
            cb_context:set_doc(Context, Doc);
        {'error', 'not_found'} when AccountDb =/= 'undefined', AccountDb =/= ?KZ_NOTIFICATIONS_DB ->
            lager:debug("doc ~s does not exist in ~s", [DocId, ?KZ_NOTIFICATIONS_DB]),
            crossbar_util:response_bad_identifier(resp_id(DocId), Context);
        {'error', 'not_found'} ->
            lager:debug("this will create a new template in ~s", [?KZ_NOTIFICATIONS_DB]),
            Doc = wh_json:set_values([{<<"pvt_type">>, <<"notification">>}
                                      ,{<<"_id">>, DocId}
                                     ], cb_context:doc(Context)),
            cb_context:setters(Context
                               ,[{fun cb_context:set_doc/2, Doc}
                                 ,{fun cb_context: set_account_db/2, ?KZ_NOTIFICATIONS_DB}
                                ]);
        {'error', _E} ->
            lager:debug("error fetching ~s from ~s: ~p", [DocId, ?KZ_NOTIFICATIONS_DB, _E]),
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load the binary attachment of a media doc
%% @end
%%--------------------------------------------------------------------
%% -spec load_template(cb_context:context(), path_token()) -> cb_context:context().
%% load_template(Id, Context) ->
%%     Context1 = read(Id, Context),
%%     case cb_context:resp_status(Context1) of
%%         'success' ->
%%             Meta = wh_json:get_value([<<"_attachments">>], cb_context:doc(Context1), []),
%%             case wh_json:get_keys(Meta) of
%%                 [] -> crossbar_util:response_bad_identifier(Id, Context);
%%                 [Attachment|_] ->
%%                     cb_context:add_resp_headers(
%%                       crossbar_doc:load_attachment(cb_context:doc(Context1), Attachment, Context1)
%%                       ,[{<<"Content-Disposition">>, <<"attachment; filename=", Attachment/binary>>}
%%                         ,{<<"Content-Type">>, wh_json:get_value([Attachment, <<"content_type">>], Meta)}
%%                         ,{<<"Content-Length">>, wh_json:get_value([Attachment, <<"length">>], Meta)}
%%                        ])
%%             end;
%%         _Status -> Context1
%%     end.

%% %%--------------------------------------------------------------------
%% %% @private
%% %% @doc
%% %% Update the binary attachment of a media doc
%% %% @end
%% %%--------------------------------------------------------------------
%% -spec update_template(cb_context:context(), path_token(), path_token(), req_files()) ->
%%                                  cb_context:context().
%% update_template(Context, Id, Format, [{_, FileObj}|_]) ->
%%     Contents = wh_json:get_value(<<"contents">>, FileObj),
%%     CT = wh_json:get_value([<<"headers">>, <<"content_type">>], FileObj),
%%     lager:debug("file content type: ~s", [CT]),
%%     Opts = [{'headers', [{'content_type', wh_util:to_list(CT)}]}],
%%     crossbar_doc:save_attachment(
%%         Id
%%         ,<<"template.", Format/binary>>
%%         ,Contents
%%         ,Context
%%         ,Opts
%%     ).
