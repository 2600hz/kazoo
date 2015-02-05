%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%% Account module
%%%
%%% @end
%%% @contributors
%%%   Jon Blanton <jon@2600hz.com>
%%%-------------------------------------------------------------------
-module(cb_whitelabel).

-export([init/0
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2
         ,resource_exists/0, resource_exists/1, resource_exists/2
         ,authorize/1, authenticate/1
         ,validate/1, validate/2, validate/3
         ,content_types_provided/2, content_types_provided/3
         ,content_types_accepted/2
         ,put/1
         ,post/1, post/2
         ,delete/1
        ]).

-include("../crossbar.hrl").

-define(WHITELABEL_ID, <<"whitelabel">>).
-define(LOGO_REQ, <<"logo">>).
-define(ICON_REQ, <<"icon">>).
-define(WELCOME_REQ, <<"welcome">>).

-define(WHITELABEL_MIME_TYPES, [{<<"image">>, <<"jpg">>}
                                ,{<<"image">>, <<"jpeg">>}
                                ,{<<"image">>, <<"png">>}
                                ,{<<"image">>, <<"gif">>}
                                ,{<<"application">>, <<"base64">>}
                                ,{<<"application">>, <<"x-base64">>}
                               ]).

%% Commonly found ico mime types
-define(WHITELABEL_ICON_MIME_TYPES, [{<<"image">>, <<"ico">>}
                                     ,{<<"image">>, <<"vnd.microsoft.icon">>}
                                     ,{<<"image">>, <<"x-icon">>}
                                     ,{<<"image">>, <<"icon">>}
                                     | ?WHITELABEL_MIME_TYPES
                                    ]).

-define(WHITELABEL_WELCOME_MIME_TYPES, [{<<"text">>, <<"html">>}
                                    ]).

-define(AGG_VIEW_WHITELABEL_DOMAIN, <<"accounts/list_by_whitelabel_domain">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.whitelabel">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.content_types_accepted.whitelabel">>, ?MODULE, 'content_types_accepted'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.whitelabel">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.whitelabel">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.whitelabel">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.whitelabel">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.whitelabel">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.whitelabel">>, ?MODULE, 'delete').

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
    [?HTTP_GET, ?HTTP_PUT, ?HTTP_POST, ?HTTP_DELETE].

allowed_methods(?LOGO_REQ) ->
    [?HTTP_GET, ?HTTP_POST];
allowed_methods(?ICON_REQ) ->
    [?HTTP_GET, ?HTTP_POST];
allowed_methods(?WELCOME_REQ) ->
    [?HTTP_GET, ?HTTP_POST];
allowed_methods(_) ->
    [?HTTP_GET].

allowed_methods(_, ?LOGO_REQ) ->
    [?HTTP_GET];
allowed_methods(_, ?ICON_REQ) ->
    [?HTTP_GET];
allowed_methods(_, ?WELCOME_REQ) ->
    [?HTTP_GET].

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

resource_exists(?LOGO_REQ) -> 'true';
resource_exists(?ICON_REQ) -> 'true';
resource_exists(?WELCOME_REQ) -> 'true';
resource_exists(_) -> 'true'.

resource_exists(_, ?LOGO_REQ) -> 'true';
resource_exists(_, ?WELCOME_REQ) -> 'true';
resource_exists(_, ?ICON_REQ) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    authorize(cb_context:req_nouns(Context), cb_context:req_verb(Context)).

-spec authorize(req_nouns(), http_method()) -> boolean().
authorize([{<<"whitelabel">>, [_]}], ?HTTP_GET) ->
    'true';
authorize([{<<"whitelabel">>, [_ | [?LOGO_REQ]]}], ?HTTP_GET) ->
    'true';
authorize([{<<"whitelabel">>, [_ | [?ICON_REQ]]}], ?HTTP_GET) ->
    'true';
authorize([{<<"whitelabel">>, [_ | [?WELCOME_REQ]]}], ?HTTP_GET) ->
    'true';
authorize(_Nouns, _Verb) ->
    'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) ->
    authenticate(cb_context:req_nouns(Context), cb_context:req_verb(Context)).

-spec authenticate(req_nouns(), http_method()) -> boolean().
authenticate([{<<"whitelabel">>, [_]}], ?HTTP_GET) ->
    'true';
authenticate([{<<"whitelabel">>, [_ | [?LOGO_REQ]]}], ?HTTP_GET) ->
    'true';
authenticate([{<<"whitelabel">>, [_ | [?ICON_REQ]]}], ?HTTP_GET) ->
    'true';
authenticate([{<<"whitelabel">>, [_ | [?WELCOME_REQ]]}], ?HTTP_GET) ->
    'true';
authenticate(_Nouns, _Verb) ->
    'false'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add content types accepted and provided by this module
%%
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided(cb_context:context(), path_token()) ->
                                    cb_context:context().
content_types_provided(Context, AttachType) ->
    content_types_provided_for_attachments(Context, AttachType, cb_context:req_verb(Context)).

-spec content_types_provided_for_attachments(cb_context:context(), path_token(), http_method()) ->
                                    cb_context:context().
content_types_provided_for_attachments(Context, ?LOGO_REQ, ?HTTP_GET) ->
    content_types_provided_for_attachments(Context, ?LOGO_REQ);
content_types_provided_for_attachments(Context, ?ICON_REQ, ?HTTP_GET) ->
    content_types_provided_for_attachments(Context, ?ICON_REQ);
content_types_provided_for_attachments(Context, ?WELCOME_REQ, ?HTTP_GET) ->
    content_types_provided_for_attachments(Context, ?WELCOME_REQ);
content_types_provided_for_attachments(Context, _Type, _Verb) ->
    Context.

-spec content_types_provided_for_attachments(cb_context:context(), ne_binary()) ->
                                                    cb_context:context().
content_types_provided_for_attachments(Context, AttachType) ->
    Context1 = load_whitelabel_meta(Context, ?WHITELABEL_ID),
    case whitelabel_binary_meta(Context1, AttachType) of
        'undefined' -> Context1;
        {_, JObj} -> set_content_type_provided(Context1, JObj)
    end.

-spec content_types_provided(cb_context:context(), path_token(), path_token()) ->
                                    cb_context:context().
content_types_provided(Context, Domain, AttachType) ->
    content_types_provided_for_domain_attachments(Context, Domain, AttachType, cb_context:req_verb(Context)).

-spec content_types_provided_for_domain_attachments(cb_context:context(), path_token(), path_token(), http_method()) ->
                                                           cb_context:context().
content_types_provided_for_domain_attachments(Context, Domain, ?LOGO_REQ, ?HTTP_GET) ->
    content_types_provided_for_domain_attachments(Context, Domain, ?LOGO_REQ);
content_types_provided_for_domain_attachments(Context, Domain, ?ICON_REQ, ?HTTP_GET) ->
    content_types_provided_for_domain_attachments(Context, Domain, ?ICON_REQ);
content_types_provided_for_domain_attachments(Context, Domain, ?WELCOME_REQ, ?HTTP_GET) ->
    content_types_provided_for_domain_attachments(Context, Domain, ?WELCOME_REQ);
content_types_provided_for_domain_attachments(Context, _Domain, _AttachType, _Verb) ->
    Context.

-spec content_types_provided_for_domain_attachments(cb_context:context(), path_token(), path_token()) ->
                                                           cb_context:context().
content_types_provided_for_domain_attachments(Context, Domain, AttachType) ->
    case find_whitelabel_binary_meta(Context, Domain, AttachType) of
        'undefined' -> Context;
        {_, JObj} -> set_content_type_provided(Context, JObj)
    end.

-spec set_content_type_provided(cb_context:context(), wh_json:object()) -> cb_context:context().
set_content_type_provided(Context, JObj) ->
    CT = wh_json:get_value(<<"content_type">>, JObj),
    [Type, SubType] = binary:split(CT, <<"/">>),
    cb_context:set_content_types_provided(Context, [{'to_binary', [{Type, SubType}]}]).

-spec content_types_accepted(cb_context:context(), path_token()) -> cb_context:context().
content_types_accepted(Context, AttachType) ->
    content_types_accepted(Context, AttachType, cb_context:req_verb(Context)).

-spec content_types_accepted(cb_context:context(), path_token(), http_method()) ->
                                    cb_context:context().
content_types_accepted(Context, ?LOGO_REQ, ?HTTP_POST) ->
    CTA = [{'from_binary', ?WHITELABEL_MIME_TYPES}],
    cb_context:set_content_types_accepted(Context, CTA);
content_types_accepted(Context, ?ICON_REQ, ?HTTP_POST) ->
    CTA = [{'from_binary', ?WHITELABEL_ICON_MIME_TYPES}],
    cb_context:set_content_types_accepted(Context, CTA);
content_types_accepted(Context, ?WELCOME_REQ, ?HTTP_POST) ->
    CTA = [{'from_binary', ?WHITELABEL_WELCOME_MIME_TYPES}],
    cb_context:set_content_types_accepted(Context, CTA);
content_types_accepted(Context, _AttachType, _Verb) ->
    Context.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_whitelabel(Context, cb_context:req_verb(Context)).

-spec validate_whitelabel(cb_context:context(), http_method()) ->
                                 cb_context:context().
validate_whitelabel(Context, ?HTTP_GET) ->
    load_whitelabel_meta(Context, ?WHITELABEL_ID);
validate_whitelabel(Context, ?HTTP_PUT) ->
    validate_request(Context, 'undefined');
validate_whitelabel(Context, ?HTTP_POST) ->
    validate_request(Context, ?WHITELABEL_ID);
validate_whitelabel(Context, ?HTTP_DELETE) ->
    load_whitelabel_meta(Context, ?WHITELABEL_ID).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, ?LOGO_REQ) ->
    validate_attachment(Context, ?LOGO_REQ, cb_context:req_verb(Context));
validate(Context, ?ICON_REQ) ->
    validate_attachment(Context, ?ICON_REQ, cb_context:req_verb(Context));
validate(Context, ?WELCOME_REQ) ->
    validate_attachment(Context, ?WELCOME_REQ, cb_context:req_verb(Context));
validate(Context, Domain) ->
    validate_domain(Context, Domain, cb_context:req_verb(Context)).

-spec validate_attachment(cb_context:context(), path_token(), http_method()) ->
                                 cb_context:context().
validate_attachment(Context, ?LOGO_REQ, ?HTTP_GET) ->
    load_whitelabel_binary(Context, ?LOGO_REQ);
validate_attachment(Context, ?ICON_REQ, ?HTTP_GET) ->
    load_whitelabel_binary(Context, ?ICON_REQ);
validate_attachment(Context, ?WELCOME_REQ, ?HTTP_GET) ->
    load_whitelabel_binary(Context, ?WELCOME_REQ);
validate_attachment(Context, AttachType, ?HTTP_POST) ->
    validate_attachment_post(Context, AttachType, cb_context:req_files(Context)).

-spec validate_attachment_post(cb_context:context(), path_token(), _) ->
                                      cb_context:context().
validate_attachment_post(Context, ?LOGO_REQ, []) ->
    cb_context:add_validation_error(
        <<"file">>
        ,<<"required">>
        ,wh_json:from_list([
            {<<"message">>, <<"Please provide an image file">>}
         ])
        ,Context
    );
validate_attachment_post(Context, ?ICON_REQ, []) ->
    cb_context:add_validation_error(
        <<"file">>
        ,<<"required">>
        ,wh_json:from_list([
            {<<"message">>, <<"Please provide an image file">>}
         ])
        ,Context
    );
validate_attachment_post(Context, ?WELCOME_REQ, []) ->
    cb_context:add_validation_error(
        <<"file">>
        ,<<"required">>
        ,wh_json:from_list([
            {<<"message">>, <<"Please provide an html file">>}
         ])
        ,Context
    );
validate_attachment_post(Context, ?LOGO_REQ, [{_Filename, FileJObj}]) ->
    validate_upload(Context, FileJObj);
validate_attachment_post(Context, ?ICON_REQ, [{_Filename, FileJObj}]) ->
    validate_upload(Context, FileJObj);
validate_attachment_post(Context, ?WELCOME_REQ, [{_Filename, FileJObj}]) ->
    validate_upload(Context, FileJObj);
validate_attachment_post(Context, ?LOGO_REQ, _Files) ->
    cb_context:add_validation_error(
        <<"file">>
        ,<<"maxItems">>
        ,wh_json:from_list([
            {<<"message">>, <<"Please provide a single image file">>}
         ])
        ,Context
    );
validate_attachment_post(Context, ?ICON_REQ, _Files) ->
    cb_context:add_validation_error(
        <<"file">>
        ,<<"maxItems">>
        ,wh_json:from_list([
            {<<"message">>, <<"Please provide a single image file">>}
         ])
        ,Context
    );
validate_attachment_post(Context, ?WELCOME_REQ, _Files) ->
    cb_context:add_validation_error(
        <<"file">>
        ,<<"maxItems">>
        ,wh_json:from_list([
            {<<"message">>, <<"please provide a single html file">>}
         ])
        ,Context
    ).

-spec validate_upload(cb_context:context(), wh_json:object()) ->
                             cb_context:context().
validate_upload(Context, FileJObj) ->
    Context1 = load_whitelabel_meta(Context, ?WHITELABEL_ID),
    case cb_context:resp_status(Context) of
        'success' ->
            CT = wh_json:get_value([<<"headers">>, <<"content_type">>], FileJObj, <<"application/octet-stream">>),
            Size = wh_json:get_integer_value([<<"headers">>, <<"content_length">>]
                                             ,FileJObj
                                             ,byte_size(wh_json:get_value(<<"contents">>, FileJObj, <<>>))
                                            ),
            Props = [{<<"content_type">>, CT}
                     ,{<<"content_length">>, Size}
                    ],
            validate_request(cb_context:set_req_data(Context1
                                                      ,wh_json:set_values(Props, cb_context:doc(Context))
                                                     )
                            ,?WHITELABEL_ID
                            );
        _Status -> Context1
    end.

-spec validate_domain(cb_context:context(), path_token(), http_method()) ->
                             cb_context:context().
validate_domain(Context, Domain, ?HTTP_GET) ->
    case cb_context:account_id(Context) of
        'undefined' -> find_whitelabel_meta(Context, Domain);
        _AccountId -> load_whitelabel_meta(Context, ?WHITELABEL_ID)
    end.

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, Domain, AttachType) ->
    validate_domain_attachment(Context, Domain, AttachType, cb_context:req_verb(Context)).

validate_domain_attachment(Context, Domain, AttachType, ?HTTP_GET) ->
    find_whitelabel_binary(Context, Domain, AttachType).

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    maybe_update_account_definition(crossbar_doc:save(Context)).

-spec post(cb_context:context()) -> cb_context:context().
post(Context) ->
    maybe_update_account_definition(crossbar_doc:save(Context)).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, ?LOGO_REQ) ->
    update_whitelabel_binary(?LOGO_REQ, ?WHITELABEL_ID, Context);
post(Context, ?ICON_REQ) ->
    update_whitelabel_binary(?ICON_REQ, ?WHITELABEL_ID, Context);
post(Context, ?WELCOME_REQ) ->
    update_whitelabel_binary(?WELCOME_REQ, ?WHITELABEL_ID, Context).

-spec delete(cb_context:context()) -> cb_context:context().
delete(Context) ->
    maybe_cleanup_account_definition(crossbar_doc:delete(Context, 'permanent')).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load the binary attachment of a whitelabel doc (based on a domain)
%% @end
%%--------------------------------------------------------------------
-spec find_whitelabel(cb_context:context(), ne_binary()) ->
                             cb_context:context().
find_whitelabel(Context, Domain) ->
    ViewOptions = [{'key', wh_util:to_lower_binary(Domain)}],
    Context1 = crossbar_doc:load_view(?AGG_VIEW_WHITELABEL_DOMAIN, ViewOptions, cb_context:set_account_db(Context, ?WH_ACCOUNTS_DB)),
    case cb_context:resp_status(Context1) of
        'success' ->
            case cb_context:doc(Context1) of
                [JObj] ->
                    Db = wh_json:get_ne_value([<<"value">>, <<"account_db">>], JObj),
                    Id = wh_json:get_ne_value([<<"value">>, <<"account_id">>], JObj),
                    cb_context:setters(Context1
                                       ,[{fun cb_context:set_account_db/2, Db}
                                         ,{fun cb_context:set_account_id/2, Id}
                                       ]);
                _Doc ->
                    cb_context:add_system_error(
                        'bad_identifier'
                        ,wh_json:from_list([{<<"cause">>, Domain}])
                        ,Context1
                    )
            end;
        _Status -> Context1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a whitelabel document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_whitelabel_meta(cb_context:context(), ne_binary()) -> cb_context:context().
load_whitelabel_meta(Context, WhitelabelId) ->
    crossbar_doc:load(WhitelabelId, Context).

-spec find_whitelabel_meta(cb_context:context(), ne_binary()) -> cb_context:context().
find_whitelabel_meta(Context, Domain) ->
    Context1 = find_whitelabel(Context, Domain),
    case cb_context:resp_status(Context1) of
        'success' -> load_whitelabel_meta(Context1, ?WHITELABEL_ID);
        _Status -> Context1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a whitelabel binary attachment from the database
%% @end
%%--------------------------------------------------------------------
-spec load_whitelabel_binary(cb_context:context(), ne_binary()) -> ne_binary().
load_whitelabel_binary(Context, AttachType) ->
    case whitelabel_binary_meta(Context, AttachType) of
        'undefined' -> crossbar_util:response_bad_identifier(AttachType, Context);
        {AttachmentId, JObj} ->
            update_response_with_attachment(Context, AttachmentId, JObj)
    end.

-spec find_whitelabel_binary(cb_context:context(), ne_binary(), ne_binary()) -> ne_binary().
find_whitelabel_binary(Context, Domain, AttachType) ->
    Context1 = find_whitelabel_meta(Context, Domain),
    case cb_context:resp_status(Context1) of
        'success' -> load_whitelabel_binary(Context1, AttachType);
        _Status -> Context1
    end.

-spec find_whitelabel_binary_meta(cb_context:context(), ne_binary(), ne_binary()) ->
                                         'undefined' | {ne_binary(), wh_json:object()}.
find_whitelabel_binary_meta(Context, Domain, AttachType) ->
    Context1 = find_whitelabel_meta(Context, Domain),
    case cb_context:resp_status(Context1) of
        'success' -> whitelabel_binary_meta(Context1, AttachType);
        _Status -> 'undefined'
    end.

-spec whitelabel_binary_meta(cb_context:context(), ne_binary()) ->
                                    'undefined' | {ne_binary(), wh_json:object()}.
whitelabel_binary_meta(Context, AttachType) ->
    JObj = wh_json:get_value(<<"_attachments">>
                            ,cb_context:doc(Context)
                            ,wh_json:new()),
    case whitelabel_attachment_id(JObj, AttachType) of
        'undefined' -> 'undefined';
        AttachmentId ->
            {AttachmentId, wh_json:get_value(AttachmentId, JObj)}
    end.

-spec whitelabel_attachment_id(wh_json:object(), ne_binary()) ->
                                      'undefined' | {ne_binary(), wh_json:object()}.
whitelabel_attachment_id(JObj, AttachType) ->
    filter_attachment_type(wh_json:get_keys(JObj), AttachType).

-spec filter_attachment_type(ne_binaries(), ne_binary()) -> api_binary().
filter_attachment_type([], _) -> 'undefined';
filter_attachment_type([AttachmentId|AttachmentIds], AttachType) ->
    case binary:match(AttachmentId, AttachType) of
        {0, _} -> AttachmentId;
        _Else -> filter_attachment_type(AttachmentIds, AttachType)
    end.

-spec update_response_with_attachment(cb_context:context(), ne_binary(), wh_json:object()) -> cb_context:context().
update_response_with_attachment(Context, AttachmentId, JObj) ->
    cb_context:set_resp_etag(
      cb_context:add_resp_headers(
        crossbar_doc:load_attachment(cb_context:doc(Context), AttachmentId, Context)
                                 ,[{<<"Content-Disposition">>, <<"attachment; filename=", AttachmentId/binary>>}
                                  ,{<<"Content-Type">>, wh_json:get_value([AttachmentId, <<"content_type">>], JObj)}
                                  ,{<<"Content-Length">>, wh_json:get_value([AttachmentId, <<"length">>], JObj)}
                                  ]
       ), 'undefined'
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_request(cb_context:context(), api_binary()) -> cb_context:context().
validate_request(Context, WhitelabelId) ->
    validate_unique_domain(Context, WhitelabelId).

-spec validate_unique_domain(cb_context:context(), ne_binary()) -> cb_context:context().
validate_unique_domain(Context, WhitelabelId) ->
    Domain = wh_json:get_ne_value(<<"domain">>, cb_context:req_data(Context)),
    case is_domain_unique(cb_context:account_id(Context), Domain) of
        'true' -> check_whitelabel_schema(Context, WhitelabelId);
        'false' ->
            Context1 =
                cb_context:add_validation_error(
                    <<"domain">>
                   ,<<"unique">>
                   ,wh_json:from_list([
                        {<<"message">>, <<"White label domain is already in use">>}
                        ,{<<"cause">>, Domain}
                     ])
                   ,Context
                ),
            check_whitelabel_schema(Context1, WhitelabelId)
    end.

-spec check_whitelabel_schema(cb_context:context(), ne_binary()) -> cb_context:context().
check_whitelabel_schema(Context, WhitelabelId) ->
    OnSuccess = fun(C) -> on_successful_validation(C, WhitelabelId) end,
    cb_context:validate_request_data(<<"whitelabel">>, Context, OnSuccess).

-spec on_successful_validation(cb_context:context(), api_binary()) -> cb_context:context().
on_successful_validation(Context, 'undefined') ->
    Doc = wh_json:set_values([{<<"pvt_type">>, <<"whitelabel">>}
                              ,{<<"_id">>, ?WHITELABEL_ID}
                             ], cb_context:doc(Context)),
    cb_context:set_doc(Context, Doc);
on_successful_validation(Context, WhitelabelId) ->
    crossbar_doc:load_merge(WhitelabelId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load the binary attachment of a whitelabel doc
%% @end
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update the binary attachment of a whitelabel doc
%% @end
%%--------------------------------------------------------------------
-spec update_whitelabel_binary(ne_binary(), path_token(), cb_context:context()) ->
                                      cb_context:context().
update_whitelabel_binary(AttachType, WhitelabelId, Context) ->
    JObj = cb_context:doc(Context),
    [{Filename, FileObj}] = cb_context:req_files(Context),
    Contents = wh_json:get_value(<<"contents">>, FileObj),
    CT = wh_json:get_value([<<"headers">>, <<"content_type">>], FileObj),
    Opts = [{'headers', [{'content_type', wh_util:to_list(CT)}]}],

    JObj1 = case whitelabel_binary_meta(Context, AttachType) of
                'undefined' -> JObj;
                {AttachmentId, _} -> wh_json:delete_key([<<"_attachments">>, AttachmentId], JObj)
            end,
    Context1 = crossbar_doc:save(cb_context:set_doc(Context, JObj1)),

    crossbar_doc:save_attachment(WhitelabelId, attachment_name(AttachType, Filename, CT), Contents, Context1, Opts).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generate an attachment name if one is not provided and ensure
%% it has an extension (for the associated content type)
%% @end
%%--------------------------------------------------------------------
-spec attachment_name(ne_binary(), ne_binary()) -> ne_binary().
-spec attachment_name(ne_binary(), ne_binary(), ne_binary()) -> ne_binary().
attachment_name(Filename, CT) ->
    Generators = [fun(A) ->
                          case wh_util:is_empty(A) of
                              'true' -> wh_util:to_hex_binary(crypto:rand_bytes(16));
                              'false' -> A
                          end
                  end
                  ,fun(A) ->
                           case wh_util:is_empty(filename:extension(A)) of
                               'false' -> A;
                               'true' ->
                                   <<A/binary, ".", (cb_modules_util:content_type_to_extension(CT))/binary>>
                           end
                   end
                 ],
    lists:foldl(fun(F, A) -> F(A) end, Filename, Generators).

attachment_name(AttachType, Filename, CT) ->
    <<AttachType/binary, "-", (attachment_name(Filename, CT))/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_domain_unique(ne_binary(), ne_binary()) -> boolean().
is_domain_unique(AccountId, Domain) ->
    ViewOptions = [{<<"key">>, wh_util:to_lower_binary(Domain)}],
    case couch_mgr:get_results(?WH_ACCOUNTS_DB, ?AGG_VIEW_WHITELABEL_DOMAIN, ViewOptions) of
        {'ok', []} -> 'true';
        {'ok', [JObj]} ->
            wh_json:get_ne_value([<<"value">>, <<"account_id">>], JObj) =:= AccountId;
        {'ok', _} -> 'false';
        {'error', _R} ->
            lager:debug("unable to get whitelabel domain view: ~p", [_R]),
            'false'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_update_account_definition(cb_context:context()) -> cb_context:context().
maybe_update_account_definition(Context) ->
    maybe_update_account_definition(Context, cb_context:resp_status(Context)).
maybe_update_account_definition(Context, 'success') ->
    Context1 = crossbar_doc:load(cb_context:account_id(Context), Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            AccountDoc = cb_context:doc(Context1),
            Domain = wh_json:get_ne_value(<<"domain">>, cb_context:doc(Context)),
            AccountDoc1 = wh_json:set_value(<<"pvt_whitelabel_domain">>, Domain, AccountDoc),
            _ = cb_accounts:post(cb_context:set_doc(Context1, AccountDoc1), cb_context:account_id(Context)),
            Context;
        _Status -> Context1
    end;
maybe_update_account_definition(Context, _Status) -> Context.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_cleanup_account_definition(cb_context:context()) -> cb_context:context().
maybe_cleanup_account_definition(Context) ->
    maybe_cleanup_account_definition(Context, cb_context:resp_status(Context)).

maybe_cleanup_account_definition(Context, 'success') ->
    Context1 = crossbar_doc:load(cb_context:account_id(Context), Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            AccountDoc = cb_context:doc(Context1),
            AccountDoc1 = wh_json:delete_key(<<"pvt_whitelabel_domain">>, AccountDoc),
            _ = cb_accounts:post(cb_context:set_doc(Context1, AccountDoc1), cb_context:account_id(Context)),
            Context;
        _Status -> Context1
    end;
maybe_cleanup_account_definition(Context, _Status) -> Context.
