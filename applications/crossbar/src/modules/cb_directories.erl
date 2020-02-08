%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Handle CRUD operations for Directories
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_directories).

-export([init/0
        ,allowed_methods/0, allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,content_types_provided/2
        ,to_pdf/1
        ,validate/1, validate/2
        ,put/1
        ,post/2
        ,patch/2
        ,delete/2
        ]).

-include("crossbar.hrl").

-define(PVT_FUNS, [fun add_pvt_type/2]).
-define(CB_LIST, <<"directories/crossbar_listing">>).
-define(CB_USERS_LIST, <<"directories/users_listing">>).

-type payload() :: {cowboy_req:req(), cb_context:context()}.
-export_type([payload/0]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.directories">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.directories">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.directories">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.to_pdf.get.directories">>, ?MODULE, 'to_pdf'),
    _ = crossbar_bindings:bind(<<"*.validate.directories">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.directories">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.directories">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.directories">>, ?MODULE, 'patch'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.directories">>, ?MODULE, 'delete'),
    'ok'.

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
allowed_methods(_DirectoryId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].

%%------------------------------------------------------------------------------
%% @doc This function determines if the provided list of Nouns are valid.
%% Failure here returns `404 Not Found'.
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc What content-types will the module be requiring (matched to the client's
%% Content-Type header.
%% Of the form `{atom, [{Type, SubType}]} :: {to_json, [{<<"application">>, <<"json">>}]}'
%% @end
%%------------------------------------------------------------------------------
-spec content_types_provided(cb_context:context(), path_token()) -> cb_context:context().
content_types_provided(Context, _Id) ->
    case cb_context:req_verb(Context) of
        ?HTTP_GET ->
            CTPs = ?CONTENT_PROVIDED ++ [{'to_pdf', ?PDF_CONTENT_TYPES}],
            cb_context:add_content_types_provided(Context, CTPs);
        _Verb ->
            Context
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec to_pdf(payload()) -> payload().
to_pdf({Req, Context}) ->
    Nouns = cb_context:req_nouns(Context),
    case props:get_value(<<"directories">>, Nouns, []) of
        [] -> {Req, Context};
        [Id] ->
            Context1 = read(Id, Context),
            case cb_context:resp_status(Context1) of
                'success' -> {Req, get_pdf(Context1)};
                _Status -> {Req, Context1}
            end
    end.

%%------------------------------------------------------------------------------
%% @doc This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400.
%% @end
%%------------------------------------------------------------------------------

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_directories(Context, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, Id) ->
    validate_directory(Context, Id, cb_context:req_verb(Context)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _) ->
    crossbar_doc:save(Context).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(Context).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, _) ->
    crossbar_doc:save(Context).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _) ->
    crossbar_doc:delete(Context).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec validate_directories(cb_context:context(), path_token()) -> cb_context:context().
validate_directories(Context, ?HTTP_GET) ->
    summary(Context);
validate_directories(Context, ?HTTP_PUT) ->
    create(Context).

%%------------------------------------------------------------------------------
%% @doc Create a new instance with the data provided, if it is valid
%% @end
%%------------------------------------------------------------------------------
-spec validate_directory(cb_context:context(), kz_term:ne_binary(), path_token()) -> cb_context:context().
validate_directory(Context, Id, ?HTTP_GET) ->
    read(Id, Context);
validate_directory(Context, Id, ?HTTP_POST) ->
    update(Id, Context);
validate_directory(Context, Id, ?HTTP_PATCH) ->
    validate_patch(Id, Context);
validate_directory(Context, Id, ?HTTP_DELETE) ->
    read_directory(Id, Context).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_pdf(cb_context:context()) -> cb_context:context().
get_pdf(Context) ->
    AccountId = cb_context:account_id(Context),
    Data = pdf_props(Context),
    case kz_pdf:generate(AccountId, Data) of
        {'error', _} ->
            cb_context:set_resp_data(Context, <<>>);
        {'ok', PDF} ->
            DocType = props:get_first_defined([<<"type">>, <<"pvt_type">>], Data),
            FileName = <<(cb_context:account_name(Context))/binary, "-", DocType/binary>>,
            CD = <<"attachment; filename=\"", FileName/binary, ".pdf\"">>,
            Context1 = cb_context:set_resp_header(Context, <<"content-disposition">>, CD),
            cb_context:set_resp_data(Context1, PDF)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec pdf_props(cb_context:context()) -> kz_term:proplist().
pdf_props(Context) ->
    RespData = cb_context:resp_data(Context),
    AccountId = cb_context:account_id(Context),

    Directory = kz_json:to_proplist(kz_json:delete_key(<<"users">>, RespData)),
    Users =
        pdf_users(AccountId
                 ,props:get_binary_value(<<"sort_by">>, Directory, <<"last_name">>)
                 ,kz_json:get_value(<<"users">>, RespData, [])
                 ),

    [{<<"type">>, <<"directory">>}
    ,{<<"users">>, Users}
    ,{<<"directory">>, Directory}
    ].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec pdf_users(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:objects()) -> any().
pdf_users(AccountId, SortBy, Users) ->
    AccountDb = kzs_util:format_account_db(AccountId),
    pdf_users(AccountDb, SortBy, Users, []).

-spec pdf_users(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:objects(), any()) -> any().
pdf_users(_AccountDb, SortBy, [], Acc) ->
    Users = [{props:get_value([<<"user">>, SortBy], U), U} || U <- Acc],
    [U || {_SortCriterion, U} <- lists:keysort(1, Users)];
pdf_users(AccountDb, SortBy, [JObj|Users], Acc) ->
    UserId = kz_json:get_value(<<"user_id">>, JObj),
    CallflowId = kz_json:get_value(<<"callflow_id">>, JObj),
    Props = [{<<"user">>, pdf_user(AccountDb, UserId)}
            ,{<<"callflow">>, pdf_callflow(AccountDb, CallflowId)}
            ],
    pdf_users(AccountDb, SortBy, Users, [Props|Acc]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec pdf_user(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:proplist().
pdf_user(AccountDb, UserId) ->
    case kz_datamgr:open_cache_doc(AccountDb, UserId) of
        {'error', _R} ->
            lager:error("failed to fetch user ~s in ~s: ~p", [UserId, AccountDb, _R]),
            [];
        {'ok', Doc} ->
            kz_json:recursive_to_proplist(kz_doc:public_fields(Doc))
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec pdf_callflow(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:proplist().
pdf_callflow(AccountDb, CallflowId) ->
    case kz_datamgr:open_cache_doc(AccountDb, CallflowId) of
        {'error', _R} ->
            lager:error("failed to fetch callflow ~s in ~s: ~p", [CallflowId, AccountDb, _R]),
            [];
        {'ok', Doc} ->
            kz_json:recursive_to_proplist(kz_doc:public_fields(Doc))
    end.

%%------------------------------------------------------------------------------
%% @doc Create a new instance with the data provided, if it is valid
%% @end
%%------------------------------------------------------------------------------
-spec create(cb_context:context()) -> cb_context:context().
create(Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"directories">>, Context, OnSuccess).

%%------------------------------------------------------------------------------
%% @doc Load an instance from the database
%% @end
%%------------------------------------------------------------------------------
-spec read(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    Context1 = crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(<<"directory">>)),
    case cb_context:resp_status(Context1) of
        'success' ->
            load_directory_users(Id, Context1);
        _Status -> Context1
    end.

-spec read_directory(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
read_directory(Id, Context) ->
    crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(<<"directory">>)).

-spec load_directory_users(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
load_directory_users(Id, Context) ->
    Context1 = crossbar_doc:load_view(?CB_USERS_LIST
                                     ,[{'startkey', [Id]}
                                      ,{'endkey', [Id, kz_json:new()]}
                                      ]
                                     ,Context
                                     ,fun normalize_users_results/2
                                     ),
    case cb_context:resp_status(Context1) of
        'success' ->
            Users = cb_context:resp_data(Context1),
            Directory = cb_context:resp_data(Context),
            cb_context:set_resp_data(Context, kz_json:set_value(<<"users">>, Users, Directory));
        _Status -> Context
    end.

%%------------------------------------------------------------------------------
%% @doc Update an existing conference document with the data provided, if it is
%% valid
%% @end
%%------------------------------------------------------------------------------
-spec update(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
update(DocId, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(DocId, C) end,
    cb_context:validate_request_data(<<"directories">>, Context, OnSuccess).

%%------------------------------------------------------------------------------
%% @doc Update-merge an existing conference document partially with the data provided, if it is
%% valid
%% @end
%%------------------------------------------------------------------------------
-spec validate_patch(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
validate_patch(DocId, Context) ->
    crossbar_doc:patch_and_validate(DocId, Context, fun update/2).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec on_successful_validation(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    cb_context:set_doc(Context
                      ,kz_doc:set_type(cb_context:doc(Context), <<"directory">>)
                      );
on_successful_validation(DocId, Context) ->
    crossbar_doc:load_merge(DocId, Context, ?TYPE_CHECK_OPTION(<<"directory">>)).

%%------------------------------------------------------------------------------
%% @doc Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%------------------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%------------------------------------------------------------------------------
%% @doc Normalizes the results of a view.
%% @end
%%------------------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj)|Acc].

-spec normalize_users_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_users_results(JObj, Acc) ->
    [kz_json:from_list([{<<"user_id">>, kz_doc:id(JObj)}
                       ,{<<"callflow_id">>, kz_json:get_ne_binary_value(<<"value">>, JObj)}
                       ])
     | Acc
    ].
