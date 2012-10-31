%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%%
%%% Handle client requests for phone_number documents
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_phone_numbers).

-export([init/0
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2, allowed_methods/3
         ,resource_exists/0, resource_exists/1, resource_exists/2, resource_exists/3
         ,content_types_accepted/4
         ,validate/1, validate/2, validate/3, validate/4
         ,authorize/1
         ,authenticate/1
         ,put/2, put/3, put/4
         ,post/2, post/4
         ,delete/2, delete/4
         ,populate_phone_numbers/1
        ]).

-include("include/crossbar.hrl").
-include_lib("whistle_number_manager/include/wh_number_manager.hrl").
-include_lib("whistle/src/wh_json.hrl").

-define(PORT_DOCS, <<"docs">>).
-define(PORT, <<"port">>).
-define(ACTIVATE, <<"activate">>).
-define(RESERVE, <<"reserve">>).

-define(FIND_NUMBER_SCHEMA, "{\"$schema\": \"http://json-schema.org/draft-03/schema#\", \"id\": \"http://json-schema.org/draft-03/schema#\", \"properties\": {\"prefix\": {\"required\": \"true\", \"type\": \"string\", \"minLength\": 3, \"maxLength\": 8}, \"quantity\": {\"default\": 1, \"type\": \"integer\", \"minimum\": 1}}}").

-define(MIME_TYPES, [{<<"application">>, <<"pdf">>}
                     ,{<<"application">>, <<"x-gzip">>}
                     ,{<<"application">>, <<"zip">>}
                     ,{<<"application">>, <<"x-rar-compressed">>}
                     ,{<<"application">>, <<"x-tar">>}
                     ,{<<"image">>, <<"*">>}
                     ,{<<"text">>, <<"plain">>}
                     ,{<<"application">>, <<"base64">>}
                     ,{<<"application">>, <<"x-base64">>}
                    ]).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"account.created">>, ?MODULE, populate_phone_numbers),
    _ = crossbar_bindings:bind(<<"v1_resource.content_types_accepted.phone_numbers">>, ?MODULE, content_types_accepted),
    _ = crossbar_bindings:bind(<<"v1_resource.authenticate">>, ?MODULE, authenticate),
    _ = crossbar_bindings:bind(<<"v1_resource.authorize">>, ?MODULE, authorize),
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.phone_numbers">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.phone_numbers">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.phone_numbers">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.phone_numbers">>, ?MODULE, put),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.phone_numbers">>, ?MODULE, post),
    crossbar_bindings:bind(<<"v1_resource.execute.delete.phone_numbers">>, ?MODULE, delete).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec populate_phone_numbers/1 :: (#cb_context{}) -> 'ok'.
populate_phone_numbers(#cb_context{db_name=AccountDb, account_id=AccountId}) ->
    PVTs = [{<<"_id">>, ?WNM_PHONE_NUMBER_DOC}
            ,{<<"pvt_account_db">>, AccountDb}
            ,{<<"pvt_account_id">>, AccountId}
            ,{<<"pvt_vsn">>, <<"1">>}
            ,{<<"pvt_type">>, ?WNM_PHONE_NUMBER_DOC}
            ,{<<"pvt_modified">>, wh_util:current_tstamp()}
            ,{<<"pvt_created">>, wh_util:current_tstamp()}
           ],
    _ = couch_mgr:save_doc(AccountDb, wh_json:from_list(PVTs)),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods/0 :: () -> http_methods().
-spec allowed_methods/1 :: (path_token()) -> http_methods().
-spec allowed_methods/2 :: (path_token(), path_token()) -> http_methods().
-spec allowed_methods/3 :: (path_token(), path_token(), path_token()) -> http_methods().
allowed_methods() ->
    ['GET'].

allowed_methods(_) ->
    ['GET', 'PUT', 'POST', 'DELETE'].

allowed_methods(_, ?ACTIVATE) ->
    ['PUT'];
allowed_methods(_, ?RESERVE) ->
    ['PUT'];
allowed_methods(_, ?PORT) ->
    ['PUT'];
allowed_methods(_, ?PORT_DOCS) ->
    ['GET'].

allowed_methods(_, ?PORT_DOCS, _) ->
    ['PUT', 'POST', 'DELETE'].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists/0 :: () -> 'true'.
-spec resource_exists/1 :: (path_token()) -> 'true'.
-spec resource_exists/2 :: (path_token(), path_token()) -> boolean().
-spec resource_exists/3 :: (path_token(), path_token(), path_token()) -> boolean().
resource_exists() -> true.

resource_exists(_) -> true.

resource_exists(_, ?ACTIVATE) -> true;
resource_exists(_, ?RESERVE) -> true;
resource_exists(_, ?PORT) -> true;
resource_exists(_, ?PORT_DOCS) -> true;
resource_exists(_, _) -> false.

resource_exists(_, ?PORT_DOCS, _) -> true;
resource_exists(_, _, _) -> false.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec content_types_accepted/4 :: (#cb_context{}, path_token(), path_token(), path_token()) -> #cb_context{}.
content_types_accepted(#cb_context{req_verb = <<"put">>}=Context, _Number, ?PORT_DOCS, _Name) ->
    Context#cb_context{content_types_accepted=[{from_binary, ?MIME_TYPES}]};
content_types_accepted(#cb_context{req_verb = <<"post">>}=Context, _Number, ?PORT_DOCS, _Name) ->
    Context#cb_context{content_types_accepted=[{from_binary, ?MIME_TYPES}]}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authenticates the incoming request, returning true if the requestor is
%% known, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authenticate/1 :: (#cb_context{}) -> boolean().
authenticate(#cb_context{req_nouns=[{<<"phone_numbers">>,[]}], req_verb = <<"get">>}) ->
    true;
authenticate(#cb_context{}) -> false.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authorize/1 :: (#cb_context{}) -> boolean().
authorize(#cb_context{req_nouns=[{<<"phone_numbers">>,[]}], req_verb = <<"get">>}) ->
    true;
authorize(#cb_context{}) -> false.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate/1 :: (#cb_context{}) -> #cb_context{}.
-spec validate/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
-spec validate/3 :: (#cb_context{}, path_token(), path_token()) -> #cb_context{}.
-spec validate/4 :: (#cb_context{}, path_token(), path_token(), path_token()) -> #cb_context{}.

validate(#cb_context{req_verb = <<"get">>, account_id=undefined}=Context) ->
    find_numbers(Context);
validate(#cb_context{req_verb = <<"get">>}=Context) ->
    summary(Context).

validate(#cb_context{req_verb = <<"get">>}=Context, Number) ->
    read(Number, Context);
validate(#cb_context{req_verb = <<"put">>}=Context, _Number) ->
    validate_request(Context);
validate(#cb_context{req_verb = <<"post">>}=Context, _Number) ->
    validate_request(Context);
validate(#cb_context{req_verb = <<"delete">>}=Context, _Number) ->
    validate_delete(Context).

validate(#cb_context{req_verb = <<"put">>}=Context, _Number, ?ACTIVATE) ->
    validate_request(Context);
validate(#cb_context{req_verb = <<"put">>}=Context, _Number, ?RESERVE) ->
    validate_request(Context);
validate(#cb_context{req_verb = <<"put">>}=Context, _Number, ?PORT) ->
    validate_request(Context);
validate(#cb_context{req_verb = <<"get">>}=Context, Number, ?PORT_DOCS) ->
    list_attachments(Number, Context).

validate(#cb_context{req_verb = <<"delete">>}=Context, Number, ?PORT_DOCS, _) ->
    read(Number, Context);
validate(#cb_context{req_files=[]}=Context, _, ?PORT_DOCS, _) ->
    lager:debug("No files in request to save attachment"),
    Message = <<"please provide an port document">>,
    cb_context:add_validation_error(<<"file">>, <<"required">>, Message, Context);
validate(#cb_context{req_files=[{_, FileObj}]}=Context, Number, ?PORT_DOCS, Name) ->
    FileName = wh_util:to_binary(http_uri:encode(wh_util:to_list(Name))),
    read(Number, Context#cb_context{req_files=[{FileName, FileObj}]});
validate(Context, _, ?PORT_DOCS, _) ->
    lager:debug("Multiple files in request to save attachment"),
    Message = <<"please provide a single port document per request">>,
    cb_context:add_validation_error(<<"file">>, <<"maxItems">>, Message, Context).

-spec post/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
-spec post/4 :: (#cb_context{}, path_token(), path_token(), path_token()) -> #cb_context{}.
post(#cb_context{doc=JObj, auth_account_id=AuthBy}=Context, Number) ->
    Result = wh_number_manager:set_public_fields(Number, JObj, AuthBy),
    set_response(Result, Number, Context).

post(#cb_context{req_files=Files}=Context, Number, ?PORT_DOCS, _) ->
    put_attachments(Number, Context, Files).

-spec put/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
-spec put/3 :: (#cb_context{}, path_token(), path_token()) -> #cb_context{}.
-spec put/4 :: (#cb_context{}, path_token(), path_token(), path_token()) -> #cb_context{}.
put(#cb_context{account_id=AssignTo, auth_account_id=AuthBy, doc=JObj}=Context, Number) ->
    Result = wh_number_manager:create_number(Number, AssignTo, AuthBy, JObj),
    set_response(Result, Number, Context).

put(#cb_context{account_id=AssignTo, auth_account_id=AuthBy, doc=JObj}=Context, Number, ?PORT) ->
    Result = wh_number_manager:port_in(Number, AssignTo, AuthBy, JObj),
    set_response(Result, Number, Context);
put(#cb_context{account_id=AssignTo, auth_account_id=AuthBy, doc=JObj}=Context, Number, ?ACTIVATE) ->
    Result = wh_number_manager:assign_number_to_account(Number, AssignTo, AuthBy, JObj),
    set_response(Result, Number, Context);
put(#cb_context{account_id=AssignTo, auth_account_id=AuthBy, doc=JObj}=Context, Number, ?RESERVE) ->
    Result = wh_number_manager:reserve_number(Number, AssignTo, AuthBy, JObj),
    set_response(Result, Number, Context);
put(#cb_context{req_files=Files}=Context, Number, ?PORT_DOCS) ->
    put_attachments(Number, Context, Files).

put(#cb_context{req_files=Files}=Context, Number, ?PORT_DOCS, _) ->
    put_attachments(Number, Context, Files).

-spec delete/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
-spec delete/4 :: (#cb_context{}, path_token(), path_token(), path_token()) -> #cb_context{}.

delete(#cb_context{auth_account_id=AuthBy}=Context, Number) ->
    Result = wh_number_manager:release_number(Number, AuthBy),
    set_response(Result, Number, Context).

delete(#cb_context{auth_account_id=AuthBy}=Context, Number, ?PORT_DOCS, Name) ->
    FileName = wh_util:to_binary(http_uri:encode(wh_util:to_list(Name))),
    Result = wh_number_manager:delete_attachment(Number, FileName, AuthBy),
    set_response(Result, Number, Context).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_numbers/1 :: (#cb_context{}) -> #cb_context{}.
find_numbers(#cb_context{query_json=JObj}=Context) ->
    OnSuccess = fun(C) ->
                        Prefix = wh_json:get_ne_value(<<"prefix">>, JObj),
                        Quantity = wh_json:get_ne_value(<<"quantity">>, JObj, 1),
                        C#cb_context{resp_status=success
                                     ,resp_data=wh_number_manager:find(Prefix, Quantity)
                                    }
                end,
    Schema = wh_json:decode(?FIND_NUMBER_SCHEMA),
    cb_context:validate_request_data(Schema, Context#cb_context{req_data=JObj}, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary/1 :: (#cb_context{}) -> #cb_context{}.
summary(Context) ->
    case crossbar_doc:load(?WNM_PHONE_NUMBER_DOC, Context) of
        #cb_context{resp_error_code=404}=C ->
            crossbar_util:response(wh_json:new(), C);
        Else -> Else
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
read(Number, #cb_context{auth_account_id=AuthBy}=Context) ->
    Result = wh_number_manager:get_public_fields(Number, AuthBy),
    set_response(Result, Number, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec validate_request/1 :: (#cb_context{}) -> #cb_context{}.
validate_request(Context) ->
    check_phone_number_schema(Context).

check_phone_number_schema(Context) ->
    cb_context:validate_request_data(<<"phone_numbers">>, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec validate_delete/1 :: (#cb_context{}) -> #cb_context{}.
validate_delete(#cb_context{}=Context) ->
    Context#cb_context{resp_status=success
                       ,doc=undefined
                      }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec list_attachments/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
list_attachments(Number, #cb_context{auth_account_id=AuthBy}=Context) ->
    Result = wh_number_manager:list_attachments(Number, AuthBy),
    set_response(Result, Number, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec put_attachments/3 :: (ne_binary(), #cb_context{}, proplist()) -> #cb_context{}.
put_attachments(_, Context, []) ->
    Context#cb_context{resp_status=success};
put_attachments(Number, #cb_context{auth_account_id=AuthBy}=Context, [{Filename, FileObj}|Files]) ->
    HeadersJObj = wh_json:get_value(<<"headers">>, FileObj),
    Content = wh_json:get_value(<<"contents">>, FileObj),
    CT = wh_json:get_value(<<"content_type">>, HeadersJObj, <<"application/octet-stream">>),
    Options = [{headers, [{content_type, wh_util:to_list(CT)}]}],
    lager:debug("setting Content-Type to ~s", [CT]),
    case wh_number_manager:put_attachment(Number, Filename, Content, Options, AuthBy) of
        {ok, NewDoc} ->
            put_attachments(Number, Context#cb_context{resp_data=NewDoc}, Files);
        Result ->
            set_response(Result, Number, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_response/3 :: (operation_return(), ne_binary(), #cb_context{}) -> #cb_context{}.
set_response({ok, Doc}, _, Context) ->
    crossbar_util:response(Doc, Context);
set_response({Error, Reason}, _, Context) ->
    crossbar_util:response(error, wh_util:to_binary(Error), 500, Reason, Context);
set_response(_Else, _, Context) ->
    lager:debug("unexpected response: ~p", [_Else]),
    cb_context:add_system_error(unspecified_fault, Context).
