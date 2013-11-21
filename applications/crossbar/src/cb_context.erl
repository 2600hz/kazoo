%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz INC
%%% @doc
%%% Helpers for manipulating the #cb_context{} record
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_context).

-export([store/3
         ,fetch/2, fetch/3
         ,put_reqid/1
         ,import_errors/1
         ,response/1
         ,has_errors/1
         ,add_system_error/2, add_system_error/3
         ,add_validation_error/4
         ,validate_request_data/2, validate_request_data/3, validate_request_data/4
         ,add_content_types_provided/2
         ,add_content_types_accepted/2

         %% Getters / Setters
         ,account_id/1, set_account_id/2
         ,account_db/1, set_account_db/2
         ,auth_token/1, set_auth_token/2
         ,auth_doc/1, set_auth_doc/2
         ,auth_account_id/1, set_auth_account_id/2
         ,req_verb/1, set_req_verb/2
         ,req_data/1, set_req_data/2
         ,req_id/1, set_req_id/2
         ,req_files/1, set_req_files/2
         ,query_string/1, set_query_string/2
         ,client_ip/1
         ,doc/1, set_doc/2
         ,resp_data/1, set_resp_data/2
         ,resp_status/1, set_resp_status/2
         ,resp_headers/1, set_resp_header/3, add_resp_header/3
         ,api_version/1, set_api_version/2

         %% Special accessors
         ,req_value/2, req_value/3
        ]).

-include("crossbar.hrl").

-type context() :: #cb_context{}.
-export_type([context/0]).

req_value(Context, Key) -> req_value(Context, Key, 'undefined').
req_value(#cb_context{req_data=ReqData, query_json=QS}, Key, Default) ->
    wh_json:find(Key, [ReqData, QS], Default).

%% Accessors
account_id(#cb_context{account_id=AcctId}) -> AcctId.
account_db(#cb_context{db_name=AcctDb}) -> AcctDb.
auth_token(#cb_context{auth_token=AuthToken}) -> AuthToken.
auth_doc(#cb_context{auth_doc=AuthDoc}) -> AuthDoc.
auth_account_id(#cb_context{auth_account_id=AuthBy}) -> AuthBy.
req_verb(#cb_context{req_verb=ReqVerb}) -> ReqVerb.
req_data(#cb_context{req_data=ReqData}) -> ReqData.
req_files(#cb_context{req_files=ReqFiles}) -> ReqFiles.
query_string(#cb_context{query_json=Q}) -> Q.
client_ip(#cb_context{client_ip=IP}) -> IP.
req_id(#cb_context{req_id=ReqId}) -> ReqId.
doc(#cb_context{doc=Doc}) -> Doc.
resp_data(#cb_context{resp_data=RespData}) -> RespData.
resp_status(#cb_context{resp_status=RespStatus}) -> RespStatus.
resp_headers(#cb_context{resp_headers=RespHeaders}) -> RespHeaders.
api_version(#cb_context{api_version=ApiVersion}) -> ApiVersion.

%% Setters
set_account_id(#cb_context{}=Context, AcctId) -> Context#cb_context{account_id=AcctId}.
set_account_db(#cb_context{}=Context, AcctDb) -> Context#cb_context{db_name=AcctDb}.
set_auth_token(#cb_context{}=Context, AuthToken) -> Context#cb_context{auth_token=AuthToken}.
set_auth_doc(Context, AuthDoc) -> Context#cb_context{auth_doc=AuthDoc}.
set_auth_account_id(#cb_context{}=Context, AuthBy) -> Context#cb_context{auth_account_id=AuthBy}.
set_req_verb(#cb_context{}=Context, ReqVerb) -> Context#cb_context{req_verb=ReqVerb}.
set_req_data(#cb_context{}=Context, ReqData) -> Context#cb_context{req_data=ReqData}.
set_req_files(Context, ReqFiles) -> Context#cb_context{req_files=ReqFiles}.
set_query_string(#cb_context{}=Context, Q) -> Context#cb_context{query_json=Q}.
set_req_id(#cb_context{}=Context, ReqId) -> Context#cb_context{req_id=ReqId}.
set_doc(#cb_context{}=Context, Doc) -> Context#cb_context{doc=Doc}.
set_resp_data(#cb_context{}=Context, RespData) -> Context#cb_context{resp_data=RespData}.
set_resp_status(#cb_context{}=Context, RespStatus) -> Context#cb_context{resp_status=RespStatus}.
set_api_version(#cb_context{}=Context, ApiVersion) -> Context#cb_context{api_version=ApiVersion}.
set_resp_header(K, V, #cb_context{resp_headers=RespHeaders}=Context) ->
    Context#cb_context{resp_headers=lists:keystore(K, 1, RespHeaders, {K, V})}.
add_resp_header(K, V, #cb_context{resp_headers=RespHeaders}=Context) ->
    Context#cb_context{resp_headers=[{K, V} | RespHeaders]}.

%% Helpers

-spec add_content_types_provided(context(), crossbar_content_handler() | crossbar_content_handlers()) ->
                                              context().
add_content_types_provided(#cb_context{content_types_provided=CTPs}=Context, [_|_]=NewCTPs) ->
    Context#cb_context{content_types_provided = NewCTPs ++ CTPs};
add_content_types_provided(Context, {_, _}=NewCTP) ->
    add_content_types_provided(Context,[NewCTP]).

-spec add_content_types_accepted(context(), crossbar_content_handler() | crossbar_content_handlers()) ->
                                              context().
add_content_types_accepted(#cb_context{content_types_accepted=CTAs}=Context, [_|_]=NewCTAs) ->
    Context#cb_context{content_types_provided = NewCTAs ++ CTAs};
add_content_types_accepted(Context, {_, _}=NewCTA) ->
    add_content_types_provided(Context,[NewCTA]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Sets a value in the crossbar context for later retrieval during
%% this request.
%% @end
%%--------------------------------------------------------------------
-spec store(term(), term(), context()) -> context().
store(Key, Data, #cb_context{storage=Storage}=Context) ->
    Context#cb_context{storage=[{Key, Data}|props:delete(Key, Storage)]}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Fetches a previously stored value from the current request.
%% @end
%%--------------------------------------------------------------------
-spec fetch(term(), context()) -> term().
-spec fetch(term(), context(), term()) -> term().

fetch(Key, #cb_context{}=Context) ->
    fetch(Key, Context, 'undefined').

fetch(Key, #cb_context{storage=Storage}, Default) ->
    case props:get_value(Key, Storage) of
        'undefined' -> Default;
        Else -> Else
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function extracts the request ID and sets it as 'callid' in
%% the process dictionary, where the logger expects it.
%% @end
%%--------------------------------------------------------------------
-spec put_reqid(context()) -> api_binary().
put_reqid(#cb_context{req_id=ReqId}) ->
    put('callid', ReqId).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
has_errors(#cb_context{validation_errors=JObj, resp_status='success'}) ->
    (not wh_util:is_empty(JObj));
has_errors(_) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
import_errors(#cb_context{}=Context) ->
    case response(Context) of
        {'ok', _} -> Context;
        {'error', {ErrorCode, ErrorMsg, Errors}} ->
            Context#cb_context{resp_error_code=ErrorCode
                               ,resp_error_msg=ErrorMsg
                               ,resp_data=Errors
                               ,resp_status='error'
                              }
    end.

response(#cb_context{resp_status='success'
                     ,resp_data=JObj
                    }) ->
    {'ok', JObj};
response(#cb_context{resp_error_code=Code
                     ,resp_error_msg=Msg
                     ,resp_data=DataJObj
                     ,validation_errors=ValidationJObj
                    }) ->
    ErrorCode = try wh_util:to_integer(Code) of
                    C -> C
                catch
                    _:_ -> 500
                end,
    ErrorMsg = case wh_util:is_empty(Msg) of
                   'false' -> wh_util:to_binary(Msg);
                   'true' -> <<"generic_error">>
               end,
    ErrorData = case {wh_util:is_empty(ValidationJObj), wh_util:is_empty(DataJObj)} of
                    {'false', _} -> ValidationJObj;
                    {_, _} -> DataJObj
                end,
    {'error', {ErrorCode, ErrorMsg, ErrorData}}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Add a validation error to the list of request errors
%% @end
%%--------------------------------------------------------------------
validate_request_data(Schema, #cb_context{req_data=Data
                                          ,resp_status=RespStatus
                                         }=Context) ->
    case wh_json_validator:is_valid(wh_json:public_fields(Data), Schema) of
        {'fail', Errors} ->
            lager:debug("request data did not validate against ~s: ~p", [Schema, Errors]),
            lists:foldl(fun({Property, Error}, C) ->
                                [Code, Message] = binary:split(Error, <<":">>),
                                add_validation_error(Property, Code, Message, C)
                        end, Context#cb_context{resp_status='error'}, Errors);
        {'pass', JObj} ->
            Status = case RespStatus =:= 'error' of
                         'true' -> 'error';
                         'false' -> 'success'
                     end,
            %% Allow onboarding to set the document ID
            case wh_json:get_ne_value(<<"_id">>, Data) of
                'undefined' ->
                    Context#cb_context{resp_status=Status
                                       ,doc=JObj
                                      };
                Id ->
                    Context#cb_context{resp_status=Status
                                       ,doc=wh_json:set_value(<<"_id">>, Id, JObj)
                                      }
            end
    end.

validate_request_data(Schema, Context, OnSuccess) ->
    validate_request_data(Schema, Context, OnSuccess, 'undefined').

validate_request_data(Schema, Context, OnSuccess, OnFailure) ->
    case validate_request_data(Schema, Context) of
        #cb_context{resp_status='success'}=C1 when is_function(OnSuccess) ->
            OnSuccess(C1);
        #cb_context{}=C2 when is_function(OnFailure) ->
            OnFailure(C2);
        Else -> Else
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
add_system_error('too_many_requests', Context) ->
    crossbar_util:response('fatal', <<"too many requests">>, 429, Context);
add_system_error('unspecified_fault', Context) ->
    crossbar_util:response('fatal', <<"unspecified fault">>, Context);
add_system_error('account_cant_create_tree', Context) ->
    crossbar_util:response('fatal', <<"account creation fault">>, Context);
add_system_error('account_has_descendants', Context) ->
    crossbar_util:response('fatal', <<"account has descendants">>, Context);
add_system_error('faulty_request', Context) ->
    crossbar_util:response_faulty_request(Context);

add_system_error('bad_identifier', Context) ->
    crossbar_util:response_bad_identifier(<<"unknown">>, Context);

add_system_error('forbidden', Context) ->
    crossbar_util:response('error', <<"forbidden">>, 403, Context);
add_system_error('invalid_credentials', Context) ->
    crossbar_util:response('error', <<"invalid credentials">>, 401, Context);

add_system_error('datastore_missing', Context) ->
    crossbar_util:response_db_missing(Context);
add_system_error('datastore_missing_view', Context) ->
    crossbar_util:response_missing_view(Context);
add_system_error('datastore_conflict', Context) ->
    crossbar_util:response_conflicting_docs(Context);
add_system_error('datastore_unreachable', Context) ->
    crossbar_util:response_datastore_timeout(Context);
add_system_error('datastore_fault', Context) ->
    crossbar_util:response_db_fatal(Context);
add_system_error('empty_tree_accounts_exist', Context) ->
    crossbar_util:response('error', <<"unable to create account tree">>, 400, Context);

add_system_error('parse_error', Context) ->
    crossbar_util:response('error', <<"failed to parse request body">>, 400, Context);
add_system_error('invalid_method', Context) ->
    crossbar_util:response('error', <<"method not allowed">>, 405, Context);
add_system_error('not_found', Context) ->
    crossbar_util:response('error', <<"not found">>, 404, Context).

add_system_error('bad_identifier', Props, Context) ->
    Identifier = props:get_value('details', Props),
    crossbar_util:response_bad_identifier(Identifier, Context);
add_system_error('invalid_bulk_type', Props, Context) ->
    Type = props:get_value('type', Props),
    Reason = <<"bulk operations do not support documents of type ", (wh_util:to_binary(Type))/binary>>,
    crossbar_util:response('error', <<"invalid bulk type">>, 400, Reason, Context);
add_system_error('forbidden', Props, Context) ->
    Reason = props:get_value('details', Props),
    crossbar_util:response('error', <<"forbidden">>, 403, Reason, Context);
add_system_error('timeout', Props, Context) ->
    crossbar_util:response('error', <<"timeout">>, 500, props:get_value('details', Props), Context);
add_system_error(Error, _, Context) ->
    add_system_error(Error, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Add a validation error to the list of request errors
%% @end
%%--------------------------------------------------------------------
add_validation_error(Property, <<"type">>=C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);
add_validation_error(Property, <<"items">>=C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);
add_validation_error(Property, <<"required">>=C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);
add_validation_error(Property, <<"minimum">>=C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);
add_validation_error(Property, <<"maximum">>=C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);
add_validation_error(Property, <<"minItems">>=C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);
add_validation_error(Property, <<"maxItems">>=C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);
add_validation_error(Property, <<"uniqueItems">>=C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);
add_validation_error(Property, <<"pattern">>=C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);
add_validation_error(Property, <<"minLength">>=C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);
add_validation_error(Property, <<"maxLength">>=C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);
add_validation_error(Property, <<"enum">>=C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);
add_validation_error(Property, <<"format">>=C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);
add_validation_error(Property, <<"divisibleBy">>=C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);

%% Not unique within the datastore
add_validation_error(Property, <<"unique">> = C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);
%% User is not authorized to update the property
add_validation_error(Property, <<"forbidden">> = C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);
%% Date range is invalid, too small, or too large
add_validation_error(Property, <<"date_range">> = C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);
%% Value was required to locate a resource, but failed (like account_name)
add_validation_error(Property, <<"not_found">> = C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);

add_validation_error(Property, Code, Message, Context) ->
    lager:debug("UNKNOWN ERROR CODE: ~p", [Code]),
    _ = file:write_file("/tmp/kazoo_unknown_error_codes.log", io_lib:format("~p~n", [Code]), ['append']),
    add_depreciated_validation_error(Property, Code, Message, Context).

add_depreciated_validation_error(Property, Code, Message, Context) when is_binary(Property) ->
    add_depreciated_validation_error([Property], Code, Message, Context);
add_depreciated_validation_error(Property, Code, Message, #cb_context{validation_errors=JObj}=Context) ->
    %% Maintain the same error format we are currently using until we are ready to
    %% convert to something that makes sense....
    Key = wh_util:join_binary(Property, <<".">>),
    Context#cb_context{validation_errors=wh_json:set_value([Key, Code], Message, JObj)
                       ,resp_status='error'
                       ,resp_error_code=400
                       ,resp_data=wh_json:new()
                       ,resp_error_msg = <<"invalid data">>
                      }.
