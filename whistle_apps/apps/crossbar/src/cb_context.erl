%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
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
        ]).

-include_lib("crossbar/include/crossbar.hrl").

-type context() :: #cb_context{}.
-export_type([context/0]).

-spec add_content_types_provided/2 :: (context(), crossbar_content_handler() | crossbar_content_handlers()) ->
                                              context().
add_content_types_provided(#cb_context{content_types_provided=CTPs}=Context, [_|_]=NewCTPs) ->
    Context#cb_context{content_types_provided = NewCTPs ++ CTPs};
add_content_types_provided(Context, {_, _}=NewCTP) ->
    add_content_types_provided(Context,[NewCTP]).

-spec add_content_types_accepted/2 :: (context(), crossbar_content_handler() | crossbar_content_handlers()) ->
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
-spec store/3 :: (term(), term(), context()) -> context().
store(Key, Data, #cb_context{storage=Storage}=Context) ->
    Context#cb_context{storage=[{Key, Data}|props:delete(Key, Storage)]}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Fetches a previously stored value from the current request.
%% @end
%%--------------------------------------------------------------------
-spec fetch/2 :: (term(), context()) -> term().
-spec fetch/3 :: (term(), context(), term()) -> term().

fetch(Key, #cb_context{}=Context) ->
    fetch(Key, Context, undefined).

fetch(Key, #cb_context{storage=Storage}, Default) ->
    case props:get_value(Key, Storage) of
        undefined -> Default;
        Else -> Else
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function extracts the request ID and sets it as 'callid' in
%% the process dictionary, where the logger expects it.
%% @end
%%--------------------------------------------------------------------
-spec put_reqid/1 :: (context()) -> api_binary().
put_reqid(#cb_context{req_id=ReqId}) ->
    put(callid, ReqId).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
has_errors(#cb_context{validation_errors=JObj, resp_status=success}) ->
    (not wh_util:is_empty(JObj));
has_errors(_) -> true.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
import_errors(#cb_context{}=Context) ->
    case response(Context) of
        {ok, _} -> Context;
        {error, {ErrorCode, ErrorMsg, Errors}} ->
            Context#cb_context{resp_error_code=ErrorCode
                               ,resp_error_msg=ErrorMsg
                               ,resp_data=Errors
                               ,resp_status=error}
    end.

response(#cb_context{resp_status=success, resp_data=JObj}) ->
    {ok, JObj};
response(#cb_context{resp_error_code=Code, resp_error_msg=Msg, resp_data=DataJObj
                     ,validation_errors=ValidationJObj}=Context) ->
    ErrorCode = try wh_util:to_integer(Context#cb_context.resp_error_code) of
                    Code -> Code
                catch
                    _:_ -> 500
                end,
    ErrorMsg = case wh_util:is_empty(Msg) of
                   false -> wh_util:to_binary(Msg);
                   true -> <<"generic_error">>
               end,
    ErrorData = case {wh_util:is_empty(ValidationJObj), wh_util:is_empty(DataJObj)} of
                    {false, _} -> ValidationJObj;
                    {_, _} -> DataJObj
                end,
    {error, {ErrorCode, ErrorMsg, ErrorData}}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Add a validation error to the list of request errors
%% @end
%%--------------------------------------------------------------------
validate_request_data(Schema, #cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(wh_json:public_fields(Data), Schema) of
        {fail, Errors} ->
            lager:debug("request data did not validate against ~s: ~p", [Schema, Errors]),
            io:format("request data did not validate against ~s: ~p", [Schema, Errors]),
            lists:foldl(fun({Property, Error}, C) ->
                                [Code, Message] = binary:split(Error, <<":">>),
                                add_validation_error(Property, Code, Message, C)
                        end, Context#cb_context{resp_status=error}, Errors);
        {pass, JObj} ->
            %% Allow onboarding to set the document ID
            case wh_json:get_ne_value(<<"_id">>, Data) of
                undefined -> Context#cb_context{resp_status=success, doc=JObj};
                Id -> Context#cb_context{resp_status=success
                                         ,doc=wh_json:set_value(<<"_id">>, Id, JObj)}
            end
    end.

validate_request_data(Schema, Context, OnSuccess) ->
    validate_request_data(Schema, Context, OnSuccess, undefined).

validate_request_data(Schema, Context, OnSuccess, OnFailure) ->
    case validate_request_data(Schema, Context) of
        #cb_context{resp_status=success}=C1 when is_function(OnSuccess) ->
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
add_system_error(unspecified_fault, Context) ->
    crossbar_util:response(fatal, <<"unspecified fault">>, Context);
add_system_error(account_cant_create_tree, Context) ->
    crossbar_util:response(fatal, <<"account creation fault">>, Context);
add_system_error(account_has_descendants, Context) ->
    crossbar_util:response(fatal, <<"account has descendants">>, Context);
add_system_error(faulty_request, Context) ->
    crossbar_util:response_faulty_request(Context);

add_system_error(bad_identifier, Context) ->    
    crossbar_util:response_bad_identifier(<<"unknown">>, Context);

add_system_error(forbidden, Context) ->
    crossbar_util:response(error, <<"forbidden">>, 403, Context);
add_system_error(invalid_crentials, Context) ->
    crossbar_util:response(error, <<"invalid crentials">>, 401, Context);

add_system_error(datastore_missing, Context) ->
    crossbar_util:response_db_missing(Context);
add_system_error(datastore_missing_view, Context) ->
    crossbar_util:response_missing_view(Context);
add_system_error(datastore_conflict, Context) ->
    crossbar_util:response_conflicting_docs(Context);
add_system_error(datastore_unreachable, Context) ->
    crossbar_util:response_datastore_timeout(Context);
add_system_error(datastore_fault, Context) ->
    crossbar_util:response_db_fatal(Context);
add_system_error(empty_tree_accounts_exist, Context) ->
    crossbar_util:response(error, <<"unable to create account tree">>, 400, Context);

add_system_error(parse_error, Context) ->
    crossbar_util:response(error, <<"failed to parse request body">>, 400, Context);
add_system_error(invalid_method, Context) ->
    crossbar_util:response(error, <<"method not allowed">>, 405, Context);
add_system_error(not_found, Context) ->
    crossbar_util:response(error, <<"not found">>, 404, Context).

add_system_error(bad_identifier, Props, Context) ->
    Identifier = props:get_value(details, Props),
    crossbar_util:response_bad_identifier(Identifier, Context);
add_system_error(forbidden, Props, Context) ->
    Reason = props:get_value(details, Props),
    crossbar_util:response(error, <<"forbidden">>, 403, Reason, Context);
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
add_validation_error(Property, <<"unique">>=C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);
%% User is not authorized to update the property  
add_validation_error(Property, <<"forbidden">>=C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);
%% Date range is invalid, too small, or too large
add_validation_error(Property, <<"date_range">>=C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);
%% Value was required to locate a resource, but failed (like account_name)
add_validation_error(Property, <<"not_found">>=C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);


add_validation_error(Property, Code, Message, Context) ->
    lager:debug("UNKNOWN ERROR CODE: ~p", [Code]),
    _ = file:write_file("/tmp/kazoo_unknown_error_codes.log", io_lib:format("~p~n", [Code]), [append]),
    add_depreciated_validation_error(Property, Code, Message, Context).    

add_depreciated_validation_error(Property, Code, Message, Context) when is_binary(Property) ->
    add_depreciated_validation_error([Property], Code, Message, Context);    
add_depreciated_validation_error(Property, Code, Message, #cb_context{validation_errors=JObj}=Context) ->
    %% Maintain the same error format we are currently using until we are ready to
    %% convert to something that makes sense....
    Key = wh_util:join_binary(Property, <<".">>),
    Context#cb_context{validation_errors=wh_json:set_value([Key, Code], Message, JObj)
                       ,resp_status=error
                       ,resp_error_code=400
                       ,resp_data=wh_json:new()
                       ,resp_error_msg = <<"invalid data">>
                      }.
