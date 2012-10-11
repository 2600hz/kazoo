%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Helpers for manipulating the #cb_context{} record
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_context).

-export([fetch/2]).
-export([store/3]).
-export([put_reqid/1]).
-export([import_errors/1]).
-export([has_errors/1]).
-export([add_validation_error/4]).
-export([validate_request_data/2]).

-include("include/crossbar.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Sets a value in the crossbar context for later retrieval during
%% this request.
%% @end
%%--------------------------------------------------------------------
-spec store/3 :: (term(), term(), #cb_context{}) -> #cb_context{}.
store(Key, Data, #cb_context{storage=Storage}=Context) ->
    Context#cb_context{storage=[{Key, Data}|props:delete(Key, Storage)]}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Fetches a previously stored value from the current request.
%% @end
%%--------------------------------------------------------------------
-spec fetch/2 :: (term(), #cb_context{}) -> term().
fetch(Key, #cb_context{storage=Storage}) ->
    props:get_value(Key, Storage).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function extracts the request ID and sets it as 'callid' in
%% the process dictionary, where the logger expects it.
%% @end
%%--------------------------------------------------------------------
-spec put_reqid/1 :: (#cb_context{}) -> api_binary().
put_reqid(#cb_context{req_id=ReqId}) ->
    put(callid, ReqId).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
has_errors(#cb_context{validation_errors=JObj}) ->
    (not wh_util:is_empty(JObj)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
import_errors(#cb_context{validation_errors=JObj}=Context) ->
    case has_errors(Context) of
        false -> Context;
        true -> crossbar_util:response_invalid_data(JObj, Context)
    end.
           
%%--------------------------------------------------------------------
%% @public
%% @doc
%% Add a validation error to the list of request errors
%% @end
%%--------------------------------------------------------------------
validate_request_data(Schema, #cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(Data, Schema) of
        {fail, Errors} ->
            lists:foldl(fun({Property, Error}, C) ->
                                [Code, Message] = binary:split(Error, <<":">>),
                                add_validation_error(Property, Code, Message, C)
                        end, Context#cb_context{resp_status=error}, Errors);
        {pass, JObj} ->
            Context#cb_context{resp_status=success, doc=JObj}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
add_system_error(account_cant_create_tree, Context) ->
    Context;
add_system_error(account_not_found, Context) ->
    Context;
add_system_error(account_has_descendants, Context) ->
    Context;
add_system_error(database_missing_view, Context) ->
    Context;
add_system_error(database_fault, Context) ->
    Context.


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
add_validation_error(Property, <<"unique">>=C, Message, Context) ->
    add_depreciated_validation_error(Property, C, Message, Context);
add_validation_error(Property, Code, Message, Context) ->
    lager:debug("UNKNOWN ERROR CODE: ~p", [Code]),
    file:write_file("/tmp/kazoo_unknown_error_codes.log", io_lib:format("~p~n", [Code]), [append]),
    add_depreciated_validation_error(Property, Code, Message, Context).    

add_depreciated_validation_error(Property, Code, Message, Context) when is_binary(Property) ->
    add_depreciated_validation_error([Property], Code, Message, Context);    
add_depreciated_validation_error(Property, Code, Message, #cb_context{validation_errors=JObj}=Context) ->
    %% Maintain the same error format we are currently using until we are ready to
    %% convert to something that makes sense....
    Key = wh_util:join_binary(Property, <<".">>),
    Context#cb_context{validation_errors=wh_json:set_value([Key, Code], Message, JObj)
                       ,resp_status=error}.
