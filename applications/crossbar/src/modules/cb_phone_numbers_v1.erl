-module(cb_phone_numbers_v1).

-export([validate/1]).

-include("../crossbar.hrl").
-include_lib("whistle_number_manager/include/wh_number_manager.hrl").

-define(FIND_NUMBER_SCHEMA, "{\"$schema\": \"http://json-schema.org/draft-03/schema#\", \"id\": \"http://json-schema.org/draft-03/schema#\", \"properties\": {\"prefix\": {\"required\": \"true\", \"type\": \"string\", \"minLength\": 3, \"maxLength\": 10}, \"quantity\": {\"default\": 1, \"type\": \"integer\", \"minimum\": 1}}}").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(#cb_context{req_verb = ?HTTP_GET
                     ,account_id='undefined'
                    }=Context) ->
    find_numbers(Context);
validate(#cb_context{req_verb = ?HTTP_GET}=Context) ->
    cb_phone_numbers:summary(Context).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_numbers(cb_context:context()) -> cb_context:context().
find_numbers(Context) ->
    AccountId = cb_context:auth_account_id(Context),
    QueryString = wh_json:set_value(<<"Account-ID">>, AccountId, cb_context:query_string(Context)),
    OnSuccess = fun(C) ->
                    cb_context:set_resp_data(
                        cb_context:set_resp_status(C, 'success')
                        ,get_numbers(QueryString)
                    )
                end,
    Schema = wh_json:decode(?FIND_NUMBER_SCHEMA),
    cb_context:validate_request_data(Schema
                                     ,cb_context:set_req_data(Context, QueryString)
                                     ,OnSuccess
                                    ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_numbers(wh_json:object()) -> ne_binaries().
get_numbers(QueryString) ->
    Prefix = wh_json:get_ne_value(<<"prefix">>, QueryString),
    Quantity = wh_json:get_ne_value(<<"quantity">>, QueryString, 1),
    lists:reverse(
        lists:foldl(
            fun(JObj, Acc) ->
                [wh_json:get_value(<<"number">>, JObj)|Acc]
            end
            ,[]
            ,wh_number_manager:find(Prefix, Quantity, wh_json:to_proplist(QueryString))
        )
    ).






