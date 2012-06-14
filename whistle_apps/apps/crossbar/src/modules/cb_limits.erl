%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_limits).

-export([init/0
         ,allowed_methods/0
         ,resource_exists/0
         ,billing/1
         ,validate/1
         ,post/1
        ]).

-include_lib("crossbar/include/crossbar.hrl").
-include_lib("whistle/src/wh_json.hrl").

-define(CB_LIST, <<"limits/crossbar_listing">>).
-define(PVT_TYPE, <<"limits">>).
-define(PVT_FUNS, [fun add_pvt_type/2, fun add_static_id/2]).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.limits">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.limits">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.billing">>, ?MODULE, billing),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.limits">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.limits">>, ?MODULE, post).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods/0 :: () -> http_methods().
allowed_methods() ->
    ['GET', 'POST'].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists/0 :: () -> 'true'.
resource_exists() ->
    true.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Bill for limits
%% @end
%%--------------------------------------------------------------------
billing(#cb_context{req_nouns=[{<<"limits">>, _}|_], req_verb = <<"get">>}=Context) ->
    Context;
billing(#cb_context{req_nouns=[{<<"limits">>, _}|_], doc=JObj, account_id=AccountId}=Context) ->
    case wh_resellers:fetch(AccountId) of
        {error, no_service_plan} -> Context;
        {ok, Resellers} ->
            try 
                UpdatedResellers = wh_service_limits:update(JObj, Resellers),
                ok = wh_resellers:commit_changes(UpdatedResellers)
            catch
                throw:{Error, Reason} ->
                    crossbar_util:response(error, wh_util:to_binary(Error), 500, Reason, Context)
            end
    end;
billing(Context) -> Context.

            
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate/1 :: (#cb_context{}) -> #cb_context{}.
validate(#cb_context{req_verb = <<"get">>}=Context) ->
    try
        load_limit(Context)
    catch
        _T:_R ->
            ST = erlang:get_stacktrace(),
            lager:debug("loading summary crashed: ~p: ~p", [_T, _R]),
            _ = [lager:debug("~p", [S]) || S <- ST],
            crossbar_util:response_db_fatal(Context)
    end;
validate(#cb_context{req_verb = <<"put">>}=Context) ->
    update_limits(Context);
validate(#cb_context{req_verb = <<"post">>}=Context) ->
    update_limits(Context).

-spec post/1 :: (#cb_context{}) -> #cb_context{}.
post(Context) ->
    crossbar_doc:save(Context).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a Limit document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_limit/1 :: (#cb_context{}) -> #cb_context{}.
load_limit(Context) ->
    case crossbar_doc:load(?PVT_TYPE, Context) of
        #cb_context{resp_error_code=404} ->
            {pass, JObj} = wh_json_validator:is_valid(wh_json:new(), <<"limits">>),
            crossbar_util:response(JObj, Context);
        Else -> Else
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing device document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update_limits/1 :: (#cb_context{}) -> #cb_context{}.
update_limits(#cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(Data, <<"limits">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            {JObj1, _} = lists:foldr(fun(F, {J, C}) ->
                                             {F(J, C), C}
                                     end, {JObj, Context}, ?PVT_FUNS),
            case crossbar_doc:load_merge(?PVT_TYPE, JObj1, Context) of
                #cb_context{resp_error_code=404} ->
                    Context#cb_context{resp_status=success
                                       ,doc=crossbar_doc:update_pvt_parameters(JObj1, Context)
                                      };
                Else -> Else
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% These are the pvt funs that add the necessary pvt fields to every
%% instance
%% @end
%%--------------------------------------------------------------------
-spec add_pvt_type/2 :: (wh_json:json_object(), #cb_context{}) -> wh_json:json_object().
add_pvt_type(JObj, _) ->
    wh_json:set_value(<<"pvt_type">>, ?PVT_TYPE, JObj).

-spec add_static_id/2 :: (wh_json:json_object(), #cb_context{}) -> wh_json:json_object().
add_static_id(JObj, _) ->
    wh_json:set_value(<<"_id">>, ?PVT_TYPE, JObj).
