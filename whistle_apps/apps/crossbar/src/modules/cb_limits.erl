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
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,validate/1, validate/2
         ,put/1
         ,post/2
        ]).

-include_lib("crossbar/include/crossbar.hrl").

-define(CB_LIST, <<"limits/crossbar_listing">>).
-define(PVT_TYPE, <<"sip_service">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.limits">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.limits">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.limits">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.limits">>, ?MODULE, put),
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
-spec allowed_methods/1 :: (path_token()) -> http_methods().
allowed_methods() ->
    ['GET', 'PUT'].
allowed_methods(_) ->
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
-spec resource_exists/1 :: (path_token()) -> 'true'.
resource_exists() ->
    true.
resource_exists(_) ->
    true.

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
-spec validate/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
validate(#cb_context{req_verb = <<"get">>}=Context) ->
    try
        load_limit_summary(Context)
    catch
        _T:_R ->
            ST = erlang:get_stacktrace(),
            lager:debug("loading summary crashed: ~p: ~p", [_T, _R]),
            _ = [lager:debug("~p", [S]) || S <- ST],
            crossbar_util:response_db_fatal(Context)
    end;
validate(#cb_context{req_verb = <<"put">>}=Context) ->
    create_limits(Context).

validate(#cb_context{req_verb = <<"get">>}=Context, LimitId) ->
    try
        load_limit(LimitId, Context)
    catch
        _T:_R ->
            lager:debug("loading limit crashed: ~p: ~p", [_T, _R]),
            crossbar_util:response_db_fatal(Context)
    end;
validate(#cb_context{req_verb = <<"post">>}=Context, LimitId) ->
    update_limits(LimitId, Context).

-spec post/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
post(Context, _) ->
    crossbar_doc:save(Context).

-spec put/1 :: (#cb_context{}) -> #cb_context{}.
put(Context) ->
    crossbar_doc:save(Context).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of Limit, each summarized.
%% @end
%%--------------------------------------------------------------------
-spec load_limit_summary/1 :: (#cb_context{}) -> #cb_context{}.
load_limit_summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [{<<"include_docs">>, true}], Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a Limit document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_limit/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
load_limit(LimitId, Context) ->
    crossbar_doc:load(LimitId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new limits document with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create_limits/1 :: (#cb_context{}) -> #cb_context{}.
create_limits(Context) ->
    case load_limit_summary(Context) of
        #cb_context{doc=[]} ->
            lager:debug("No other limit doc exists, creating"),
            validate_create(Context);
        #cb_context{doc=[LimitDoc]} ->
            DocId = wh_json:get_value(<<"id">>, LimitDoc),

            lager:debug("limit doc ~s exists, redirecting", [DocId]),

            crossbar_util:response_redirect(Context, DocId, wh_json:from_list([{<<"Location">>, DocId}]))
    end.

-spec validate_create/1 :: (#cb_context{}) -> #cb_context{}.
validate_create(#cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(Data, <<"limits">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            Context#cb_context{resp_status=success
                               ,doc=wh_json:set_value(<<"pvt_type">>, ?PVT_TYPE, JObj)
                              }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing device document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update_limits/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
update_limits(DocId, #cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(Data, <<"limits">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            crossbar_doc:load_merge(DocId, JObj, Context)
    end.
