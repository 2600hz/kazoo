%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Account API auth module
%%%
%%% This is a non-standard module:
%%% * it authenticates and authorizes itself
%%% * it has a completely unique role
%%% * it operates without an account id (or account db)
%%% * it authorizes an account level cred
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_api_auth).

-export([init/0
         ,allowed_methods/0
         ,resource_exists/0
         ,authorize/1
         ,authenticate/1
         ,validate/1
         ,put/1
        ]).

-include("include/crossbar.hrl").

-define(AGG_VIEW_FILE, <<"views/accounts.json">>).
-define(AGG_VIEW_API, <<"accounts/listing_by_api">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    couch_mgr:db_create(?TOKEN_DB),
    _ = crossbar_bindings:bind(<<"v1_resource.authenticate">>, ?MODULE, authenticate),
    _ = crossbar_bindings:bind(<<"v1_resource.authorize">>, ?MODULE, authorize),
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.api_auth">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.api_auth">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.api_auth">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.api_auth">>, ?MODULE, put).

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
    ['PUT'].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists/0 :: () -> 'true'.
resource_exists() -> true.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authorize/1 :: (#cb_context{}) -> boolean().
authorize(#cb_context{req_nouns=[{<<"api_auth">>, _}]}) ->
    true;
authorize(_) ->
    false.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authenticate/1 :: (#cb_context{}) -> boolean().
authenticate(#cb_context{req_nouns=[{<<"api_auth">>, []}]}) ->
    true;
authenticate(_) ->
    false.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec validate/1 :: (#cb_context{}) -> #cb_context{}.
validate(#cb_context{req_data=Data, req_verb = <<"put">>}=Context) ->
    case wh_json_validator:is_valid(Data, <<"api_auth">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            _ = cb_context:put_reqid(Context),
            authorize_api_key(Context, wh_json:get_value(<<"api_key">>, JObj))
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
put(Context) ->
    _ = cb_context:put_reqid(Context),
    create_token(Context).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the credentials are valid based on the
%% provided hash method
%%
%% Failure here returns 401
%% @end
%%--------------------------------------------------------------------
-spec authorize_api_key/2 :: (#cb_context{}, ne_binary()) -> #cb_context{}.
authorize_api_key(Context, ApiKey) when not is_binary(ApiKey) ->
    lager:debug("api key is not the correct format"),
    crossbar_util:response(error, <<"invalid crentials">>, 401, Context);
authorize_api_key(Context, <<>>) ->
    lager:debug("request has no api key"),
    crossbar_util:response(error, <<"invalid crentials">>, 401, Context);
authorize_api_key(Context, ApiKey) ->
    case crossbar_doc:load_view(?AGG_VIEW_API, [{<<"key">>, ApiKey}], Context#cb_context{db_name=?WH_ACCOUNTS_DB}) of
        #cb_context{resp_status=success, doc=[JObj|_]}->
            lager:debug("found more account with ~s, using ~s", [ApiKey, wh_json:get_value(<<"id">>, JObj)]),
            Context#cb_context{resp_status=success, doc=wh_json:get_value(<<"value">>, JObj)};
        #cb_context{resp_status=success, doc=JObj} ->
            case wh_json:is_empty(JObj) of
                true ->
                    lager:debug("found API key belongs to account ~s", [wh_json:get_value(<<"id">>, JObj)]),
                    Context#cb_context{resp_status=success, doc=wh_json:get_value(<<"value">>, JObj)};
                false ->
                    lager:debug("API key does not belong to any account"),
                    crossbar_util:response(error, <<"invalid crentials">>, 401, Context)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to create a token and save it to the token db
%% @end
%%--------------------------------------------------------------------
-spec create_token/1 :: (#cb_context{}) -> #cb_context{}.
create_token(#cb_context{doc=JObj}=Context) ->
    case wh_json:is_empty(JObj) of
        true ->
            lager:debug("refusing to create auth token for an empty doc"),
            crossbar_util:response(error, <<"invalid crentials">>, 401, Context);
        false ->
            AccountId = wh_json:get_value(<<"account_id">>, JObj),
            Token = [{<<"account_id">>, AccountId}
                     ,{<<"created">>, calendar:datetime_to_gregorian_seconds(calendar:universal_time())}
                     ,{<<"modified">>, calendar:datetime_to_gregorian_seconds(calendar:universal_time())}
                     ,{<<"method">>, wh_util:to_binary(?MODULE)}
                    ],
            case couch_mgr:save_doc(?TOKEN_DB, wh_json:from_list(Token)) of
                {ok, Doc} ->
                    AuthToken = wh_json:get_value(<<"_id">>, Doc),
                    lager:debug("created new local auth token ~s", [AuthToken]),
                    crossbar_util:response(wh_json:from_list([{<<"account_id">>, AccountId}])
                                           ,Context#cb_context{auth_token=AuthToken, auth_doc=Doc});
                {error, R} ->
                    lager:debug("could not create new local auth token, ~p", [R]),
                    crossbar_util:response(error, <<"invalid crentials">>, 401, Context)
            end
    end.
