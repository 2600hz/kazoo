%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Account IP auth module
%%%
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(cb_ip_auth).

-export([init/0
         ,allowed_methods/0
         ,resource_exists/0
         ,authorize/1
         ,authenticate/1
         ,validate/1
         ,put/1
        ]).

-include("../crossbar.hrl").

-define(AGG_VIEW_FILE, <<"views/accounts.json">>).
-define(AGG_VIEW_IP, <<"accounts/listing_by_ip">>).

%%%===================================================================
%%% IP
%%%===================================================================
init() ->
    couch_mgr:db_create(?TOKEN_DB),
    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, authenticate),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, authorize),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.ip_auth">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"*.resource_exists.ip_auth">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"*.validate.ip_auth">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"*.execute.put.ip_auth">>, ?MODULE, put).

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
allowed_methods() ->
    [?HTTP_PUT].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> true.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authenticate(#cb_context{}) -> 'false' | {'true', #cb_context{}}.
authenticate(#cb_context{req_nouns=[{<<"ip_auth">>, _}]}) ->
    lager:debug("request is for the ip_auth module", []),
    true;
authenticate(#cb_context{client_ip=IpKey}=Context) ->
    ViewOptions = [{<<"key">>, IpKey}],
    lager:debug("attemping to authenticate ip ~s", [IpKey]),
    case wh_json:is_empty(IpKey)
        orelse crossbar_doc:load_view(?AGG_VIEW_IP, ViewOptions, Context#cb_context{db_name=?WH_ACCOUNTS_DB}) 
    of
        true ->
            lager:debug("client ip address is empty", []),
            false;
        #cb_context{resp_status=success, doc=[JObj]}=Context1 -> 
            lager:debug("client ip address is allowed without an auth-token: ~p", [JObj]),
            {true, create_fake_token(Context1#cb_context{doc=JObj})};
        _ ->
            lager:debug("unable to find client ip in database", []),
            false
    end;
authenticate(_) ->
    false.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authorize(#cb_context{}) -> boolean().
authorize(#cb_context{req_nouns=[{<<"ip_auth">>, []}]}) ->
    true;
authorize(_) ->
    false.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec validate(#cb_context{}) -> #cb_context{}.
validate(#cb_context{req_verb = ?HTTP_PUT}=Context) ->
    cb_context:validate_request_data(<<"ip_auth">>, Context, fun on_successful_validation/1).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
put(Context) ->
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
-spec on_successful_validation(#cb_context{}) -> #cb_context{}.
on_successful_validation(#cb_context{client_ip=IpKey}=Context) ->
    ViewOptions = [{<<"key">>, IpKey}],
    case wh_json:is_empty(IpKey)
        orelse crossbar_doc:load_view(?AGG_VIEW_IP, ViewOptions, Context#cb_context{db_name=?WH_ACCOUNTS_DB}) 
    of
        true -> 
            cb_context:add_system_error(invalid_credentials, Context);
        #cb_context{resp_status=success, doc=[Doc]}=Context1 ->
            lager:debug("found IP key belongs to account ~p", [wh_json:get_value(<<"value">>, Doc)]),
            Context1#cb_context{resp_status=success, doc=Doc};
        Else -> Else
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to create a token and save it to the token db
%% @end
%%--------------------------------------------------------------------
-spec create_token(#cb_context{}) -> #cb_context{}.
create_token(#cb_context{doc=JObj}=Context) ->
    case wh_json:is_empty(JObj) of
        true ->
            lager:debug("refusing to create auth token for an empty doc"),
            cb_context:add_system_error(invalid_credentials, Context);
        false ->
            lager:debug("~p", [JObj]),
            AccountId = wh_json:get_value([<<"value">>, <<"account_id">>], JObj),
            Token = [{<<"account_id">>, AccountId}
                     ,{<<"created">>, calendar:datetime_to_gregorian_seconds(calendar:universal_time())}
                     ,{<<"modified">>, calendar:datetime_to_gregorian_seconds(calendar:universal_time())}
                     ,{<<"method">>, wh_util:to_binary(?MODULE)}
                    ],
            case couch_mgr:save_doc(?TOKEN_DB, wh_json:from_list(Token)) of
                {ok, Doc} ->
                    AuthToken = wh_json:get_value(<<"_id">>, Doc),
                    lager:debug("created new local auth token ~s", [AuthToken]),
                    crossbar_util:response(crossbar_util:response_auth(JObj)
                                           ,Context#cb_context{auth_token=AuthToken, auth_doc=Doc});
                {error, R} ->
                    lager:debug("could not create new local auth token, ~p", [R]),
                    cb_context:add_system_error(datastore_fault, Context)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to create a token
%% @end
%%--------------------------------------------------------------------
-spec create_fake_token(#cb_context{}) -> #cb_context{}.
create_fake_token(#cb_context{doc=JObj}=Context) ->
    case wh_json:is_empty(JObj) of
        true ->
            lager:debug("refusing to create auth token for an empty doc"),
            cb_context:add_system_error(invalid_credentials, Context);
        false ->
            AccountId = wh_json:get_value([<<"value">>, <<"account_id">>], JObj),
            AuthToken = wh_util:rand_hex_binary(12),
            Token = [{<<"account_id">>, AccountId}
                     ,{<<"created">>, calendar:datetime_to_gregorian_seconds(calendar:universal_time())}
                     ,{<<"modified">>, calendar:datetime_to_gregorian_seconds(calendar:universal_time())}
                     ,{<<"method">>, wh_util:to_binary(?MODULE)}
                     ,{<<"_id">>, AuthToken}
                    ],
            crossbar_util:response(crossbar_util:response_auth(JObj)
                                   ,Context#cb_context{auth_token=AuthToken
                                                       ,auth_doc=wh_json:from_list(Token)
                                                       ,auth_account_id=AccountId})

    end.

