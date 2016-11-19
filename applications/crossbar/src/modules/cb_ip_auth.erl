%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
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

-include("crossbar.hrl").

-define(AGG_VIEW_FILE, <<"views/accounts.json">>).
-define(AGG_VIEW_IP, <<"accounts/listing_by_ip">>).

%%%===================================================================
%%% IP
%%%===================================================================

-spec init() -> ok.
init() ->
    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.ip_auth">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.ip_auth">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.ip_auth">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.ip_auth">>, ?MODULE, 'put'),
    ok.

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
resource_exists() -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authenticate(cb_context:context()) ->
                          'false' |
                          {'true', cb_context:context()}.
authenticate(Context) ->
    authenticate_nouns(Context, cb_context:req_nouns(Context)).

-spec authenticate_nouns(cb_context:context(), req_nouns()) ->
                                'false' | 'true' | {'true', cb_context:context()}.
authenticate_nouns(_Context, [{<<"ip_auth">>, _}]) ->
    lager:debug("request is for the ip_auth module", []),
    'true';
authenticate_nouns(Context, _Nouns) ->
    authenticate_ip(Context, cb_context:client_ip(Context)).

-spec authenticate_ip(cb_context:context(), ne_binary()) ->
                             'false' |
                             {'true', cb_context:context()}.
authenticate_ip(Context, IpKey) ->
    ViewOptions = [{'key', IpKey}],
    lager:debug("attemping to authenticate ip ~s", [IpKey]),
    case kz_json:is_empty(IpKey)
        orelse crossbar_doc:load_view(?AGG_VIEW_IP
                                     ,ViewOptions
                                     ,cb_context:set_account_db(Context, ?KZ_ACCOUNTS_DB)
                                     )
    of
        'true' ->
            lager:debug("client ip address is empty"),
            'false';
        Context1 ->
            authenticate_view(Context1, cb_context:resp_status(Context1))
    end.

-spec authenticate_view(cb_context:context(), atom()) ->
                               'false' |
                               {'true', cb_context:context()}.
authenticate_view(Context, 'success') ->
    case cb_context:doc(Context) of
        [] -> 'false';
        [JObj] ->
            lager:debug("client ip address is allowed without an auth-token: ~p", [JObj]),
            {'true', create_fake_token(cb_context:set_doc(Context, JObj))}
    end;
authenticate_view(_Context, _Status) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    authorize_nouns(cb_context:req_nouns(Context)).

-spec authorize_nouns(req_nouns()) -> boolean().
authorize_nouns([{<<"ip_auth">>, []}]) ->
    'true';
authorize_nouns(_Nouns) ->
    'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    cb_context:validate_request_data(<<"ip_auth">>, Context, fun on_successful_validation/1).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    crossbar_auth:create_auth_token(Context, ?MODULE).

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
-spec on_successful_validation(cb_context:context()) -> cb_context:context().
on_successful_validation(Context) ->
    IpKey = cb_context:client_ip(Context),
    ViewOptions = [{'key', IpKey}],
    case kz_json:is_empty(IpKey)
        orelse crossbar_doc:load_view(?AGG_VIEW_IP
                                     ,ViewOptions
                                     ,cb_context:set_account_db(Context, ?KZ_ACCOUNTS_DB)
                                     )
    of
        'true' ->
            cb_context:add_system_error('invalid_credentials', Context);
        Context1 ->
            on_successful_load(Context1, cb_context:resp_status(Context1), cb_context:doc(Context1))
    end.

-spec on_successful_load(cb_context:context(), crossbar_status(), kz_json:objects()) -> cb_context:context().
on_successful_load(Context, 'success', [Doc]) ->
    lager:debug("found IP key belongs to account ~p", [kz_json:get_value(<<"value">>, Doc)]),
    cb_context:set_doc(Context, Doc);
on_successful_load(Context, _Status, _Doc) ->
    Context.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to create a token
%% @end
%%--------------------------------------------------------------------
-spec create_fake_token(cb_context:context()) -> cb_context:context().
-spec create_fake_token(cb_context:context(), kz_json:object()) -> cb_context:context().
create_fake_token(Context) ->
    JObj = cb_context:doc(Context),
    case kz_json:is_empty(JObj) of
        'true' ->
            lager:debug("refusing to create auth token for an empty doc"),
            cb_context:add_system_error('invalid_credentials', Context);
        'false' ->
            create_fake_token(Context, JObj)
    end.

create_fake_token(Context, JObj) ->
    AccountId = kz_json:get_value([<<"value">>, <<"account_id">>], JObj),
    AuthToken = kz_util:rand_hex_binary(12),
    Token = [{<<"account_id">>, AccountId}
            ,{<<"method">>, kz_util:to_binary(?MODULE)}
            ,{<<"_id">>, AuthToken}
            ],
    crossbar_util:response(crossbar_util:response_auth(JObj)
                          ,cb_context:setters(Context
                                             ,[{fun cb_context:set_auth_token/2, AuthToken}
                                              ,{fun cb_context:set_auth_doc/2, kz_json:from_list(Token)}
                                              ,{fun cb_context:set_auth_account_id/2, AccountId}
                                              ])).
