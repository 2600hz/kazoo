%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP, INC
%%% @doc
%%% OpenID auth module
%%% Handle OpenID authentication of users, this module is non-standard:
%%%
%%% * it authenticates and authorizes itself
%%% * it operates without an account id (or account db)
%%% * it breaks the REST API (prefoming a GETs due to OpenID specs
%%%       and usability for the client)
%%% * it can (and will) redirect the user out of our domain
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_openid_auth).

-export([init/0
         ,allowed_methods/1, allowed_methods/2
         ,resource_exists/1, resource_exists/2
         ,validate/2, validate/3
         ,authenticate/1
         ,authorize/1
         ,get/2, get/3
        ]).

-include("include/crossbar.hrl").

-define(SIGNUP_DB, <<"signups">>).

-define(IdPs, [<<"google">>]).
-define(OPENID_CONFIG_CATEGORY, <<"crossbar.openid">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    ok = ssl:start(),
    {ok, _} = openid_srv:start_link(openid_auth_srv),

    %% prefetch some config values
    whapps_config:get(?OPENID_CONFIG_CATEGORY, <<"realm">>, <<"http://host.com:8000/v1">>),
    whapps_config:get(?OPENID_CONFIG_CATEGORY, <<"reg_url">>, <<"http://host.com:8000/register.php">>),
    whapps_config:get(?OPENID_CONFIG_CATEGORY, <<"app_url">>, <<"http://host.com:8000/winkstart.php">>),

    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.openid_auth">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.openid_auth">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.openid_auth">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"v1_resource.authorize">>, ?MODULE, authorize),
    _ = crossbar_bindings:bind(<<"v1_resource.authenticate">>, ?MODULE, authenticate),
    crossbar_bindings:bind(<<"v1_resource.execute.get.openid_auth">>, ?MODULE, get).

authorize(#cb_context{req_nouns=[{<<"openid_auth">>,[_]}]}) ->
    lager:debug("authorizing request"),
    true;
authorize(#cb_context{req_nouns=[{<<"openid_auth">>,[<<"checkauth">>, _]}]}) ->
    lager:debug("authorizing request"),
    true.

authenticate(#cb_context{req_nouns=[{<<"openid_auth">>,[_]}]}) ->
    lager:debug("authenticating request"),
    true;
authenticate(#cb_context{req_nouns=[{<<"openid_auth">>,[<<"checkauth">>, _]}]}) ->
    lager:debug("authenticating request"),
    true.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods/1 :: (path_token()) -> http_methods().
-spec allowed_methods/2 :: (path_token(), path_token()) -> http_methods().
allowed_methods(_) ->
    ['GET'].
allowed_methods(<<"checkauth">>, _) ->
    ['GET'].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists/1 :: (path_token()) -> 'true'.
-spec resource_exists/2 :: (path_token(), path_token()) -> 'true'.
resource_exists(_) ->
    true.
resource_exists(<<"checkauth">>, _) ->
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
-spec validate/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
-spec validate/3 :: (#cb_context{}, path_token(), path_token()) -> #cb_context{}.
validate(Context, Provider) ->
    case lists:member(Provider, ?IdPs) of
        true ->
            lager:debug("requested openid identity provider ~s is supported", [Provider]),
            Context#cb_context{resp_status=success};
        false ->
            lager:debug("requested openid identity provider ~s is unsupported", [Provider]),
            Context#cb_context{resp_status=error
                               ,resp_error_msg = <<"unsupported openid identity provider">>
                               ,resp_error_code=400
                              }
    end.

validate(Context, <<"checkauth">>, _) ->
    Context#cb_context{resp_status=success}.

-spec get/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
-spec get/3 :: (#cb_context{}, path_token(), path_token()) -> #cb_context{}.
get(#cb_context{query_json=QS}=Context, Provider) ->
    _ = cb_context:put_reqid(Context),
    Realm = whapps_config:get(?OPENID_CONFIG_CATEGORY, <<"realm">>),

    %% find the discovery URL of the IdP
    true = is_list(ProviderUrl = get_provider_url(Provider)),

    %% if this is a popup then we will not do redirects
    Popup = wh_json:is_true(<<"popup">>, QS),

    %% we cant just put the UUID on the url, that would defeat the purpose
    CacheKey = wh_util:to_hex_binary(crypto:rand_bytes(16)),
    Seed = wh_util:to_hex_binary(crypto:rand_bytes(32)),
    wh_cache:store(CacheKey, {Seed, Provider, Popup}, 60),

    %% build up our URL
    Return = list_to_binary([Realm, "/openid_auth/checkauth/", wh_util:to_list(CacheKey)]),

    %% HELO IdP
    case gen_server:call(openid_auth_srv, {prepare, Seed, ProviderUrl}) of
        %% Yay! Its friendly.. redirect the user to it
        {ok, AuthReq} when Popup ->
            Location = wh_util:to_binary(openid:authentication_url(AuthReq, Return, Realm)),
            lager:debug("providing redirect location ~s as openid auth ~s", [Location, Seed]),
            Context#cb_context{resp_data=wh_json:from_list([{<<"location">>, Location}])
                               ,resp_status=success};
        {ok, AuthReq} ->
            Location = openid:authentication_url(AuthReq, Return, Realm),
            lager:debug("redirecting client to ~s as openid auth ~s", [Location, Seed]),
            redirect_client(Location, Context);
        %% Must be grumpy today
        {error, Error} ->
            lager:debug("openid auth srv prepare: ~p", [Error]),
            E = wh_util:to_binary(Error),
            crossbar_util:response(fatal, E, Context)
    end.

get(#cb_context{query_json=QS}=Context, <<"checkauth">>, CacheKey) ->
    _ = cb_context:put_reqid(Context),

    Realm = whapps_config:get(?OPENID_CONFIG_CATEGORY, <<"realm">>),
    RegUrl = whapps_config:get(?OPENID_CONFIG_CATEGORY, <<"reg_url">>),
    AppUrl = whapps_config:get(?OPENID_CONFIG_CATEGORY, <<"app_url">>),

    %% get the UUID that we stored when we started this
    %% NOTE: this restricts the return_to to the same machine the redirected the user (cache is local)
    {ok, {Seed, Provider, Popup}} = wh_cache:fetch(CacheKey),

    %% determine the return URL we used
    Return = list_to_binary([Realm, "/openid_auth/checkauth/", wh_util:to_list(CacheKey)]),

    %% checkid_setup with the IdP
    case gen_server:call(openid_auth_srv, {verify, Seed, Return, QS}) of
        %% GREAT SUCCESS, now do we know who this is...
        {ok, IdentityUrl} ->
            Identity = get_identity(IdentityUrl, Provider, QS),
            case find_account(Identity, Provider) of
                %% ...we do, we do
                {ok, AccountId} when Popup->
                    lager:debug("determined that ~s id ~s is associated with account ~s", [Provider, Identity, AccountId]),
                    create_token(IdentityUrl, AccountId, Context);
                {ok, AccountId} ->
                    #cb_context{auth_token=AuthToken} = create_token(IdentityUrl, AccountId, Context),
                    Location = list_to_binary([AppUrl, "?account_id=", AccountId, "&token=", AuthToken]),
                    lager:debug("redirecting client to web app url: ~s", [Location]),
                    redirect_client(Location, Context);
                %% ...nope-ish
                {error, _} when Popup->
                    JObj = wh_json:from_list(extract_attributes(QS)),
                    lager:debug("determined that ~s id ~s (~s) has no associated account", [Provider, Identity, wh_json:get_value(<<"email">>, JObj)]),
                    Context#cb_context{resp_data=JObj
                                       ,resp_status=error
                                       ,resp_error_code=400
                                       ,resp_error_msg = <<"not registered">>
                                      };
                {error, _}  ->
                    RespQS = mochiweb_util:urlencode(extract_attributes(QS)),
                    Location = list_to_binary([RegUrl, "?", RespQS]),
                    lager:debug("redirecting client to registration url: ~s", [Location]),
                    redirect_client(Location, Context)
            end;
        %% bugger
        {error, Error} ->
            lager:debug("openid auth srv verify error: ~p", [Error]),
            E = wh_util:to_binary(Error),
            crossbar_util:response(error, E, 400, Context)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is used to identify the IdP from a library of
%% supported providers. Returns the providers discovery URL.
%%
%% TODO: Currently google is hardcoded but this should draw from
%% a list of IdPs
%% @end
%%--------------------------------------------------------------------
-spec get_provider_url/1 :: (ne_binary()) -> nonempty_string() | 'undefined'.
get_provider_url(<<"google">>) ->
    "http://google.com/accounts/o8/id";
get_provider_url(_) ->
    undefined.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% IdP specific identity extractor, this will allow the url to change
%% without breaking our account mappings
%% @end
%%--------------------------------------------------------------------
-spec get_identity/3 :: (ne_binary(), ne_binary(), wh_json:json_object()) -> ne_binary().
get_identity(IdentityUrl, <<"google">>, _QS) ->
    {_, _, _, IdentityQS, _} = mochiweb_util:urlsplit(IdentityUrl),
    wh_util:to_binary(wh_json:get_value(<<"id">>, mochiweb_util:parse_qs(IdentityQS))).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% find the account id mapping from the IdP unique identifier
%% @end
%%--------------------------------------------------------------------
-spec find_account/2 :: (ne_binary(), ne_binary()) ->
                                {'ok', ne_binary()} |
                                {'error', atom()}.
find_account(Identifier, Provider) ->
    case couch_mgr:get_results(?WH_ACCOUNTS_DB, <<?WH_ACCOUNTS_DB/binary, "listing_by_openid">>, [{key, [Identifier, Provider]}]) of
        {ok, []} ->
            {error, not_registered};
        {ok, [JObj]} ->
            AccountId = wh_json:get_value([<<"value">>, <<"account_id">>], JObj),
            lager:debug("found openid ~s belongs to account ~s", [AccountId]),
            {ok, AccountId};
        {error, R}=E ->
            lager:debug("failed to find account for ~s from ~s, ~p", [Identifier, Provider, R]),
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to create a token and save it to the token db
%% @end
%%--------------------------------------------------------------------
-spec create_token/3 :: (ne_binary(), ne_binary(), #cb_context{}) -> #cb_context{}.
create_token(IdentityUrl, AccountId, Context) ->
    Token = wh_json:from_list( [{<<"account_id">>, AccountId}
                                %%,{<<"owner_id">>, OwnerId}
                                ,{<<"created">>, calendar:datetime_to_gregorian_seconds(calendar:universal_time())}
                                ,{<<"modified">>, calendar:datetime_to_gregorian_seconds(calendar:universal_time())}
                                ,{<<"method">>, wh_util:to_binary(?MODULE)}
                                ,{<<"openid_identity_url">>, wh_util:to_binary(IdentityUrl)}
                               ]),
    case couch_mgr:save_doc(?TOKEN_DB, Token) of
        {ok, Doc} ->
            AuthToken = wh_json:get_value(<<"_id">>, Doc),
            lager:debug("created new local auth token ~s", [AuthToken]),
            crossbar_util:response(wh_json:from_list([{<<"account_id">>, AccountId}
                                                      ,{<<"owner_id">>, <<>>}
                                                     ])
                                   ,Context#cb_context{auth_token=AuthToken, auth_doc=Doc});
        {error, R} ->
            lager:debug("could not create new local auth token, ~p", [R]),
            crossbar_util:response(error, <<"invalid credentials">>, 401, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Extract all known attributes that may have been returned by the
%% openid provider
%% @end
%%--------------------------------------------------------------------
-spec extract_attributes/1 :: (wh_json:json_object()) ->  [{ne_binary(), ne_binary()},...] | [].
extract_attributes(QS) ->
    Attributes = [{<<"http://axschema.org/contact/email">>, <<"email">>}
                  ,{<<"http://axschema.org/namePerson/first">>, <<"first_name">>}
                  ,{<<"http://axschema.org/namePerson/last">>, <<"last_name">>}
                  ,{<<"http://axschema.org/pref/language">>, <<"lang">>}
                  ,{<<"http://axschema.org/contact/country/home">>, <<"country">>}
                 ],

    [normalize(QS, Name, K) || {K, V} <- wh_json:to_proplist(QS),
                         (Name = props:get_value(V, Attributes)) =/= undefined].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Extract known attributes that may have been returned by the
%% openid provider and accumulate a proplist (normalizing the names)
%% @end
%%--------------------------------------------------------------------
normalize(QS, NormalizedName, K) ->
    %% heavy handed approach to namespace, should only operate in "http://openid.net/srv/ax/1.0"
    %% ...getting it done fast
    VKey = re:replace(K, "\\.type\\.", ".value.", [{return, list}]),
    {NormalizedName, wh_json:get_binary_value(VKey, QS, <<>>)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update the crossbar context to cause the client to be redirected
%% to a given URL
%% @end
%%--------------------------------------------------------------------
-spec redirect_client/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
redirect_client(Location, Context) ->
    crossbar_util:response_redirect(Context, Location, wh_json:new(), 302).
