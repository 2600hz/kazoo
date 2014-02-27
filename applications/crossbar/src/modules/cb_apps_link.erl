%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% Listing of all expected v1 callbacks
%%%
%%% @end
%%% @contributors:
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_apps_link).

-export([init/0
         ,authorize/1
         ,allowed_methods/1
         ,resource_exists/1
         ,validate/2
        ]).

-include("../crossbar.hrl").

-define(DEFAULT_LANGUAGE, <<"en-US">>).
-define(AUTHORIZE, <<"authorize">>).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Initializes the bindings this module will respond to.
%% @end
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.apps_link">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.apps_link">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.apps_link">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.get.apps_link">>, ?MODULE, 'get').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> 'false'.
authorize(#cb_context{req_nouns=[{<<"apps_link">>, _}]}) ->
    'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?AUTHORIZE) ->
    [?HTTP_GET].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /apps_link => []
%%    /apps_link/foo => [<<"foo">>]
%%    /apps_link/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists(path_token()) -> 'true'.
resource_exists(?AUTHORIZE) -> 'true'.


%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /apps_link mights load a list of skel objects
%% /apps_link/123 might load the skel object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(#cb_context{auth_doc=Doc}=Context, ?AUTHORIZE) ->
    RequestNouns = cb_context:req_nouns(Context),
    AccountId = case props:get_value(<<"accounts">>, RequestNouns) of
                    'undefined' -> wh_json:get_value(<<"account_id">>, Doc, <<>>);
                    [Else] -> Else
                end,
    OwnerId = wh_json:get_value(<<"owner_id">>, Doc, <<>>),
    JObj = populate_resp(Doc, AccountId, OwnerId),
    crossbar_util:response(crossbar_util:response_auth(JObj), Context).



-spec populate_resp(wh_json:object(), ne_binary(), ne_binary()) -> wh_json:object().
populate_resp(JObj, AccountId, UserId) ->
    Routines = [fun(J) -> wh_json:set_value(<<"account_id">>, AccountId, J) end
                ,fun(J) -> wh_json:set_value(<<"apps">>, load_apps(AccountId, UserId), J) end
                ,fun(J) -> wh_json:set_value(<<"language">>, get_language(AccountId, UserId), J) end
                ,fun(J) -> wh_json:set_value(<<"account_name">>, get_account_name(AccountId), J) end
               ],
    lists:foldl(fun(F, J) -> F(J) end, JObj, Routines).

-spec load_apps(ne_binary(), ne_binary()) -> api_object().
load_apps(AccountId, UserId) ->
    MasterAccountDb = get_master_account_db(),
    Lang = get_language(AccountId, UserId),
    case couch_mgr:get_all_results(MasterAccountDb, <<"apps_store/crossbar_listing">>) of
        {'error', _E} ->
            lager:error("failed to load lookup apps in ~p", [MasterAccountDb]),
            'undefined';
        {'ok', JObjs} -> filter_apps(JObjs, Lang)
    end.

-spec filter_apps(wh_json:objects(), ne_binary()) -> wh_json:objects().
-spec filter_apps(wh_json:objects(), ne_binary(), wh_json:objects()) -> wh_json:objects().
filter_apps(JObjs, Lang) ->
    filter_apps(JObjs, Lang, []).

filter_apps([], _, Acc) -> Acc;
filter_apps([JObj|JObjs], Lang, Acc) ->
    App = wh_json:get_value(<<"value">>, JObj, wh_json:new()),
    DefaultLabel = wh_json:get_value([<<"i18n">>, ?DEFAULT_LANGUAGE, <<"label">>], App),
    NewApp = wh_json:from_list([{<<"id">>, wh_json:get_value(<<"id">>, App)}
                                ,{<<"name">>, wh_json:get_value(<<"name">>, App)}
                                ,{<<"api_url">>, wh_json:get_value(<<"api_url">>, App)}
                                ,{<<"label">>, wh_json:get_value([<<"i18n">>, Lang, <<"label">>], App, DefaultLabel)}
                               ]),
    filter_apps(JObjs, Lang, [NewApp|Acc]).

-spec get_language(ne_binary(), ne_binary()) -> ne_binary().
get_language(AccountId, UserId) ->
    case crossbar_util:get_user_lang(AccountId, UserId) of
        {'ok', Lang} -> Lang;
        'error' ->
            case crossbar_util:get_account_lang(AccountId) of
                {'ok', Lang} -> Lang;
                'error' -> ?DEFAULT_LANGUAGE
            end
    end.

-spec get_account_name(ne_binary()) -> ne_binary().
get_account_name(AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_cache_doc(AccountDb, AccountId) of
        {'error', _} -> 'undefined';
        {'ok', JObj} -> wh_json:get_value(<<"name">>, JObj)
    end.

-spec get_master_account_db() -> ne_binary().
get_master_account_db() ->
    {'ok', MasterAccountId} = whapps_util:get_master_account_id(),
    wh_util:format_account_id(MasterAccountId, 'encoded').







