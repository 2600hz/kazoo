%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc Listing of all expected v1 callbacks
%%% @author Peter Defebvre
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_alerts).

-export([init/0
        ,allowed_methods/0, allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,validate/1, validate/2
        ,put/1
        ,delete/2
        ]).

-include("crossbar.hrl").

-define(AVAILABLE_LIST, <<"alerts/available">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.alerts">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.alerts">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.alerts">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.get.alerts">>, ?MODULE, 'get'),
    _ = crossbar_bindings:bind(<<"*.execute.put.alerts">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.alerts">>, ?MODULE, 'delete').

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_AlertId) ->
    [?HTTP_GET, ?HTTP_DELETE].

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource.
%% For example:
%%
%% ```
%%    /alerts => []
%%    /alerts/foo => [<<"foo">>]
%%    /alerts/foo/bar => [<<"foo">>, <<"bar">>]
%% '''
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /alerts might load a list of alert objects
%% /alerts/123 might load the alert object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%------------------------------------------------------------------------------

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_alerts(Context, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, Id) ->
    validate_alert(Context, Id, cb_context:req_verb(Context)).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%------------------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    Context1 = cb_context:set_account_db(Context, ?KZ_ALERTS_DB),
    crossbar_doc:save(Context1).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%------------------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _) ->
    crossbar_doc:delete(Context).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_alerts(cb_context:context(), http_method()) -> cb_context:context().
validate_alerts(Context, ?HTTP_GET) ->
    summary(Context);
validate_alerts(Context, ?HTTP_PUT) ->
    case cb_context:is_superduper_admin(Context) of
        'true' -> create(Context);
        'false' ->
            cb_context:add_system_error('forbidden', Context)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_alert(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_alert(Context, Id, ?HTTP_GET) ->
    read(Id, Context);
validate_alert(Context, Id, ?HTTP_DELETE) ->
    read(Id, Context).

%%------------------------------------------------------------------------------
%% @doc Create a new instance with the data provided, if it is valid
%% @end
%%------------------------------------------------------------------------------
-spec create(cb_context:context()) -> cb_context:context().
create(Context) ->
    Props = kz_json:to_proplist(cb_context:req_data(Context)),

    Title = props:get_value(kzd_alert:title(), Props),
    Msg = props:get_value(kzd_alert:message(), Props),
    From = props:get_value(kzd_alert:from(), Props),
    To = props:get_value(kzd_alert:to(), Props),

    case kapps_alert:create(Title, Msg, From, To, Props) of
        {'required', Item} ->
            cb_context:add_validation_error(Item
                                           ,<<"required">>
                                           ,<<"missing property">>
                                           ,Context
                                           );
        {'error', 'disabled'} ->
            cb_context:add_system_error('disabled', Context);
        {'ok', JObj} ->
            Setters = [{fun cb_context:set_resp_status/2, 'success'}
                      ,{fun cb_context:set_doc/2, JObj}
                      ],
            cb_context:setters(Context, Setters)
    end.

%%------------------------------------------------------------------------------
%% @doc Load an instance from the database
%% @end
%%------------------------------------------------------------------------------
-spec read(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    Context1 = cb_context:set_account_db(Context, ?KZ_ALERTS_DB),
    crossbar_doc:load(Id, Context1, ?TYPE_CHECK_OPTION(<<"alert">>)).

%%------------------------------------------------------------------------------
%% @doc Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%------------------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    Context1 = load_summary(Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            fix_envelope(Context1);
        _ -> Context1
    end.

%%------------------------------------------------------------------------------
%% @doc Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%------------------------------------------------------------------------------
-spec load_summary(cb_context:context()) -> cb_context:context().
load_summary(Context) ->
    Context1 = cb_context:set_account_db(Context, ?KZ_ALERTS_DB),
    crossbar_doc:load_view(?AVAILABLE_LIST
                          ,[{'keys', view_keys(Context1)}]
                          ,Context1
                          ,fun normalize_view_results/2
                          ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fix_envelope(cb_context:context()) -> cb_context:context().
fix_envelope(Context) ->
    RespData = cb_context:resp_data(Context),
    RespEnv = cb_context:resp_envelope(Context),

    Alerts = filter_alerts(RespData),

    Setters = [{fun cb_context:set_resp_data/2, Alerts}
              ,{fun cb_context:set_resp_envelope/2
               ,kz_json:set_value(<<"page_size">>, erlang:length(Alerts), RespEnv)
               }
              ],
    cb_context:setters(Context, Setters).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec filter_alerts(kz_json:objects()) -> kz_json:objects().
filter_alerts(Alerts) ->
    [Alert || Alert <- lists:usort(Alerts),
              should_filter_alert(Alert)
    ].

-spec should_filter_alert(kzd_alert:doc()) -> boolean().
should_filter_alert(Alert) ->
    case kzd_alert:expired(Alert) of
        'false' -> 'true';
        'true' ->
            _ = kz_util:spawn(fun kapps_alert:delete/1, [kzd_alert:id(Alert)]),
            'false'
    end.

%%------------------------------------------------------------------------------
%% @doc Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%------------------------------------------------------------------------------
-spec view_keys(cb_context:context()) -> list().
view_keys(Context) ->
    AuthDoc = cb_context:auth_doc(Context),

    AccountId = cb_context:account_id(Context),
    OwnerId = kz_json:get_value(<<"owner_id">>, AuthDoc),

    IsAdmin = kzd_user:is_account_admin(AccountId, OwnerId),

    Routines = [fun(K) ->
                        [[<<"all">>, <<"all">>]
                        ,[<<"all">>, <<"users">>]
                         |K
                        ]
                end
               ,fun(K) -> [[AccountId, <<"all">>], [AccountId, <<"users">>]|K] end
               ,fun(K) when OwnerId =:= 'undefined' -> K;
                   (K) -> [[AccountId, OwnerId]|K]
                end
               ,fun(K) ->
                        case kz_services_reseller:is_reseller(AccountId) of
                            'false' -> K;
                            'true' -> [[<<"resellers">>, <<"all">>], [<<"resellers">>, <<"users">>]|K]
                        end
                end
               ,fun(K) when IsAdmin ->
                        [[<<"all">>, <<"admins">>]
                        ,[AccountId, <<"admins">>]
                        ,[<<"resellers">>, <<"admins">>]
                         |K
                        ];
                   (K) -> K
                end
               ,fun(K) ->
                        lists:foldl(fun(Descendant, Acc) ->
                                            add_descendants(Descendant, IsAdmin, Acc)
                                    end
                                   ,K
                                   ,crossbar_util:get_descendants(AccountId)
                                   )
                end
               ],
    lists:foldl(fun(F, Keys) -> F(Keys) end, [], Routines).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec add_descendants(kz_term:ne_binary(), boolean(), list()) -> list().
add_descendants(Descendant, 'false', Keys) ->
    [[<<"descendants">>, [Descendant, <<"all">>]]
    ,[<<"descendants">>, [Descendant, <<"users">>]]
     | Keys
    ];
add_descendants(Descendant, 'true', Keys) ->
    [[<<"descendants">>, [Descendant, <<"all">>]]
    ,[<<"descendants">>, [Descendant, <<"users">>]]
    ,[<<"descendants">>, [Descendant, <<"admins">>]]
     | Keys
    ].

%%------------------------------------------------------------------------------
%% @doc Normalizes the results of a view.
%% @end
%%------------------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj)|Acc].
