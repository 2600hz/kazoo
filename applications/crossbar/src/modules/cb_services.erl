%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%%
%%%
%%% @end
%%% @contributors:
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_services).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,validate/1, validate/2
         ,get/1, get/2
         ,post/1
        ]).

-include("../crossbar.hrl").

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".services">>).

-define(PVT_TYPE, <<"service">>).
-define(PVT_FUNS, [fun add_pvt_type/2]).
-define(CB_LIST, <<"services/crossbar_listing">>).
-define(AUDIT_LOG_LIST, <<"services/audit_logs">>).

-define(PATH_PLAN, <<"plan">>).
-define(PATH_AUDIT, <<"audit">>).

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
    _ = crossbar_bindings:bind(<<"*.allowed_methods.services">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.services">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.services">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.get.services">>, ?MODULE, 'get'),
    _ = crossbar_bindings:bind(<<"*.execute.put.services">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.services">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.services">>, ?MODULE, 'delete').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods() | [].
-spec allowed_methods(path_token()) -> http_methods() | [].

allowed_methods() ->
    [?HTTP_GET, ?HTTP_POST].

allowed_methods(?PATH_PLAN) ->
    [?HTTP_GET];
allowed_methods(?PATH_AUDIT) ->
    [?HTTP_GET];
allowed_methods(_) ->
    [].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /services => []
%%    /services/foo => [<<"foo">>]
%%    /services/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> boolean().
resource_exists() -> 'true'.

resource_exists(?PATH_PLAN) -> 'true';
resource_exists(?PATH_AUDIT) -> 'true';
resource_exists(_) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /services mights load a list of service objects
%% /services/123 might load the service object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().

validate(Context) ->
    validate_services(Context, cb_context:req_verb(Context)).

validate_services(Context, ?HTTP_GET) ->
    crossbar_util:response(wh_services:public_json(cb_context:account_id(Context)), Context);
validate_services(Context, ?HTTP_POST) ->
    BillingId = wh_json:get_value(<<"billing_id">>, cb_context:req_data(Context)),
    try wh_services:set_billing_id(BillingId, cb_context:account_id(Context)) of
        'undefined' ->
            cb_context:setters(Context
                               ,[{fun cb_context:set_doc/2, 'undefined'}
                                 ,{fun cb_context:set_resp_status/2, 'success'}
                                ]);
        ServicesRec ->
            cb_context:setters(Context
                               ,[{fun cb_context:set_doc/2, wh_services:public_json(ServicesRec)}
                                 ,{fun cb_context:store/3, 'services', ServicesRec}
                                 ,{fun cb_context:set_resp_status/2, 'success'}
                                ])
    catch
        'throw':{Error, Reason} ->
            R = wh_json:set_value([<<"billing_id">>, <<"invalid">>], Reason, wh_json:new()),
            crossbar_util:response('error', wh_util:to_binary(Error), 400, R, Context)
    end.

validate(Context, ?PATH_PLAN) ->
    crossbar_util:response(wh_services:service_plan_json(cb_context:account_id(Context)), Context);
validate(Context, ?PATH_AUDIT) ->
    load_audit_logs(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is a GET, execute necessary code to fulfill the GET
%% request. Generally, this will involve stripping pvt fields and loading
%% the resource into the resp_data, resp_headers, etc...
%% @end
%%--------------------------------------------------------------------
-spec get(cb_context:context()) -> cb_context:context().
-spec get(cb_context:context(), path_token()) -> cb_context:context().

get(Context) ->
    Context.

get(Context, ?PATH_PLAN) ->
    Context;
get(Context, ?PATH_AUDIT) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context()) -> cb_context:context().
post(Context) ->
    post(Context, cb_context:fetch(Context, 'services')).

post(Context, 'undefined') -> Context;
post(Context, Services) ->
    try wh_services:save(Services) of
        NewServices ->
            crossbar_util:response(wh_services:public_json(NewServices), Context)
    catch
        'throw':{Error, Reason} ->
            crossbar_util:response('error', wh_util:to_binary(Error), 500, Reason, Context)
    end.

-spec load_audit_logs(cb_context:context()) -> cb_context:context().
load_audit_logs(Context) ->
    case create_view_options(Context) of
        {'ok', ViewOptions} ->
            crossbar_doc:load_view(?AUDIT_LOG_LIST
                                   ,ViewOptions
                                   ,Context
                                  );
        Context1 -> Context1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec pagination_page_size(cb_context:context()) ->pos_integer().
pagination_page_size(Context) ->
    case crossbar_doc:pagination_page_size(Context) of
        'undefined' -> 'undefined';
        PageSize -> PageSize + 1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create_view_options(cb_context:context()) ->
                                 {'ok', wh_proplist()} |
                                 cb_context:context().
create_view_options(Context) ->
    MaxRange = whapps_config:get_integer(?MOD_CONFIG_CAT
                                         ,<<"maximum_range">>
                                         ,(?SECONDS_IN_DAY * 31  + ?SECONDS_IN_HOUR)
                                        ),
    case cb_modules_util:range_view_options(Context, MaxRange) of
        {CreatedFrom, CreatedTo} ->
            create_view_options(Context, CreatedFrom, CreatedTo);
        Context1 -> Context1
    end.

-spec create_view_options(cb_context:context(), gregorian_seconds(), gregorian_seconds()) ->
                                 {'ok', wh_proplist()}.
create_view_options(Context, CreatedFrom, CreatedTo) ->
    {'ok', [{'startkey', CreatedTo}
            ,{'endkey', CreatedFrom}
            ,{'limit', pagination_page_size(Context)}
            ,{'databases', ranged_modbs(Context, CreatedFrom, CreatedTo)}
           ,'descending'
           ]}.

-spec ranged_modbs(cb_context:context(), gregorian_seconds(), gregorian_seconds()) ->
                          ne_binaries().
ranged_modbs(Context, From, To) ->
    {{FromYear, FromMonth, _}, _} = calendar:gregorian_seconds_to_datetime(From),
    {{ToYear, ToMonth, _}, _} = calendar:gregorian_seconds_to_datetime(To),

    Seq = crossbar_util:generate_year_month_sequence({FromYear, FromMonth}
                                                     ,{ToYear, ToMonth}
                                                    ),
    AccountId = cb_context:account_id(Context),
    [kazoo_modb:get_modb(AccountId, Year, Month) || {Year, Month} <- Seq].
