%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, Dinkor Media Group
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Sergey Korobkov
%%%-------------------------------------------------------------------
-module(cb_allotments).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,validate/1, validate/2
         ,post/1
        ]).

-export([is_allowed/1]).

-include("../crossbar.hrl").
-include_lib("whistle/src/wh_json.hrl").

-define(CB_LIST, <<"allotments/crossbar_listing">>).
-define(PVT_TYPE, <<"limits">>).
-define(CONSUMED, <<"consumed">>).
-define(PVT_ALLOTMENTS, <<"pvt_allotments">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.allotments">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.allotments">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.allotments">>, ?MODULE, 'validate'),
   crossbar_bindings:bind(<<"*.execute.post.allotments">>, ?MODULE, 'post').

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
    [?HTTP_GET, ?HTTP_POST].

allowed_methods(?CONSUMED) ->
    [?HTTP_GET].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(?CONSUMED) -> 'true'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_allotments(Context, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, ?CONSUMED) ->
    validate_consumed(Context, cb_context:req_verb(Context)).

-spec validate_allotments(cb_context:context(), http_method()) -> cb_context:context().
validate_allotments(Context, ?HTTP_GET) ->
    load_allotments(Context);
validate_allotments(Context, ?HTTP_POST) ->
    case is_allowed(Context) of
        'true' -> maybe_handle_load_failure(crossbar_doc:load(?PVT_TYPE, Context));
        'false' -> crossbar_util:response_400("sub-accounts of non-master resellers must contact the reseller to change their allotments", wh_json:new(), Context)
    end.

-spec validate_consumed(cb_context:context(), http_method()) -> cb_context:context().
validate_consumed(Context, ?HTTP_GET) ->
    load_consumed(Context).

-spec post(cb_context:context()) -> cb_context:context().
post(Context) ->
    maybe_update_allotments(Context).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a Limit document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_allotments(cb_context:context()) -> cb_context:context().
load_allotments(Context) ->
    Context1 = maybe_handle_load_failure(crossbar_doc:load(?PVT_TYPE, Context)),
    Allotments = wh_json:get_json_value(?PVT_ALLOTMENTS, cb_context:doc(Context1), wh_json:new()),
    cb_context:set_resp_data(Context1, Allotments).


-spec load_consumed(cb_context:context()) -> cb_context:context().
load_consumed(Context) ->
    cb_context:set_doc(Context,wh_json:new()).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_allowed(cb_context:context()) -> boolean().
is_allowed(Context) ->
    AccountId = cb_context:account_id(Context),
    AuthAccountId = cb_context:auth_account_id(Context),
    IsSystemAdmin = wh_util:is_system_admin(AuthAccountId),
    {'ok', MasterAccount} = whapps_util:get_master_account_id(),
    case wh_services:find_reseller_id(AccountId) of
        AuthAccountId ->
            lager:debug("allowing reseller to update allotments"),
            'true';
        MasterAccount ->
            lager:debug("allowing direct account to update allotments"),
            'true';
        _Else when IsSystemAdmin ->
            lager:debug("allowing system admin to update allotments"),
            'true';
        _Else ->
            lager:debug("sub-accounts of non-master resellers must contact the reseller to change their allotments"),
            'false'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_handle_load_failure(cb_context:context()) ->
                                       cb_context:context().
-spec maybe_handle_load_failure(cb_context:context(), pos_integer()) ->
                                       cb_context:context().
maybe_handle_load_failure(Context) ->
    maybe_handle_load_failure(Context, cb_context:resp_error_code(Context)).

maybe_handle_load_failure(Context, 404) ->
    Data = cb_context:req_data(Context),
    NewLimits = wh_json:from_list([{<<"pvt_type">>, ?PVT_TYPE}
                                   ,{<<"_id">>, ?PVT_TYPE}
                                  ]),
    JObj = wh_json_schema:add_defaults(wh_json:merge_jobjs(NewLimits, wh_json:public_fields(Data))
                                       ,<<"limits">>
                                      ),

    cb_context:setters(Context
                       ,[{fun cb_context:set_resp_status/2, 'success'}
                         ,{fun cb_context:set_resp_data/2, wh_json:public_fields(JObj)}
                         ,{fun cb_context:set_doc/2, crossbar_doc:update_pvt_parameters(JObj, Context)}
                        ]);
maybe_handle_load_failure(Context, _RespCode) -> Context.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_update_allotments(cb_context:context()) -> cb_context:context().
maybe_update_allotments(Context) ->
    Doc = cb_context:doc(Context),
    Allotments = cb_context:req_data(Context),
    NewDoc = wh_json:set_value(<<"pvt_allotments">>, Allotments, Doc),
    Context1 = crossbar_doc:save(cb_context:set_doc(Context, NewDoc)),
    cb_context:set_resp_data(Context1, Allotments).

