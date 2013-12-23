%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%% Hotdesks module
%%%
%%% Handle client requests for hotdesks management
%%%
%%% @end
%%% @contributors
%%%   Edouard Swiac
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_hotdesks).

-export([init/0
         ,allowed_methods/0
         ,resource_exists/0
         ,validate/1
        ]).

-include("../crossbar.hrl").

-define(VIEW_FILE, <<"views/hotdesks.json">>).
-define(CB_LIST, <<"hotdesks/crossbar_listing">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.hotdesks">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.hotdesks">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.hotdesks">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.hotdesks">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.hotdesks">>, ?MODULE, 'post'),
    crossbar_bindings:bind(<<"*.execute.delete.hotdesks">>, ?MODULE, 'delete').

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
allowed_methods() -> [?HTTP_GET].

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
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(#cb_context{req_verb = ?HTTP_GET}=Context) ->
    Type = wh_json:get_value(<<"pvt_type">>, cb_context:doc(Context)),
    route_by_type(Type, Context).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(wh_json:object(), wh_json:objects()) ->
                                    wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

-spec route_by_type(ne_binary(), cb_context:context()) ->
                           cb_context:context().
route_by_type('undefined', Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2);
route_by_type(<<"device">>, Context) ->
    UserIds = wh_json:get_value([<<"hotdesk">>, <<"users">>], cb_context:doc(Context), wh_json:new()),
    UserJObjs = wh_json:foldl(
                  fun(UserId, _, Acc) ->
                          case get_username(UserId, cb_context:account_db(Context)) of
                              'undefined' -> Acc;
                              JObj -> [JObj|Acc]
                          end
                  end, [], UserIds),
    case UserJObjs of
        [] -> cb_context:add_system_error('not_found', Context);
        RespData ->
            cb_context:set_resp_data(
              cb_context:set_resp_status(Context, 'success')
              ,RespData
             )
    end;
route_by_type(<<"user">>, #cb_context{doc=Doc}=Context) ->
    UserId = wh_json:get_value(<<"_id">>, Doc),
    crossbar_doc:load_view(?CB_LIST, [{<<"key">>, UserId}], Context, fun normalize_view_results/2).

-spec get_username(ne_binary(), ne_binary()) -> api_object().
get_username(UserId, AccoundDb) ->
    case couch_mgr:open_cache_doc(AccoundDb, UserId) of
        {'error', _} -> 'undefined';
        {'ok', JObj} ->
            FirstName = wh_json:get_value(<<"first_name">>, JObj),
            LastName = wh_json:get_value(<<"last_name">>, JObj),
            wh_json:from_list([{<<"first_name">>, FirstName}
                               ,{<<"last_name">>, LastName}
                               ,{<<"id">>, UserId}
                              ])
    end.
