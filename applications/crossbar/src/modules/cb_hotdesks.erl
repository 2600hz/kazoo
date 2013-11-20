%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
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
    _ = crossbar_bindings:bind(<<"*.allowed_methods.hotdesks">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"*.resource_exists.hotdesks">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"*.validate.hotdesks">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"*.execute.put.hotdesks">>, ?MODULE, put),
    _ = crossbar_bindings:bind(<<"*.execute.post.hotdesks">>, ?MODULE, post),
    crossbar_bindings:bind(<<"*.execute.delete.hotdesks">>, ?MODULE, delete).

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
resource_exists() ->
    true.

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
validate(#cb_context{req_verb = ?HTTP_GET, doc=Doc}=Context) ->
    Type = wh_json:get_value(<<"pvt_type">>, Doc, <<"undefined">>),
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
-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

-spec route_by_type(ne_binary(), cb_context:context()) ->cb_context:context(). 
route_by_type(<<"undefined">>, Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2);
route_by_type(<<"device">>, #cb_context{doc=Doc, db_name=AccoundDb}=Context) -> 
    UserIds = wh_json:to_proplist(wh_json:get_value([<<"hotdesk">>, <<"users">>], Doc, wh_json:new())),
    JObjs = lists:foldl(
              fun(UserId, Acc) -> 
                      case get_username(UserId, AccoundDb) of
                          'undefined' ->
                              Acc;
                          JObj ->
                              [JObj|Acc]
                      end
              end, [], UserIds),
    case JObjs of
        [] ->
            cb_context:add_system_error('not_found', Context);
        RespData ->    
            Context#cb_context{resp_status=success
                               ,resp_data=RespData
                              }
    end;
route_by_type(<<"user">>, #cb_context{doc=Doc}=Context) ->
    UserId = wh_json:get_value(<<"_id">>, Doc),
    crossbar_doc:load_view(?CB_LIST, [{<<"key">>, UserId}], Context, fun normalize_view_results/2).

-spec get_username({ne_binary(), any()}, ne_binary()) -> wh_json:object().
get_username({Id, _}, AccoundDb) ->
    case couch_mgr:open_cache_doc(AccoundDb, Id) of
        {'ok', JObj} ->
            FirstName = wh_json:get_value(<<"first_name">>, JObj),
            LastName = wh_json:get_value(<<"last_name">>, JObj),
            wh_json:set_values([{<<"first_name">>, FirstName}
                                ,{<<"last_name">>, LastName}
                                ,{<<"id">>, Id}]
                               ,wh_json:new());
        _ ->
            'undefined'
    end;
get_username(_, _) ->
    'undefined'.

    
