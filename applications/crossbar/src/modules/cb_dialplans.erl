%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors:
%%%   Karl Anderson
%%%   James Aimonetti
%%%   Sponsored by Conversant Ltd,
%%%     implemented by SIPLABS, LLC (Ilya Ashchepkov)
%%%-------------------------------------------------------------------
-module(cb_dialplans).

-export([init/0
         ,allowed_methods/1
         ,resource_exists/1
         ,validate/1
        ]).

-include("../crossbar.hrl").

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
    _ = crossbar_bindings:bind(<<"*.allowed_methods.dialplans">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.dialplans">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.dialplans">>, ?MODULE, 'validate').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods(cb_context:context()) -> http_methods().
allowed_methods(_Context) ->
    [?HTTP_GET].

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec resource_exists(cb_context:context()) -> api_util:resource_existence().
resource_exists(Context) -> {'true', Context}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    Doc = wh_json:from_list(whapps_config:get_all_kvs(<<"dialplans">>)),
    cb_context:setters(Context, [{fun cb_context:set_resp_data/2, Doc}
                                 ,{fun cb_context:set_resp_status/2, 'success'}
                                ]).
