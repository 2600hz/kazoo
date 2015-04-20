%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
%%% @doc
%%%  The module generalizes an access to the eradius app.
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Vladimir Potapev)
%%%-------------------------------------------------------------------
-module(cm_radius).

%% API
-export([add_request_authz/2, add_request_start/2, add_request_stop/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec add_request_authz(wh_json:object(), boolean()) -> any().
add_request_authz(_JObj, true) ->
    not_implemented.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec add_request_start(wh_json:object(), boolean()) -> any().
add_request_start(_JObj, true) ->
    not_implemented.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec add_request_stop(wh_json:object(), boolean()) -> any().
add_request_stop(_JObj, true) ->
    not_implemented.
