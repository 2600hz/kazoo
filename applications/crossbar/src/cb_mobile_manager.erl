%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(cb_mobile_manager).

-export([delete_account/1]).

-include("crossbar.hrl").

-define(MOD_CONFIG_CAT, <<"mobile_manager">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec delete_account(cb_context:context()) -> 'ok'.
delete_account(Context) ->
    case req_uri([<<"accounts">>, cb_context:account_id(Context)]) of
        'undefined' ->
            lager:debug("ignore request mobile_manger url is not set");
        UrlString ->
            lager:debug("mobile_manager delete via ~s", [UrlString]),

            Headers = req_headers(cb_context:auth_token(Context)),

            Resp = kz_http:delete(UrlString, Headers),
            handle_resp(Resp)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_resp(kz_http:ret()) -> 'ok'.
handle_resp({'ok', 200, _, Resp}) ->
    lager:debug("mobile_manager success ~s", [Resp]);
handle_resp({'ok', Code, _, Resp}) ->
    lager:warning("mobile_manager error ~p. ~s", [Code,Resp]);
handle_resp(_Error) ->
    lager:error("mobile_manager fatal error ~p", [_Error]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec req_uri(ne_binaries()) -> api_list().
req_uri(ExplodedPath) ->
    case whapps_config:get_binary(?MOD_CONFIG_CAT, <<"url">>) of
        'undefined' -> 'undefined';
        Url ->
            Uri = wh_util:uri(Url, ExplodedPath),
            wh_util:to_list(Uri)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec req_headers(ne_binary()) -> wh_proplist().
req_headers(AuthToken) ->
    props:filter_undefined(
      [{"Content-Type", "application/json"}
      ,{"X-Auth-Token", wh_util:to_list(AuthToken)}
      ,{"User-Agent", wh_util:to_list(erlang:node())}
      ]).
