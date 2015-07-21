%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(knm_locality).

-export([fetch/1]).

-include("knm.hrl").

-define(LOCALITY_CONFIG_CAT, <<"number_manager.locality">>).

-define(DEFAULT_URL, whapps_config:get(<<"number_manager.other">>, <<"phonebook_url">>)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch(ne_binary() | ne_binaries()) -> {'ok', wh_json:object()} | {'error', 'lookup_failed' | 'missing_url'}.
fetch(Number) when is_binary(Number)->
    fetch([Number]);
fetch(Numbers) when is_list(Numbers)->
    case whapps_config:get(?LOCALITY_CONFIG_CAT, <<"url">>, ?DEFAULT_URL) of
        'undefined' ->
            lager:error("could not get number locality url"),
            {'error', 'missing_url'};
        Url ->
            ReqBody = wh_json:set_value(<<"data">>, Numbers, wh_json:new()),
            Uri = <<Url/binary, "/locality/metadata">>,
            case ibrowse:send_req(binary:bin_to_list(Uri), [], 'post', wh_json:encode(ReqBody)) of
                {'error', Reason} ->
                    lager:error("number locality lookup failed: ~p", [Reason]),
                    {'error', 'lookup_failed'};
                {'ok', "200", _Headers, Body} ->
                    handle_resp(wh_json:decode(Body));
                {'ok', _Status, _, _Body} ->
                    lager:error("number locality lookup failed: ~p ~p", [_Status, _Body]),
                    {'error', 'lookup_failed'}
            end
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
-spec handle_resp(wh_json:object()) -> {'error', 'lookup_failed'} | {'ok', wh_json:object()}.
handle_resp(Resp) ->
    case wh_json:get_value(<<"status">>, Resp, <<"error">>) of
        <<"success">> ->
            {'ok', wh_json:get_value(<<"data">>, Resp, wh_json:new())};
        _E ->
            lager:error("number locality lookup failed, status: ~p", [_E]),
            {'error', 'lookup_failed'}
    end.