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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch(ne_binary() | ne_binaries()) ->
                   {'ok', wh_json:object()} |
                   {'error', 'lookup_failed' | 'missing_url'}.
fetch(Number) when is_binary(Number)->
    fetch([Number]);
fetch(Numbers) when is_list(Numbers)->
    case knm_config:locality_url() of
        'undefined' ->
            lager:error("could not get number locality url"),
            {'error', 'missing_url'};
        Url ->
            Resp = fetch_req(Numbers, Url),
            handle_resp(Resp)
    end.

-spec fetch_req(ne_binaries(), ne_binary()) -> ibrowse_ret().
fetch_req(Numbers, Url) ->
    ReqBody = wh_json:set_value(<<"data">>, Numbers, wh_json:new()),
    Uri = <<Url/binary, "/locality/metadata">>,
    ibrowse:send_req(binary:bin_to_list(Uri), [], 'post', wh_json:encode(ReqBody)).

-spec handle_resp(ibrowse_ret()) ->
                         {'ok', wh_json:object()} |
                         {'error', any()}.
handle_resp({'error', Reason}) ->
    lager:error("number locality lookup failed: ~p", [Reason]),
    {'error', 'lookup_failed'};
handle_resp({'ok', "200", _Headers, Body}) ->
    handle_resp_body(wh_json:decode(Body));
handle_resp({'ok', _Status, _, _Body}) ->
    lager:error("number locality lookup failed: ~p ~p", [_Status, _Body]),
    {'error', 'lookup_failed'}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_resp_body(wh_json:object()) ->
                              {'ok', wh_json:object()} |
                              {'error', 'lookup_failed'}.
handle_resp_body(Resp) ->
    case wh_json:get_value(<<"status">>, Resp, <<"error">>) of
        <<"success">> ->
            {'ok', wh_json:get_value(<<"data">>, Resp, wh_json:new())};
        _E ->
            lager:error("number locality lookup failed, status: ~p", [_E]),
            {'error', 'lookup_failed'}
    end.
