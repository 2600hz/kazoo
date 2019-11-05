%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_locality).

-export([fetch/1]).
-export([prefix/3]).

-include("knm.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fetch(kz_term:ne_binaries()) -> {'ok', kz_json:object()} |
                                      {'error', 'lookup_failed' | 'missing_url'}.
fetch(Numbers) when is_list(Numbers)->
    case knm_config:locality_url() of
        'undefined' ->
            lager:error("could not get number locality url"),
            {'error', 'missing_url'};
        Url ->
            Resp = fetch_req(Numbers, Url),
            handle_resp(Resp)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec prefix(kz_term:text(), kz_term:text(), kz_term:text()) -> {'ok', kz_json:object()} |
                                                                {'error', kz_json:object()}.
prefix(Url, Country, City) ->
    ReqParam = kz_http_util:urlencode(City),
    Uri = lists:flatten([Url, "/", Country, "/city?pattern=", ReqParam]),
    case kz_http:get(Uri) of
        {'ok', 200, _Headers, Body} ->
            JObj = kz_json:decode(Body),
            case kz_json:get_value(<<"data">>, JObj) of
                'undefined' -> {'error', JObj};
                Data -> {'ok', Data}
            end;
        {'ok', _Status, _Headers, Body} ->
            {'error', kz_json:decode(Body)};
        {'error', Reason} ->
            JObj = kz_json:from_list([{<<"unknown error">>, kz_term:to_binary(Reason)}]),
            {'error', JObj}
    end.


%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fetch_req(kz_term:ne_binaries(), kz_term:ne_binary()) -> kz_http:http_ret().
fetch_req(Numbers, Url) ->
    ReqBody = kz_json:from_list([{<<"data">>, Numbers}]),
    Uri = <<Url/binary, "/locality/metadata">>,
    Headers = [{"content_type", "application/json"}],
    kz_http:post(Uri, Headers, kz_json:encode(ReqBody)).

-spec handle_resp(kz_http:http_ret()) -> {'ok', kz_json:object()} |
                                         {'error', 'lookup_failed'}.
handle_resp({'error', Reason}) ->
    lager:error("number locality lookup failed: ~p", [Reason]),
    {'error', 'lookup_failed'};
handle_resp({'ok', 200, _Headers, Body}) ->
    handle_resp_body(kz_json:decode(Body));
handle_resp({'ok', _Status, _, _Body}) ->
    lager:error("number locality lookup failed: ~p ~p", [_Status, _Body]),
    {'error', 'lookup_failed'}.

-spec handle_resp_body(kz_json:object()) -> {'ok', kz_json:object()} |
                                            {'error', 'lookup_failed'}.
handle_resp_body(Resp) ->
    case kz_json:get_value(<<"status">>, Resp, <<"error">>) of
        <<"success">> ->
            {'ok', kz_json:get_value(<<"data">>, Resp, kz_json:new())};
        _E ->
            lager:error("number locality lookup failed, status: ~p", [_E]),
            {'error', 'lookup_failed'}
    end.
