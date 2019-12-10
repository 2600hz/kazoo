%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(wg_httpc).

-export([post/0, post/1
        ,user_agent/0
        ,version/0]).

-include("waveguide.hrl").

%%------------------------------------------------------------------------------
%% @doc construct default waveguide headers proplist
%% @end
%%------------------------------------------------------------------------------
-spec default_headers() -> kz_http:headers().
default_headers() ->
    [{"Accept", "application/json"}
    ,{"Content-Type", "application/json"}
    ,{"User-Agent", user_agent()}
    ,{"X-Waveguide-version", version()}
    ].

%%------------------------------------------------------------------------------
%% @doc submit a main ping to waveguide
%% @end
%%------------------------------------------------------------------------------
-spec post() -> kz_http:httpc_ret().
post() ->
    {'ok', Ping} = waveguide_reducer:ping(),
    post(Ping).

%%------------------------------------------------------------------------------
%% @doc submit a ping object to waveguide
%% @end
%%------------------------------------------------------------------------------
-spec post(kz_json:object()) ->
          {'ok', iodata()} |
          {'retry', kz_json:object()}.
post(PingObj) ->
    Type = kz_json:get_ne_binary_value(<<"type">>, PingObj),
    Response = kz_http:post(endpoint_url(Type), default_headers(), kz_json:encode(PingObj)),
    handle_response(Response).

%%------------------------------------------------------------------------------
%% @doc helper for waveguide base url
%% @end
%%------------------------------------------------------------------------------
-spec base_url() -> kz_term:ne_binary().
base_url() -> ?WG_URL.

%%------------------------------------------------------------------------------
%% @doc helper for waveguide endpoint specific urls
%% @end
%%------------------------------------------------------------------------------
-spec endpoint_url(kz_term:ne_binary()) -> kz_term:ne_binary().
endpoint_url(?WG_ACTIVATION_PING) ->
    <<(base_url())/binary,"/activate">>;
endpoint_url(?WG_HANDSHAKE_PING) ->
    <<(base_url())/binary,"/handshake">>;
endpoint_url(?WG_MAIN_PING) ->
    <<(base_url())/binary,"/collector">>.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_response(kz_http:httpc_ret()) ->
          {'ok', kz_term:text()} |
          {'retry', kz_term:text()} |
          {'error', any()}.
handle_response({'ok', Code, _, Body})
  when Code >=200
       andalso Code < 300  ->
    {'ok', Body};
handle_response({'ok', Code, _, Body})
  when Code >=400
       andalso Code < 500 ->
    {'ok', Body};
handle_response({'ok', Code, _, Body})
  when Code >=500
       andalso Code < 600 ->
    {'retry', Body};
handle_response(Response) -> {'retry', Response}.

%%------------------------------------------------------------------------------
%% @doc helper for waveguide user-agent
%% @end
%%------------------------------------------------------------------------------
-spec user_agent() -> string().
user_agent() -> ?WG_USER_AGENT.

%%------------------------------------------------------------------------------
%% @doc helper for current waveguide version
%% @end
%%------------------------------------------------------------------------------
-spec version() -> string().
version() -> ?WG_VERSION.
