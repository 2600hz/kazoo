%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Publishes cdr.report event
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_cdr_event_publisher).

-export([init/0]).
-export([publish_cdr/1]).

-include("ecallmgr.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = kazoo_bindings:bind(<<"event_stream.publish.cdr.report">>, ?MODULE, 'publish_cdr'),
    'ok'.

-spec publish_cdr(map()) -> any().
publish_cdr(#{payload := JObj}) ->
	kapi_cdr:publish_report(JObj).
