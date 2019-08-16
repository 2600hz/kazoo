%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_queue_update).

-export([new/0]).
-export([action/1, action/2, set_action/2]).
-export([queue_id/1, queue_id/2, set_queue_id/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"queue_update">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec action(doc()) -> kz_term:api_binary().
action(Doc) ->
    action(Doc, 'undefined').

-spec action(doc(), Default) -> binary() | Default.
action(Doc, Default) ->
    kz_json:get_binary_value([<<"action">>], Doc, Default).

-spec set_action(doc(), binary()) -> doc().
set_action(Doc, Action) ->
    kz_json:set_value([<<"action">>], Action, Doc).

-spec queue_id(doc()) -> kz_term:api_ne_binary().
queue_id(Doc) ->
    queue_id(Doc, 'undefined').

-spec queue_id(doc(), Default) -> kz_term:ne_binary() | Default.
queue_id(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"queue_id">>], Doc, Default).

-spec set_queue_id(doc(), kz_term:ne_binary()) -> doc().
set_queue_id(Doc, QueueId) ->
    kz_json:set_value([<<"queue_id">>], QueueId, Doc).
