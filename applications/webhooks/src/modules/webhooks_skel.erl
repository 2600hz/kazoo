%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%%
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(webhooks_skel).

-export([init/0
        ,bindings_and_responders/0
        ,handle_event/2
        ]).

-include("webhooks.hrl").

-define(ID, kz_term:to_binary(?MODULE)).
-define(HOOK_NAME, <<"skel">>).
-define(NAME, <<"Skel">>).
-define(DESC, <<"Example webhook module">>).
-define(METADATA
       ,kz_json:from_list([{<<"_id">>, ?ID}
                          ,{<<"name">>, ?NAME}
                          ,{<<"description">>, ?DESC}
                          ])
       ).

-spec init() -> 'ok'.
init() ->
    webhooks_util:init_metadata(?ID, ?METADATA).

-spec bindings_and_responders() -> {gen_listener:bindings(), gen_listener:responders()}.
bindings_and_responders() ->
    {[{'self', []}]
    ,[{{?MODULE, 'handle_event'}
      ,[{<<"category">>, <<"name">>}]
      }
     ]
    }.

-spec handle_event(kz_json:object(), kz_term:proplist()) -> any().
handle_event(JObj, _Props) ->
    kz_log:put_callid(JObj),
    lager:debug("event handled").
