%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, 2600Hz INC
%%%
%%% @contributors
%%%-------------------------------------------------------------------

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
    kz_util:put_callid(JObj),
    lager:debug("event handled").
