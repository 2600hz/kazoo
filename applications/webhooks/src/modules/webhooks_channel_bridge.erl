%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz INC
%%%
%%% @contributors
%%%-------------------------------------------------------------------

-module(webhooks_channel_bridge).

-export([init/0
        ,bindings_and_responders/0
        ]).

-include("webhooks.hrl").

-define(ID, kz_term:to_binary(?MODULE)).
-define(NAME, <<"Channel Bridge">>).
-define(DESC, <<"This webhook is triggered when two channels are bridged together, such as two users/devices connected together">>).
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
    {[{'call', [{'restrict_to', ['CHANNEL_BRIDGE']}
               ]
      }
     ]
    ,[{{'webhooks_channel_util', 'handle_event'}
      ,[{<<"call_event">>, <<"CHANNEL_BRIDGE">>}]
      }
     ]
    }.
