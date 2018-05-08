%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2018, 2600Hz
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(webhooks_channel_answer).

-export([init/0
        ,bindings_and_responders/0
        ]).

-include("webhooks.hrl").

-define(ID, kz_term:to_binary(?MODULE)).
-define(NAME, <<"Channel Answer">>).
-define(DESC, <<"This webhook is triggered when a channel establishes two-way audio, such as a voicemail box or the called party answering">>).
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
    {[{'call', [{'restrict_to', ['CHANNEL_ANSWER']}
               ]
      }
     ]
    ,[{{'webhooks_channel_util', 'handle_event'}
      ,[{<<"call_event">>, <<"CHANNEL_ANSWER">>}]
      }
     ]
    }.
