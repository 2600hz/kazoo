%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%%
%%% @contributors
%%%-------------------------------------------------------------------

-module(webhooks_channel_answer).

-export([init/0
	,bindings_and_responders/0
        ]).

-include("webhooks.hrl").

-define(ID, kz_util:to_binary(?MODULE)).
-define(NAME, <<"channel_answer">>).
-define(DESC, <<"Events for when the channel is answered by the endpoint">>).
-define(METADATA
       ,kz_json:from_list([{<<"_id">>, ?ID}
			  ,{<<"name">>, ?NAME}
			  ,{<<"description">>, ?DESC}
			  ])
       ).

-spec init() -> 'ok'.
init() ->
    webhooks_util:init_metadata(?ID, ?METADATA).

-spec bindings_and_responders() ->
                                     {gen_listener:bindings()
				     ,gen_listener:responders()
                                     }.
bindings_and_responders() ->
    {[{'call', [{'restrict_to', ['CHANNEL_ANSWER']}
	       ,'federate'
               ]
      }
     ]
    ,[{{'webhooks_channel_util', 'handle_event'}
      ,[{<<"call_event">>, <<"CHANNEL_ANSWER">>}]
      }
     ]
    }.
