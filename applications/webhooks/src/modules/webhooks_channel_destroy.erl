%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%%
%%% @contributors
%%%-------------------------------------------------------------------

-module(webhooks_channel_destroy).

-export([init/0
         ,bindings_and_responders/0
        ]).

-include("../webhooks.hrl").

-define(ID, wh_util:to_binary(?MODULE)).
-define(NAME, <<"channel_destroy">>).
-define(DESC, <<"Events when calls end">>).
-define(METADATA
        ,wh_json:from_list([{<<"_id">>, ?ID}
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
    {[{'call', [{'restrict_to', ['CHANNEL_DESTROY']}
                ,'federate'
               ]
      }
     ]
     ,[{{'webhooks_channel_util', 'handle_event'}
        ,[{<<"call_event">>, <<"CHANNEL_DESTROY">>}]
       }
      ]
    }.
