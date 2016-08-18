%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz INC
%%%
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(webhooks_callflow).

-export([init/0
        ,bindings_and_responders/0
        ,handle_event/2
        ]).

-include("webhooks.hrl").

-define(ID, kz_util:to_binary(?MODULE)).
-define(NAME, <<"callflow">>).
-define(DESC, <<"Fire a webhook from a callflow">>).
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
    {[{'notifications', [{'restrict_to', ['webhook']}]}]
    ,[{{?MODULE, 'handle_event'}
      ,[{<<"notification">>, <<"webhook">>}]
      }
     ]
    }.

-spec handle_event(kz_json:object(), kz_proplist()) -> 'ok'.
handle_event(JObj, _Props) ->
    'true' = kapi_notifications:webhook_v(JObj),
    Hook = webhooks_util:from_json(kz_json:get_value(<<"Hook">>, JObj)),
    Data = kz_json:get_value(<<"Data">>, JObj),
    webhooks_util:fire_hooks(Data, [Hook]).
