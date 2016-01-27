%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz INC
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

-define(ID, wh_util:to_binary(?MODULE)).
-define(NAME, <<"callflow">>).
-define(DESC, <<"Fire a webhook from a callflow">>).
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
    {[{'notifications', [{'restrict_to', ['webhook']}]}]
     ,[{{?MODULE, 'handle_event'}
        ,[{<<"notification">>, <<"webhook">>}]
       }
      ]
    }.

-spec handle_event(wh_json:object(), wh_proplist()) -> 'ok'.
handle_event(JObj, _Props) ->
    'true' = wapi_notifications:webhook_v(JObj),
    Hook = webhooks_util:from_json(wh_json:get_value(<<"Hook">>, JObj)),
    Data = wh_json:get_value(<<"Data">>, JObj),
    webhooks_util:fire_hooks(Data, [Hook]).
