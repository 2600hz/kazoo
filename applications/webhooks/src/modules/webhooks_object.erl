%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%%
%%% @contributors
%%%-------------------------------------------------------------------

-module(webhooks_object).

-export([init/0
         ,bindings_and_responders/0
         ,handle_event/2
        ]).

-include("../webhooks.hrl").

-define(ID, wh_util:to_binary(?MODULE)).
-define(NAME, <<"skel">>).
-define(DESC, <<"Example webhook module">>).

-define(TYPE_MODIFIER
        ,wh_json:from_list([{<<"type">>, <<"array">>}
                            ,{<<"description">>, <<"A list of object types to handle">>}
                            ,{<<"items">>, wh_json:from_list([{<<"type">>, <<"string">>}])}
                           ])
       ).

-define(MODIFIERS
        ,wh_json:from_list([{<<"types">>, ?TYPE_MODIFIER}])
       ).

-define(METADATA
        ,wh_json:from_list([{<<"_id">>, ?ID}
                            ,{<<"name">>, ?NAME}
                            ,{<<"description">>, ?DESC}
                            ,{<<"modifiers">>, ?MODIFIERS}
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
    {[{'self', []}]
     ,[{{?MODULE, 'handle_event'}
        ,[{<<"category">>, <<"name">>}]
       }
      ]
    }.

-spec handle_event(wh_json:object(), wh_proplist()) -> any().
handle_event(JObj, _Props) ->
    wh_util:put_callid(JObj),
    lager:debug("event handled").
