%%%============================================================================
%%% @author Vladimir Darmin <vova@2600hz.org>
%%% @copyright (C) 2011, Vladimir Darmin
%%% @doc
%%% Callflow call handler, waits for winning routes to spawn callflow processes
%%%
%%% @end
%%% Created :  3 Feb 2011 by Vladimir Darmin <vova@2600hz.org>
%%%============================================================================
%%%

-module ( cf_exe ).

%% API
-export ( [start/2] ).

-import ( logger, [format_log/3] ).

-include ( "callflow.hrl" ).
%-include ( "../../../utils/src/whistle_amqp.hrl" ).
%-include ( "../include/amqp_client/include/amqp_client.hrl" ).

start ( Call, Flow ) ->
   Module = proplists:get_value(<<"module">>, Flow),
   {struct, Data} = proplists:get_value(<<"data">>, Flow),
   %calling handling module here...
   %cf_Module:handle(Module, Data, Call#cf_call{pid=self()}),
   receive
      continue        -> self() ! { continue, 0 };
      { continue, N } ->
         {struct, NewFlow} = case proplists:get_value(<<"children">>, Flow) of
            []       -> { struct, [] };
            Children -> lists:nth(N, Children)
         end,
         case NewFlow of
            [] ->
               spawn(fun() -> start (Call, [{<<"module">>, <<"hangup">>}, {<<"data">>, {struct, []}}]) end),
               exit("end of the flow, hanging up");
            _  ->
               spawn(fun() -> start (Call, NewFlow) end),
               exit("moving on")
         end;
      stop            -> exit("Requested stop")
   end
.

%%%
%%%============================================================================
%%%== END =====
%%%============================================================================
