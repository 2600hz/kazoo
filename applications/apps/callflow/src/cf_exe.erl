%%%============================================================================
%%% @author Vladimir Darmin <vova@2600hz.org>
%%% @copyright (C) 2011, Vladimir Darmin
%%% @doc
%%% Callflow executioner, executes the head node and waits for further
%%% instructions
%%%
%%% @end
%%% Created:       21 Feb 2011 by Vladimir Darmin <vova@2600hz.org>
%%% Last Modified: 23 Feb 2011 by Vladimir Darmin <vova@2600hz.org>
%%%============================================================================
%%%

-module ( cf_exe ).

%% API
-export ( [start/2] ).

-import ( logger, [format_log/3] ).

-include ( "callflow.hrl" ).

start ( Call, Flow ) ->
   NewCall = Call#cf_call{cf_pid=self()},
   Module = proplists:get_value(<<"module">>, Flow),
   {struct, Data} = proplists:get_value(<<"data">>, Flow),
   format_log(info, "CF EXECUTIONER (~p): Executing ~p...~n", [self(), Module]),

   try list_to_existing_atom("cf_"++binary_to_list(Module)) of
      CF_Module ->
%TODO: monitor the spawned process and move on if it dies...
         spawn(fun () -> CF_Module:handle(Data, NewCall) end)
   catch
      _:_ ->
         format_log(error, "CF EXECUTIONER (~p): Module ~p doesn't exist!~n", [self(), Module]),
         self() ! { continue, 1 }
   end,
   wait( Call, Flow )
.

wait ( Call, Flow ) ->
   receive
      { continue }    -> self() ! { continue, 1 }, wait(Call, Flow);
      { continue, N } ->
         format_log(info, "CF EXECUTIONER (~p): Advancing to the next node...~n", [self()]),
         {struct, NewFlow} = case proplists:get_value(<<"children">>, Flow) of
            undefined ->
               format_log(error, "CF EXECUTIONER (~p): Unexpected end of callflow...~n", [self()]),
               exit("Bad things happened...");
            []        -> { struct, [] };
            Children  -> lists:nth(N, Children)
         end,
         case NewFlow of
            [] ->
               start (Call, [{<<"module">>, <<"dialplan">>}, {<<"data">>, {struct, [{<<"action">>, <<"hangup">>}, {<<"data">>, {struct, []}}]}}]),
               format_log(info, "CF EXECUTIONER (~p): Child node doesn't exist, hanging up...~n", [self()]);
            _  -> start (Call, NewFlow)
         end;
      { stop }        ->
         format_log(info, "CF EXECUTIONER (~p): Callflow execution has been stopped~n", [self()]),
         exit("End of execution");
      { heartbeat }   ->
         format_log(info, "CF EXECUTIONER (~p): Call is in progress...~n", [self()]),
         wait ( Call, Flow )
   after
      5000 -> wait(Call, Flow) %TODO: kill the spawned process and move to the next node
   end
.

%%%
%%%============================================================================
%%%== END =====
%%%============================================================================
