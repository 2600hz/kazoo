%%%============================================================================
%%% @author Vladimir Darmin <vova@2600hz.org>
%%% @copyright (C) 2011, Vladimir Darmin
%%% @doc
%%% Callflow executioner, executes the head node and waits for further
%%% instructions
%%%
%%% @end
%%% Created :  21 Feb 2011 by Vladimir Darmin <vova@2600hz.org>
%%%============================================================================
%%%

-module ( cf_exe ).

%% API
-export ( [start/2] ).

-import ( logger, [format_log/3] ).

-include ( "callflow.hrl" ).

start ( Call, Flow ) ->
   Module = proplists:get_value(<<"module">>, Flow),
   {struct, Data} = proplists:get_value(<<"data">>, Flow),
   format_log(info, "CF EXECUTIONER (~p): Executing ~p...~n", [self(), Module]),

%TODO: if something goes wrong move to the next node
   CF_Module = list_to_existing_atom("cf_"++binary_to_list(Module)),
   format_log(info, "CF Module: ~p~n", [CF_Module]),
   CF_Module:handle(Module, Data, Call#cf_call{cf_pid=self()}) end),

   receive
      continue        -> self() ! { continue, 0 };
      { continue, N } ->
         format_log(info, "CF EXECUTIONER (~p): Advancing to the next node...~n", [self()]),
         {struct, NewFlow} = case proplists:get_value(<<"children">>, Flow) of
            []       -> { struct, [] };
            Children -> lists:nth(N, Children)
         end,
         case NewFlow of
            [] ->
               spawn(fun() -> start (Call, [{<<"module">>, <<"hangup">>}, {<<"data">>, {struct, []}}]) end),
               format_log(info, "CF EXECUTIONER (~p): Child node doesn't exist, hanging up...~n", [self()]),
               exit("end of the flow, hanging up");
            _  ->
               spawn(fun() -> start (Call, NewFlow) end),
               exit("moving on")
         end;
      stop            ->
         format_log(info, "CF EXECUTIONER (~p): Callflow execution has been stopped~n", [self()]),
         exit("Requested stop")
   end
.

%%%
%%%============================================================================
%%%== END =====
%%%============================================================================
