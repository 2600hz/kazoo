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

   try list_to_existing_atom("cf_"++binary_to_list(Module)) of
      CF_Module ->
         format_log(info, "CF EXECUTIONER (~p): CF Module: ~p~n", [self(), CF_Module]),
         CF_Module:handle(Data, Call#cf_call{cf_pid=self()})
   catch
      _:_ ->
         format_log(error, "CF EXECUTIONER (~p): Module ~p doesn't exist!~n", [self(), Module]),
         self() ! { continue, 1 }
   end,

   receive
      continue        -> self() ! { continue, 1 };
      { continue, N } ->
         format_log(info, "CF EXECUTIONER (~p): Advancing to the next node...~n", [self()]),
         {struct, NewFlow} = case proplists:get_value(<<"children">>, Flow) of
            undefined ->
               format_log(error, "CF EXECUTIONER (~p): Something went horribly wrong...~n", [self()]),
               exit("unexpected end of callflow");
            []        -> { struct, [] };
            Children  -> lists:nth(N, Children)
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
