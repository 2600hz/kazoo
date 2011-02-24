%%%============================================================================
%%% @author Vladimir Darmin <vova@2600hz.org>
%%% @copyright (C) 2011, Vladimir Darmin
%%% @doc
%%% Handles dialplan actions
%%%
%%% @end
%%% Created:       21 Feb 2011 by Vladimir Darmin <vova@2600hz.org>
%%% Last Modified: 23 Feb 2011 by Vladimir Darmin <vova@2600hz.org>
%%%============================================================================
%%%

-module ( cf_dialplan ).

%% API
-export ( [handle/2] ).

-import ( logger, [format_log/3] ).

-include ( "../callflow.hrl" ).

-define(APP_NAME, <<"cf_dialplan">>).
-define(APP_VERSION, <<"0.1">>).

handle ( Data, #cf_call{amqp_h=AHost, call_id=CallId, ctrl_q=CtrlQ, cf_pid=Pid} ) ->
   format_log(info, "CF DIALPLAN (~p): Start handling...~n", [self()]),
   AmqpQ = amqp_util:new_queue(AHost),
   amqp_util:bind_q_to_callevt(AHost, AmqpQ, CallId),
   amqp_util:basic_consume(AHost, AmqpQ),
   format_log(info, "CF DIALPLAN (~p): consuming...~n", [self()]),

   Action = proplists:get_value(<<"action">>, Data),
   Command = [
      {<<"Application-Name">>, Action},
      {<<"Call-ID">>, CallId}
      | whistle_api:default_headers(CallId, <<"call_control">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
   ],
   Request = list_to_existing_atom(binary_to_list(Action)++"_req"),
   {ok, Json} = whistle_api:Request(Command),
   amqp_util:callctl_publish(AHost, CtrlQ, Json, <<"application/json">>),
   wait(<<"CHANNEL_EXECUTE_COMPLETE">>, Action, Pid)
.

wait ( Name, Application, Pid ) ->
   receive
      {_, #amqp_msg{props=Proplist, payload=Payload}} when Proplist#'P_basic'.content_type == <<"application/json">> ->
         {struct, Msg} = mochijson2:decode(binary_to_list(Payload)),
         EC = proplists:get_value(<<"Event-Category">>, Msg),
         EN = proplists:get_value(<<"Event-Name">>, Msg),
         AN = proplists:get_value(<<"Application-Name">>, Msg),
         case {EC, EN, AN} of
            {<<"call_event">>, <<"CHANNEL_HANGUP">>, _} -> format_log(info, "Stop...~n", []), Pid ! { stop };
            {<<"call_event">>, Name, Application}       -> format_log(info, "Continue... ~p~n", [Pid]), Pid ! { continue };
            {A, B, C}                                   -> format_log(info, "Waiting ~p ~p ~p~n", [A, B, C]),wait(Name, Application, Pid)
         end
   end
.

%%%
%%%============================================================================
%%%== END =====
%%%============================================================================
