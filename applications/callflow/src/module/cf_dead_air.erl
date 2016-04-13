%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz
%%% @doc
%%%
%%% Answers to the call and switches media off
%%% Could be usefull for external calls/conferences recording over inbound leg
%%%
%%% "data":{}
%%% 
%%% @end
%%% @contributors
%%%   Kirill Sysoev github.com/onnet
%%%-------------------------------------------------------------------
-module(cf_dead_air).

-export([handle/2]).

-include("callflow.hrl").

-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(_Data, Call) ->
    _ = whapps_call_command:b_answer(Call),
    _ = freeswitch:api(wh_util:to_atom(whapps_call:switch_nodename(Call),'true')
                       ,'uuid_media'
                       ,wh_util:to_list(<<"off ", (whapps_call:call_id(Call))/binary>>)
                      ),
    _ = whapps_call_command:wait_for_hangup(),
        cf_exe:stop(Call).
