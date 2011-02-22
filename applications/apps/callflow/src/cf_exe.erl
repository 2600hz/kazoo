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

-behaviour ( gen_server ).

%% API
-export ( [start/2] ).

-import ( logger, [format_log/3] ).

-include ( "callflow.hrl" ).
%-include ( "../../../utils/src/whistle_amqp.hrl" ).
%-include ( "../include/amqp_client/include/amqp_client.hrl" ).

start ( Call, Flow ) ->
   
.

%%%
%%%============================================================================
%%%== END =====
%%%============================================================================
