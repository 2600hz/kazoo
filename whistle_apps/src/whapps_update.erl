%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 13 Jan 2012 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(whapps_update).
-include_lib("whistle/include/wh_databases.hrl").


-export([run/0]).


run() ->
    couch_mgr:db_delete(<<"crossbar_schemas">>),
    crossbar_maintenance:blocking_refresh(),
    callflow_maintenance:blocking_refresh(),
    whistle_number_manager_maintenance:reconcile(all),
    whapps_config:flush(),
    XbarUpdates = [fun(L) -> lists:delete(<<"cb_cdr">>, L) end
                   ,fun(L) -> lists:delete(<<"cb_signups">>, L) end
                   ,fun(L) -> lists:delete(<<"cb_resources">>, L) end
                   ,fun(L) -> [<<"cb_phone_numbers">> | lists:delete(<<"cb_phone_numbers">>, L)] end
                   ,fun(L) -> [<<"cb_templates">> | lists:delete(<<"cb_templates">>, L)] end
                   ,fun(L) -> [<<"cb_onboard">> | lists:delete(<<"cb_onboard">>, L)] end
                  ],
    StartModules = whapps_config:get(<<"crossbar">>, <<"autoload_modules">>, []),
    _ = whapps_config:set_default(<<"crossbar">>
                                      ,<<"autoload_modules">>
                                      ,lists:foldr(fun(F, L) -> F(L) end, StartModules, XbarUpdates)
                                 ),
    _ = whapps_controller:stop_app(crossbar),
    whapps_controller:start_app(crossbar).
