%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(doodle_app).

-behaviour(application).

-include("doodle.hrl").

-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% @public
%% @doc Implement the application start behaviour
%%--------------------------------------------------------------------
-spec start(application:start_type(), any()) -> startapp_ret().
start(_Type, _Args) ->
    _ = declare_exchanges(),
    case whapps_config:get(?CONFIG_CAT, <<"reschedule">>) of
        'undefined' ->
            case wh_json:load_fixture_from_file(?APP, <<"reschedule.json">>) of
                {'error', Err} ->
                    lager:error("default sms is 'undefined' and cannot read default from file : ~p", [Err]);
                JObj ->
                    whapps_config:set(?CONFIG_CAT, <<"reschedule">>, JObj)
            end;
        _ -> 'ok'
    end,
    doodle_sup:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc Implement the application stop behaviour
%%--------------------------------------------------------------------
-spec stop(any()) -> any().
stop(_State) ->
    'ok'.

-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    _ = wapi_notifications:declare_exchanges(),
    _ = wapi_route:declare_exchanges(),
    _ = wapi_sms:declare_exchanges(),
    _ = wapi_registration:declare_exchanges(),
    wapi_self:declare_exchanges().
