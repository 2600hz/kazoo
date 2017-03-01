%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(teletype_bindings).

-export([start_link/0]).
-export([bind/3, bind/4
        ,flush_mod/1
        ,notification/1
        ]).

-include("teletype.hrl").

-define(SERVER, ?MODULE).
-define(ROUTING_KEY(Category, Name), <<"teletype.", Category/binary, ".", Name/binary>>).

-spec start_link() -> startlink_ret().
start_link() ->
    start_modules(),
    'ignore'.

-spec bind(ne_binary(), module(), atom()) -> 'ok'.
-spec bind(api_binary(), api_binary(), module(), atom()) -> 'ok'.
bind(EventName, Module, Fun) ->
    bind(<<"notification">>, EventName, Module, Fun).
bind(EventCategory, EventName, Module, Fun) ->
    kazoo_bindings:bind(?ROUTING_KEY(EventCategory, EventName), Module, Fun).

-spec flush_mod(module()) -> 'ok'.
flush_mod(Module) ->
    kazoo_bindings:flush_mod(Module).

-spec notification(kz_json:object()) -> 'ok'.
notification(JObj) ->
    {EventCategory, EventName} = kz_util:get_event_type(JObj),
    RoutingKey = ?ROUTING_KEY(EventCategory, EventName),
    lager:debug("dispatching notification ~s", [RoutingKey]),
    _ = kazoo_bindings:map(RoutingKey, JObj, []),
    'ok'.

-spec start_modules() -> 'ok'.
start_modules() ->
    start_modules(?AUTOLOAD_MODULES).

-spec start_modules([module()]) -> 'ok'.
start_modules([Module | Remaining]) ->
    teletype_maintenance:start_module(Module),
    start_modules(Remaining);
start_modules([]) ->
    lager:info("started all teletype modules").
