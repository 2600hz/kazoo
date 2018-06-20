%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(fax_init).

-include("fax.hrl").

-export([start_link/0]).

%%------------------------------------------------------------------------------
%% @doc Starts the application for inclusion in a supervisor tree.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    _ = declare_exchanges(),
    Dispatch = cowboy_router:compile([
                                      %% :: {HostMatch, [{PathMatch, Constraints, Handler, Opts}]}
                                      {'_', [{<<"/fax/[...]">>, [], 'fax_file_proxy', []}]}
                                     ]),

    Workers = kapps_config:get_integer(?CONFIG_CAT, <<"workers">>, 50),
    %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
    cowboy:start_clear('fax_file'
                      ,[{'port', ?PORT}
                       ,{'num_acceptors', Workers}
                       ]
                      ,#{'env' => #{'dispatch' => Dispatch}}
                      ),
    fax_maintenance:migrate_views(),
    'ignore'.

%%------------------------------------------------------------------------------
%% @doc Ensures that all exchanges used are declared.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    _ = kapi_fax:declare_exchanges(),
    _ = kapi_xmpp:declare_exchanges(),
    _ = kapi_conf:declare_exchanges(),
    _ = kapi_notifications:declare_exchanges(),
    _ = kapi_offnet_resource:declare_exchanges(),
    _ = kapi_call:declare_exchanges(),
    _ = kapi_dialplan:declare_exchanges(),
    kapi_self:declare_exchanges().
