%%%-------------------------------------------------------------------
%%% @copyright (C) 2015-2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%
%%%-------------------------------------------------------------------
-module(knm_options).

-export([should_update_services/1, should_update_services/2
        ,default/0

        ]).


-include("knm.hrl").

-type option() :: {'should_update_services', boolean()}.

-type options() :: [option()].

-export_type([option/0, options/0
             ]).

-spec default() -> options().
default() ->
    [{'should_update_services', 'true'}
    ].

-spec should_update_services(options()) -> boolean().
-spec should_update_services(options(), Default) -> boolean() | Default.
should_update_services(Options) ->
    should_update_services(Options, 'true').
should_update_services(Options, Default) ->
    props:is_true('should_update_services', Options, Default).
