%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kt_cb_vmboxes).
%% behaviour: tasks_provider

-export([init/0
        ]).

-include("tasks.hrl").


%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> 'ok'.
init() ->
    _ = tasks_bindings:bind(?TRIGGER_ACCOUNT, 'kvm_maintenance', 'cleanup_heard_voicemail').

%%% End of Module.
