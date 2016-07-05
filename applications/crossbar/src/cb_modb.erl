%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%% Token auth module
%%%
%%% This is a simple auth mechanism, once the user has aquired an
%%% auth token this module will allow access.  This module should be
%%% updated to be FAR more robust.
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_modb).

-export([init/0
	,clean_modb/1
        ]).

-include("crossbar.hrl").

-spec init() -> 'ok'.
init() ->
    case kapps_config:get_is_true(?MODULE, <<"maybe_archive_modbs">>, 'false') of
        'true' ->
            crossbar_bindings:bind(crossbar_cleanup:binding_account_mod(), ?MODULE, 'clean_modb');
        'false' ->
            'ok'
    end.

-spec clean_modb(ne_binary()) -> 'ok'.
clean_modb(AccountMODb) ->
    kazoo_modb:maybe_archive_modb(AccountMODb).
