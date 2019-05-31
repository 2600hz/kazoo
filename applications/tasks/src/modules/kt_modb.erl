%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @author James Aimonetti
%%% @author Pierre Fenoll
%%% @end
%%%-----------------------------------------------------------------------------
-module(kt_modb).

%% behaviour: tasks_provider

-export([init/0]).

%% Triggerables
-export([clean_modb/1]).

-include("tasks.hrl").

-define(SHOULD_ARCHIVE_MODBS
       ,kapps_config:get_is_true(?CONFIG_CAT, <<"should_archive_modbs">>, 'false')
       ).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    case ?SHOULD_ARCHIVE_MODBS of
        'false' -> 'ok';
        'true' -> tasks_bindings:bind(?TRIGGER_ACCOUNT_MOD, ?MODULE, 'clean_modb')
    end.

%%% Triggerables

-spec clean_modb(kz_term:ne_binary()) -> 'ok'.
clean_modb(AccountMODb) ->
    kazoo_modb:maybe_archive_modb(AccountMODb).
