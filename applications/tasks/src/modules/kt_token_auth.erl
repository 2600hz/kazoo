%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kt_token_auth).
%% behaviour: tasks_provider

-export([init/0
        ]).

%% Triggerables
-export([clean_expired/0, clean_expired/1
        ]).

-include("tasks.hrl").

-define(LOOP_TIMEOUT,
        kapps_config:get_integer(<<"crossbar.auth">>, <<"token_auth_expiry_s">>, ?SECONDS_IN_HOUR)).


%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> 'ok'.
init() ->
    _ = tasks_bindings:bind(?TRIGGER_HOURLY, ?MODULE, clean_expired).

%%% Triggerables

-spec clean_expired() -> 'ok'.
clean_expired() ->
    clean_expired(kz_time:now_s() - ?LOOP_TIMEOUT).

-spec clean_expired(gregorian_seconds()) -> 'ok'.
clean_expired(CreatedBefore) ->
    ViewOpts = [{'startkey', 0}
               ,{'endkey', CreatedBefore}
               ,{'limit', kz_datamgr:max_bulk_insert()}
               ],
    case kz_datamgr:get_results(?KZ_TOKEN_DB, <<"token_auth/listing_by_mtime">>, ViewOpts) of
        {'error', _E} -> lager:debug("failed to lookup expired tokens: ~p", [_E]);
        {'ok', []} -> lager:debug("no expired tokens found");
        {'ok', L} ->
            lager:debug("removing ~b expired tokens", [length(L)]),
            kz_datamgr:suppress_change_notice(),
            _ = kz_datamgr:del_docs(?KZ_TOKEN_DB, L),
            kz_datamgr:enable_change_notice(),
            lager:debug("removed tokens")
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%% End of Module.
