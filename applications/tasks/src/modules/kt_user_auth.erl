%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kt_user_auth).
%% behaviour: tasks_provider

-export([init/0
        ]).

%% Triggerables
-export([cleanup_reset_ids/1
        ]).

-include("tasks.hrl").

-define(RESET_PVT_TYPE, <<"password_reset">>).


%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> 'ok'.
init() ->
    _ = tasks_bindings:bind(?TRIGGER_ACCOUNT, ?MODULE, 'cleanup_reset_ids').

%%% Triggerables

%% @public
-spec cleanup_reset_ids(ne_binary()) -> 'ok'.
cleanup_reset_ids(AccountDb) ->
    ViewOptions = [{'key', ?RESET_PVT_TYPE}
                  ,'include_docs'
                  ],
    case kz_datamgr:get_results(AccountDb, <<"maintenance/listing_by_type">>, ViewOptions) of
        {'ok', [_|_]=ResetIdDocs} ->
            lager:debug("checking ~p reset_id documents"),
            DelDocs = fun (ResetIdDoc) -> maybe_delete_doc(AccountDb, ResetIdDoc) end,
            lists:foreach(DelDocs, ResetIdDocs);
        _Else -> 'ok'
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec maybe_delete_doc(ne_binary(), kz_json:object()) -> 'ok'.
maybe_delete_doc(AccountDb, ResetIdDoc) ->
    TwoDaysAgo = kz_time:now_s() - 2 * ?SECONDS_IN_DAY,
    Created = kz_doc:created(ResetIdDoc),
    case TwoDaysAgo < Created of
        'true' -> 'ok';
        'false' ->
            _ = kz_datamgr:del_doc(AccountDb, kz_doc:id(ResetIdDoc)),
            'ok'
    end.

%%% End of Module.
