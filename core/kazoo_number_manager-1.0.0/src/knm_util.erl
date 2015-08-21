%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%%
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(knm_util).

-export([get_all_number_dbs/0]).

-include("knm.hrl").

-spec get_all_number_dbs() -> ne_binaries().
get_all_number_dbs() ->
    {'ok', Dbs} = couch_mgr:admin_all_docs(<<"dbs">>
                                               ,[{'startkey', ?KNM_DB_PREFIX}
                                                 ,{'endkey', <<?KNM_DB_PREFIX_L, "\ufff0">>}
                                                ]),
    [cow_qs:urlencode(wh_doc:id(View))
     || View <- Dbs
    ].
