%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_module_tests).

-include_lib("eunit/include/eunit.hrl").

-define(AN_ACCOUNT_ID, <<"4fe69c5b61015084f1fe5684abc6e502">>).

ensure_loaded_test_() ->
    [?_assertEqual('false', kz_module:ensure_loaded('undefined'))
    ,?_assertEqual('false', kz_module:ensure_loaded("undefined"))
    ,?_assertEqual('false', kz_module:ensure_loaded(<<"undefined">>))
    ,?_assertEqual('kz_term', kz_module:ensure_loaded("kz_term"))
    ,?_assertEqual('kz_term', kz_module:ensure_loaded(<<"kz_term">>))
    ,?_assertEqual('kz_term', kz_module:ensure_loaded('kz_term'))
    ,?_assertEqual('false', kz_module:ensure_loaded(kz_term:to_list(?AN_ACCOUNT_ID)))
    ,?_assertEqual('false', kz_module:ensure_loaded(?AN_ACCOUNT_ID))
    ,?_assertEqual('false', kz_module:ensure_loaded(kz_term:to_atom(?AN_ACCOUNT_ID,'true')))
    ].
