%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_html_tests).

-include_lib("eunit/include/eunit.hrl").

escape_test_() ->
    [?_assertEqual(<<"&amp;quot;\"word &gt;&lt;&lt;up!&amp;quot;">>, kz_html:escape(<<"&quot;\"word ><<up!&quot;">>))
    ,?_assertEqual(<<"12345">>, kz_html:escape(<<"12345">>))
    ,?_assertEqual(<<"1.5">>, kz_html:escape(<<"1.5">>))
    ].
