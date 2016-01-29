-module(kz_html_tests).

-include_lib("eunit/include/eunit.hrl").

escape_test() ->
    ?assertEqual(<<"&amp;quot;\"word &gt;&lt;&lt;up!&amp;quot;">>, kz_html:escape(<<"&quot;\"word ><<up!&quot;">>)),
    ?assertEqual(<<"12345">>, kz_html:escape(<<"12345">>)),
    ?assertEqual(<<"1.5">>, kz_html:escape(<<"1.5">>)).
