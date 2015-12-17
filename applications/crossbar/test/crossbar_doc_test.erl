%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(crossbar_doc_test).

-include_lib("eunit/include/eunit.hrl").

-define(CREATED, 63599884144).
-define(MODIFIED, 63599885144).

-define(DOC
        ,<<"{\"foo\":\"bar\",\"level1\":{\"level2\":{\"level3\":\"value3\"}},\"pvt_created\":63599884144,\"pvt_modified\":63599885144}">>
       ).

filter_doc_test_() ->
    {'foreach'
     ,fun init/0
     ,fun stop/1
     ,[fun filter_foo/1
       ,fun filter_lvl3/1
       ,fun filter_not_foo/1
       ,fun filter_not_lvl3/1
       ,fun has_keys/1
       ,fun missing_keys/1
       ,fun created/1
       ,fun modified/1
       ,fun multi_filter/1
      ]
    }.

init() ->
    wh_json:decode(?DOC).

stop(_) -> 'ok'.

filter_foo(Doc) ->
    QSTrue = wh_json:decode(<<"{\"filter_foo\":\"bar\"}">>),
    QSFalse = wh_json:decode(<<"{\"filter_foo\":\"baz\"}">>),
    [{"Verify doc has key 'foo' and value 'bar'"
      ,?_assertEqual('true', crossbar_doc:filter_doc_by_querystring(Doc, QSTrue))
     }
     ,{"Verify doc has key 'foo' and not value 'baz'"
       ,?_assertEqual('false', crossbar_doc:filter_doc_by_querystring(Doc, QSFalse))
      }
    ].

filter_not_foo(Doc) ->
    QSTrue = wh_json:decode(<<"{\"filter_not_foo\":\"bar\"}">>),
    QSFalse = wh_json:decode(<<"{\"filter_not_foo\":\"baz\"}">>),
    [{"Verify doc has key 'foo' and value 'bar' fails"
      ,?_assertEqual('false', crossbar_doc:filter_doc_by_querystring(Doc, QSTrue))
     }
     ,{"Verify doc has key 'foo' and value is not 'baz'"
       ,?_assertEqual('true', crossbar_doc:filter_doc_by_querystring(Doc, QSFalse))
      }
    ].

filter_lvl3(Doc) ->
    QSTrue = wh_json:decode(<<"{\"filter_level1.level2.level3\":\"value3\"}">>),
    QSFalse = wh_json:decode(<<"{\"filter_level1.level2.level3\":\"value0\"}">>),
    [{"Verify doc has key 'level1.level2.level3' and value 'value3'"
      ,?_assertEqual('true', crossbar_doc:filter_doc_by_querystring(Doc, QSTrue))
     }
     ,{"Verify doc has key 'level1.level2.level3' and not value 'value0'"
       ,?_assertEqual('false', crossbar_doc:filter_doc_by_querystring(Doc, QSFalse))
      }
    ].

filter_not_lvl3(Doc) ->
    QSTrue = wh_json:decode(<<"{\"filter_not_level1.level2.level3\":\"value3\"}">>),
    QSFalse = wh_json:decode(<<"{\"filter_not_level1.level2.level3\":\"value0\"}">>),
    [{"Verify doc has key 'level1.level2.level3' and value 'value3' fails"
      ,?_assertEqual('false', crossbar_doc:filter_doc_by_querystring(Doc, QSTrue))
     }
     ,{"Verify doc has key 'level1.level2.level3' and value is not 'value0'"
       ,?_assertEqual('true', crossbar_doc:filter_doc_by_querystring(Doc, QSFalse))
      }
    ].

has_keys(Doc) ->
    Filter = <<"has_key">>,
    QSTrue = existing_keys(Filter),
    QSFalse = non_existing_keys(Filter),

    TrueTests = lists:foldl(fun(QS, Acc) ->
                                    has_key_gen(QS, Acc, Doc, Filter, 'true')
                            end
                            ,[]
                            ,QSTrue
                           ),
    lists:foldl(fun(QS, Acc) ->
                        has_key_gen(QS, Acc, Doc, Filter, 'false')
                end
                ,TrueTests
                ,QSFalse
               ).

missing_keys(Doc) ->
    Filter = <<"key_missing">>,
    QSTrue = non_existing_keys(Filter),
    QSFalse = existing_keys(Filter),

    TrueTests = lists:foldl(fun(QS, Acc) ->
                                    has_key_gen(QS, Acc, Doc, Filter, 'true')
                            end
                            ,[]
                            ,QSTrue
                           ),
    lists:foldl(fun(QS, Acc) ->
                        has_key_gen(QS, Acc, Doc, Filter, 'false')
                end
                ,TrueTests
                ,QSFalse
               ).

existing_keys(Filter) ->
    [wh_json:decode(<<"{\"", Filter/binary, "\":\"foo\"}">>)
     ,wh_json:decode(<<"{\"", Filter/binary, "\":\"level1\"}">>)
     ,wh_json:decode(<<"{\"", Filter/binary, "\":\"level1.level2\"}">>)
     ,wh_json:decode(<<"{\"", Filter/binary, "\":\"level1.level2.level3\"}">>)
    ].

non_existing_keys(Filter) ->
    [wh_json:decode(<<"{\"", Filter/binary, "\":\"not1\"}">>)
     ,wh_json:decode(<<"{\"", Filter/binary, "\":\"level1.not2\"}">>)
     ,wh_json:decode(<<"{\"", Filter/binary, "\":\"level1.level2.not3\"}">>)
    ].

has_key_gen(QS, Acc, Doc, Filter, Expected) ->
    Desc = io_lib:format("Verify ~s for ~s is ~s", [Filter, wh_json:get_value(Filter, QS), Expected]),
    [{lists:flatten(Desc)
      ,?_assertEqual(Expected, crossbar_doc:filter_doc_by_querystring(Doc, QS))
     }
     | Acc
    ].

created(Doc) ->
    Filter = <<"created">>,
    Timestamps = {?CREATED-1, ?CREATED, ?CREATED+1},
    to_gen(Doc, Timestamps, Filter) ++
        from_gen(Doc, Timestamps, Filter).

modified(Doc) ->
    Filter = <<"modified">>,
    Timestamps = {?MODIFIED-1, ?MODIFIED, ?MODIFIED+1},
    to_gen(Doc, Timestamps, Filter) ++
        from_gen(Doc, Timestamps, Filter).

to_gen(Doc, {Before, Now, After}, Filter) ->
    [{test_desc("Verify doc is removed when querying ~s_to with timestamp before doc's"
                  ,[Filter]
                 )
      ,?_assertEqual('false', crossbar_doc:filter_doc_by_querystring(Doc, to_qs(Before, Filter)))
     }
     ,{test_desc("Verify doc is kept when querying ~s_to with timestamp equal to doc's"
                 ,[Filter]
                )
       ,?_assertEqual('true', crossbar_doc:filter_doc_by_querystring(Doc, to_qs(Now, Filter)))
      }
     ,{test_desc("Verify doc is kept when querying ~s_to with timestamp after doc's"
                 ,[Filter]
                )
       ,?_assertEqual('true', crossbar_doc:filter_doc_by_querystring(Doc, to_qs(After, Filter)))
      }
    ].

from_gen(Doc, {Before, Now, After}, Filter) ->
    [{test_desc("Verify doc is kept when querying ~s_from with timestamp before doc's"
                ,[Filter]
               )
      ,?_assertEqual('true', crossbar_doc:filter_doc_by_querystring(Doc, from_qs(Before, Filter)))
     }
     ,{test_desc("Verify doc is kept when querying ~s_from with timestamp equal to doc's"
                 ,[Filter]
                )
       ,?_assertEqual('true', crossbar_doc:filter_doc_by_querystring(Doc, from_qs(Now, Filter)))
      }
     ,{test_desc("Verify doc is removed when querying ~s_from with timestamp after doc's"
                 ,[Filter]
                )
       ,?_assertEqual('false', crossbar_doc:filter_doc_by_querystring(Doc, from_qs(After, Filter)))
      }
    ].

to_qs(TStamp, Filter) ->
    wh_json:from_list([{<<Filter/binary, "_to">>, TStamp}]).

from_qs(TStamp, Filter) ->
    wh_json:from_list([{<<Filter/binary, "_from">>, TStamp}]).

multi_filter(Doc) ->
    CFrom = wh_util:to_binary(?CREATED-1),
    CTo = wh_util:to_binary(?CREATED+1),

    QSTrue = wh_json:decode(<<"{\"filter_foo\":\"bar\",\"created_from\":", CFrom/binary, ",\"created_to\":", CTo/binary, "}">>),

    QSFalse = wh_json:decode(<<"{\"filter_foo\":\"bar\",\"created_from\":", CTo/binary, ",\"created_to\":", CFrom/binary, "}">>),

    [{"Verify multple filters are ANDed to keep doc"
      ,?_assertEqual('true', crossbar_doc:filter_doc_by_querystring(Doc, QSTrue))
     }
     ,{"Verify multple filters are ANDed and doc is removed when one fails"
       ,?_assertEqual('false', crossbar_doc:filter_doc_by_querystring(Doc, QSFalse))
      }
    ].

test_desc(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).
