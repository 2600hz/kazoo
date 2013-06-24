#ejson

decode and encode JSON into/from Erlang terms using Elang NIF library
if available..  This the module used in CouchDB project ported a
standalone module with rebar support. 

##Build

    $ make

##Testing
    
    $ make check

All tests should pass

##  Usage

Put this app in your Erlang path.

    $ erl -pa ebin/
    Erlang R13B04 (erts-5.7.5) [source] [64-bit] [smp:2:2] [rq:2] [async-threads:0] [hipe] [kernel-poll:false]

    Eshell V5.7.5  (abort with ^G)
    1> ejson:decode(<<"{\"foo\": true}">>).
    {[{<<"foo">>,true}]}
    2> ejson:encode([true, 1.2, null]).
    <<"[true,1.2,null]">>

