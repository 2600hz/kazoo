-module(cdr_util_tests).
-include_lib("eunit/include/eunit.hrl").

-spec check_media_names_test() -> 'ok'.
check_media_names_test()->

    A = <<"a9b8c9cf7546827d9ec5ad505770fc86.mp3">>,

    B = <<"202109-d85184f8cfc5592f944bc323617c16ac">>,

    C = [<<"8e9f742001e09ff12abf5ce8bfcb1ff9.mp3">>,<<"3c37d4a18634030662e68523bf74da96.mp3,092b94f757eaceadafbc1ae0dd858057.mp3,51a945565136ed62dce5caf9b90d3630.mp3,e038de9919babfbe28f49fe1d1673405.mp3,9530f5fbdb1efe1f0796506785c6ec7c.mp3">>],

    D = [<<"8e9f742001e09ff12abf5ce8bfcb1ff9.mp3">>,
         <<"3c37d4a18634030662e68523bf74da96.mp3">>,
         <<"092b94f757eaceadafbc1ae0dd858057.mp3">>,
         <<"51a945565136ed62dce5caf9b90d3630.mp3">>,
         <<"e038de9919babfbe28f49fe1d1673405.mp3">>,
         <<"9530f5fbdb1efe1f0796506785c6ec7c.mp3">>],

    E = [<<"202109-f13d49151c46627fb19c3f69b8580c5c">>,<<"202109-2643e9204a52bf529a03a6909d740416,202109-105f6012c16cf1b5af018423ab1dbee8,202109-4cf5e781b82838e750b7ea5fd064e67f,202109-f42c80fd47f98f424520cca147641ecf,202109-103b202b96a2f963f59bc99521af98ff">>],

    F = [<<"202109-f13d49151c46627fb19c3f69b8580c5c">>,
         <<"202109-2643e9204a52bf529a03a6909d740416">>,
         <<"202109-105f6012c16cf1b5af018423ab1dbee8">>,
         <<"202109-4cf5e781b82838e750b7ea5fd064e67f">>,
         <<"202109-f42c80fd47f98f424520cca147641ecf">>,
         <<"202109-103b202b96a2f963f59bc99521af98ff">>],

    ?assertEqual('undefined',cdr_util:check_media_names('undefined')),
    ?assertMatch([A], cdr_util:check_media_names(A)),
    ?assertMatch([B], cdr_util:check_media_names(B)),
    ?assertMatch(D,cdr_util:check_media_names(C)),
    ?assertMatch(F,cdr_util:check_media_names(E)).
