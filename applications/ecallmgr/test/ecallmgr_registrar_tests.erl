%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Listener for reg_success, and reg_query AMQP requests
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_registrar_tests).

-include_lib("eunit/include/eunit.hrl").

-define(CONTACTS
       ,[{<<"sip:user_0tbp@1.1.1.1:55274;fs_path=sip:2.2.2.2:5060;lr;received='sip:1.1.1.1:55274;transport=udp'">>
         ,[{'transport', <<"udp">>}
          ,{'received', <<"1.1.1.1:55274">>}
          ,{'fs_path', <<"2.2.2.2:5060">>}
          ,{'uri', <<"user_0tbp@1.1.1.1:55274">>}
          ,{'hostport', <<"1.1.1.1:55274">>}
          ]
         }
        ,{<<"<sip:5519@2.2.2.2:55061;transport=TLS;ob;fs_path=<sip:1.1.1.1:5061;lr;received='sip:1.1.1.1:55061;transport=tls'>>">>
         ,[{'transport', <<"tls">>}
          ,{'received', <<"1.1.1.1:55061">>}
          ,{'fs_path', <<"1.1.1.1:5061">>}
          ,{'uri', <<"5519@2.2.2.2:55061">>}
          ,{'hostport', <<"2.2.2.2:55061">>}
          ]
         }
        ,{<<"<sip:User_2pnrza@3.3.3.3:64967;ob;fs_path=<sip:1.1.1.1:5060;lr;received='sip:3.3.3.3:64967;transport=udp'>>">>
         ,[{'transport', <<"udp">>}
          ,{'received', <<"3.3.3.3:64967">>}
          ,{'fs_path', <<"1.1.1.1:5060">>}
          ,{'uri', <<"User_2pnrza@3.3.3.3:64967">>}
          ,{'hostport', <<"3.3.3.3:64967">>}
          ]
         }
        ,{<<"<sip:5520@78.25.120.237:26070;transport=TLS;rinstance=e6b1e5bda0fcfd30;fs_path=<sip:94.125.5.31:5061;lr;received='sip:78.25.120.237:26070;transport=tls'>>">>
         ,[{'transport', <<"tls">>}
          ,{'received', <<"78.25.120.237:26070">>}
          ,{'fs_path', <<"94.125.5.31:5061">>}
          ,{'uri', <<"5520@78.25.120.237:26070">>}
          ,{'hostport', <<"78.25.120.237:26070">>}
          ]
         }
        ]).

registrar_summary_test_() ->
    lists:flatmap(fun contact_props/1, ?CONTACTS).

contact_props({Contact, Ps}) ->
    Props = ecallmgr_registrar:breakup_contact(Contact),
    [?_assertEqual(V, props:get_value(K, Props))
     || {K, V} <- Ps
    ].
