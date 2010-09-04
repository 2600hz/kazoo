-define(CLUECON_DEMO(UUID), [
		       {<<"Application-Name">>, <<"queue">>}
		       ,{<<"Event-Name">>, <<"dialplan">>}
		       ,{<<"Call-ID">>, UUID}
		       ,{<<"Commands">>, [
					  {struct, [{<<"Application-Name">>, <<"answer">>}
						    ,{<<"Call-ID">>, UUID}
						   ]}
					  ,{struct, [{<<"Application-Name">>, <<"play">>}
						     ,{<<"Call-ID">>, UUID}
						     ,{<<"Media-Name">>, <<"voicemail/vm-record_message.wav">>}
						     ,{<<"Terminators">>, ["#"]}
						    ]}
					  ,{struct, [{<<"Application-Name">>, <<"tone">>}
						     ,{<<"Call-ID">>, UUID}
						     ,{<<"Tones">>, [ {struct, [{<<"Frequencies">>, [800]}
										,{<<"Duration-ON">>, 300}
										,{<<"Duration-OFF">>, 0}
									       ]
								      }
								    ]
						      }
						    ]}
						%,{struct, [{<<"Application-Name">>, <<"record">>}
						%	   ,{<<"Call-ID">>, UUID}
						%	   ,{<<"Media-Name">>, list_to_binary(["recording-", UUID, ".wav"])}
						%	  ]}
						%,{struct, [{<<"Application-Name">>, <<"play">>}
						%	   ,{<<"Call-ID">>, UUID}
						%	   ,{<<"Media-Name">>, list_to_binary(["recording-", UUID, ".wav"])}
						%	  ]}
						%,{struct, [{<<"Application-Name">>, <<"store">>}
						%	   ,{<<"Call-ID">>, UUID}
						%	   ,{<<"Media-Name">>, list_to_binary(["recording-", UUID, ".wav"])}
						%	   ,{<<"Media-Transfer-Method">>, <<"put">>}
						%	   ,{<<"Media-Transfer-Destination">>
						%		 ,list_to_binary(["http://localhost:5984/trunkstore/recordings/"
						%				  , "recording-", UUID, ".wav?rev=1-8e9b72c3c8a1be8fbf62c8ca5247a40f"])
						%	    }
						%	   ,{<<"Additional-Headers">>, {struct, [{"Content-Type", "audio/x-wav"}]}}
						%	  ]}
					  ,{struct, [{<<"Application-Name">>, <<"hangup">>}
						     ,{<<"Call-ID">>, UUID}
						    ]}
					 ]
			}
		      ]).

-define(BRIDGE_DEMO, [
		      {<<"Application-Name">>, <<"bridge">>}
		      ,{<<"Event-Name">>, <<"dialplan">>}
		      ,{<<"Call-ID">>, UUID}
		      ,{<<"Endpoints">>, [{struct, [{<<"Endpoint">>, <<"sofia/foo/bar/">>}]}]}
		     ]).
