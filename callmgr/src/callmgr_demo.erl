%%%-------------------------------------------------------------------
%%% @author James Aimonetti <>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%%
%%% @end
%%% Created :  2 Aug 2010 by James Aimonetti <>
%%%-------------------------------------------------------------------
-module(callmgr_demo).

-export([start/1, init/1]).

start(UUID) ->
    spawn(callmgr_demo, init, [UUID]).

init(UUID) ->
    Demo = [
	    %% Play voicemail recording
	    [{cmd, ["sendmsg ", UUID, "\n"
		    ,"call-command: execute\n"
		    ,"execute-app-name: playback\n"
		    ,"execute-app-arg: /usr/local/freeswitch/sounds/en/us/callie/ivr/8000/ivr-save_review_record.wav\n\n"
		   ]}
	     %% Wait on this to continue
	     ,{evt, [{"Event-Name", "CHANNEL_EXECUTE_COMPLETE"}
		     ,{"Application", "playback"}
		     ,{"Application-Response", "FILE PLAYED"}
		    ]}
	    ]
	    %% Play tone
	    ,[{cmd, ["sendmsg ", UUID, "\n"
		     ,"call-command: execute\n"
		     ,"execute-app-name: playback\n"
		     ,"execute-app-arg: tone_stream://%(300,0,800)\n\n"
		    ]}
	      %% Wait on this to continue
	      ,{evt, [{"Event-Name", "CHANNEL_EXECUTE_COMPLETE"}
		      ,{"Application", "playback"}
		      ,{"Application-Response", "FILE PLAYED"}
		     ]}
	     ]
	    %% Set # as terminator
	    ,[{cmd, ["sendmsg ", UUID, "\n"
		     ,"call-command: execute\n"
		     ,"execute-app-name: set\n"
		     ,"execute-app-arg: playback_terminators=#\n\n"
		    ]}
	      %% Wait on this to continue
	      ,{evt, [{"Event-Name", "CHANNEL_EXECUTE_COMPLETE"}
		      ,{"Application", "set"}
		      ,{"Application-Data", "playback_terminators=#"}
		      ,{"Application-Response", "_none_"}
		     ]}
	     ]
	    %% Record data
	    ,[{cmd, ["sendmsg ", UUID, "\n"
		     ,"call-command: execute\n"
		     ,"execute-app-name: record\n"
		     ,"execute-app-arg: /tmp/data-", UUID, ".wav 20 200\n\n"
		    ]}
	      %% Wait on this to continue
	      ,{evt, [{"Event-Name", "CHANNEL_EXECUTE_COMPLETE"}
		      ,{"Application", "record"}
		      ,{"Application-Response", "_none_"}
		     ]}
	     ]
	    %% Playback recorded message
	    ,[{cmd, ["sendmsg ", UUID, "\n"
		     ,"call-command: execute\n"
		     ,"execute-app-name: playback\n"
		     ,"execute-app-arg: /tmp/data-", UUID, ".wav\n\n"
		    ]}
	      %% Wait on this to continue
	      ,{evt, [{"Event-Name", "CHANNEL_EXECUTE_COMPLETE"}
		      ,{"Application", "playback"}
		      ,{"Application-Response", "FILE PLAYED"}
		     ]}
	     ]
	    %% Terminate the call
	    ,[{cmd, ["sendmsg ", UUID, "\n"
		     ,"call-command: execute\n"
		     ,"execute-app-name: hangup\n"
		     ,"execute-app-arg: \n\n"
		    ]}
	     ]
	   ],
    loop(UUID, hd(Demo), tl(Demo)).

loop(UUID, FinalStep, []) ->
    Cmd = lists:concat(proplists:get_value(cmd, FinalStep)),
    callmgr_logger:format_log(info, "DEMO(~p): loop final for UUID ~p~nCmd ~p~nEvt: ~p~n", [self(), UUID, Cmd]),
    callmgr_fsdemo:exec_cmd(UUID, Cmd);
loop(UUID, Step, Steps) ->
    Cmd = lists:concat(proplists:get_value(cmd, Step)),
    Evt = proplists:get_value(evt, Step),
    callmgr_logger:format_log(info, "DEMO(~p): loop entered for UUID ~p~nCmd ~p~nEvt: ~p~n", [self(), UUID, Cmd, Evt]),
    callmgr_fsdemo:exec_cmd(UUID, Cmd),
    wait(UUID, Evt, Steps).

wait(UUID, Evt, Steps) ->
    receive
	{event_exec_completed, Evt1} ->
	    callmgr_logger:format_log(info, "DEMO(~p): RecvEvtExecCompleted~n", [self()]),
	    case lists:all(fun({K,V}) -> 
				   proplists:get_value(K, Evt1) == V
			   end, Evt) of
		true ->
		    loop(UUID, hd(Steps), tl(Steps));
		false ->
		    wait(UUID, Evt, Steps)
	    end;
	{event_destroy, UUID} ->
	    callmgr_logger:format_log(info, "DEMO(~p): Closing down~n", [self()]),
	    ok;
	_Evt ->
	    callmgr_logger:format_log(info, "DEMO(~p): Unknown Event: ~p~n", [self(), _Evt]),
	    wait(UUID, Evt, Steps)
    end.
