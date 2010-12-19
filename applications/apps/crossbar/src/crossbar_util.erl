%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% 
%%% @end
%%% Created : 14 Dec 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(crossbar_util).

-export([get_request_params/1]).
-export([winkstart_envelope/1]).
-export([winkstart_envelope/2, winkstart_envelope/3, winkstart_envelope/4]).

-include("crossbar.hrl").

-import(logger, [format_log/3]).

get_request_params(RD) ->
    case wrq:method(RD) of
	'GET' -> wrq:req_qs(RD);
	_ -> pull_from_body_and_qs(RD)
    end.

%% Favor body paramaters when key exists in both body and qs
pull_from_body_and_qs(RD) ->
    ReqBody = wrq:req_body(RD),
    PostBody = try
		   {struct, Prop} = mochijson2:decode(ReqBody),
		   Prop
	       catch
		   _:_ -> mochiweb_util:parse_qs(ReqBody)
	       end,
    QS = wrq:req_qs(RD),
    lists:ukeymerge(1, lists:ukeysort(1, PostBody), lists:ukeysort(1, QS)).

-spec(winkstart_envelope/1 :: (ApiResult :: crossbar_module_result()) -> iolist()).
winkstart_envelope({Status, Data}) -> winkstart_envelope(Status, Data);    
winkstart_envelope({Status, Data, Msg}) -> winkstart_envelope(Status, Data, Msg);
winkstart_envelope({Status, Data, Msg, Code}) -> winkstart_envelope(Status, Data, Msg, Code).

-spec(winkstart_envelope/2 :: (Status :: crossbar_status(), Data :: proplist()) -> iolist()).
winkstart_envelope(success, Data) ->
    format_log(info, "Envelope: D: ~p~n", [Data]),
    mochijson2:encode({struct, [{status, <<"success">>}
				,{data, {struct, Data}}
			       ]});
winkstart_envelope(error, Data) ->
    winkstart_envelope(error, Data, "An unspecified error has occurred");
winkstart_envelope(fatal, Data) ->
    winkstart_envelope(error, Data, "An unspecified fatal error has occurred").

-spec(winkstart_envelope/3 :: (Status :: crossbar_status(), Data :: proplist(), Msg :: string()) -> iolist()).
winkstart_envelope(success, Data, Msg) ->
    mochijson2:encode({struct, [{status, <<"success">>}
				,{message, whistle_util:to_binary(Msg)}
				,{data, {struct, Data}}
			       ]});
winkstart_envelope(error, Data, Msg) ->
    mochijson2:encode({struct, [{status, <<"error">>}
				,{message, whistle_util:to_binary(Msg)}
				,{data, {struct, Data}}
			       ]});
winkstart_envelope(fatal, Data, Msg) ->
    mochijson2:encode({struct, [{status, <<"fatal">>}
				,{message, whistle_util:to_binary(Msg)}
				,{data, {struct, Data}}
			       ]}).

-spec(winkstart_envelope/4 :: (Status :: crossbar_status(), Data :: proplist(), ErrorMsg :: string(), ErrorCode :: integer()) -> iolist()).
winkstart_envelope(success, Data, Msg, _) ->
    winkstart_envelope(success, Data, Msg);
winkstart_envelope(error, Data, ErrorMsg, ErrorCode) ->
    mochijson2:encode({struct, [{status, <<"error">>}
				,{error, ErrorCode}
				,{message, whistle_util:to_binary(ErrorMsg)}
				,{data, {struct, Data}}
			       ]});
winkstart_envelope(fatal, Data, ErrorMsg, ErrorCode) ->
    mochijson2:encode({struct, [{status, <<"fatal">>}
				,{error, ErrorCode}
				,{message, whistle_util:to_binary(ErrorMsg)}
				,{data, {struct, Data}}
			       ]}).
