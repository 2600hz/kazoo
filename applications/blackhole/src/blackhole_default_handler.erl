%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(blackhole_default_handler).
-include("blackhole.hrl").

-export([
         init/3,
         websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3
        ]).

init({_Any, 'http'}, _Req0, _HandlerOpts) ->
    {'upgrade', 'protocol', 'cowboy_websocket'}.

websocket_init(_Type, Req, _Opts) ->
    try
        {Peer, _}  = cowboy_req:peer(Req),
        {RemIp, _} = Peer,
        {'ok', Context} = blackhole_socket_callback:open(RemIp),
        blackhole_tracking:add_socket(Context),
        {'ok', Req, Context}
    catch
        _:Error ->
            lager:error("error init websocket:~p", [Error]),
            {'shutdown', Req}
    end.

websocket_handle({'text', Data}, Req, Context) ->
    try
        JObj = kz_json:decode(Data),
        NewContext = #bh_context{} = blackhole_socket_callback:recv(JObj, Context),
        blackhole_tracking:update_socket(Context),
        {'ok', Req, NewContext}
    catch
        _:Error ->
            {reply, {'text', error_message(Error)}, Req, Context}
    end.

websocket_info({'$gen_cast', _}, Req, Context) ->
    {'ok', Req, Context};

websocket_info({'send_event', Event, Data}, Req, Context) ->
    Msg = kz_json:set_value(<<"routing_key">>, Event, Data),
    {'reply', {'text', kz_json:encode(Msg)}, Req, Context};

websocket_info({'send_message', Msg}, Req, Context) ->
    {'reply', {'text', kz_json:encode(Msg)}, Req, Context};

websocket_info(Info, Req, Context) ->
    lager:info("unhandled websocket info: ~p", [Info]),
    {'ok', Req, Context}.

websocket_terminate(_Reason, _Req, Context) ->
    blackhole_tracking:remove_socket(Context),
    blackhole_socket_callback:close(Context).

-spec error_message(binary() | atom()) -> binary().
error_message(Error) when is_atom(Error) ->
    make_error_message(erlang:atom_to_binary(Error, utf8));
error_message(Error) when is_binary(Error) ->
    make_error_message(Error);
error_message({'badmatch', L})  ->
    Term = erlang:list_to_binary(io_lib:format("~p", [L])),
    make_error_message(<<"unmatched_parameter: ", Term/binary>>);
error_message(Error) ->
    lager:error("unhandled error:~p", [Error]),
    make_error_message(<<"internal">>).

-spec make_error_message(ne_binary()) -> kz_json:object().
make_error_message(<<"not_authenticated">> = Error) ->
    kz_json:encode(kz_json:from_list([{<<"error">>, Error}]));
make_error_message(Error) ->
    lager:error("blackhole error:~p", [Error]),
    kz_util:log_stacktrace(),
    kz_json:encode(kz_json:from_list([{<<"error">>, Error}])).
