% Copyright (c) 2010-2011 by Travelping GmbH <info@travelping.com>

% Permission is hereby granted, free of charge, to any person obtaining a
% copy of this software and associated documentation files (the "Software"),
% to deal in the Software without restriction, including without limitation
% the rights to use, copy, modify, merge, publish, distribute, sublicense,
% and/or sell copies of the Software, and to permit persons to whom the
% Software is furnished to do so, subject to the following conditions:

% The above copyright notice and this permission notice shall be included in
% all copies or substantial portions of the Software.

% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
% DEALINGS IN THE SOFTWARE.

% @private
-module(eradius_log).

-behaviour(gen_server).

%% API
-export([start_link/0, write_request/2,
         collect_meta/1, collect_message/2]).
-export([bin_to_hexstr/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("eradius_lib.hrl").
-include("eradius_dict.hrl").

-type sender() :: {inet:ip_address(), eradius_server:port_number(), eradius_server:req_id()}.

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link() -> {ok, pid()} | {error, Reason :: term}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec write_request(sender(), #radius_request{}) -> ok.
write_request(Sender, Request = #radius_request{}) ->
    case application:get_env(eradius, logging) of
        {ok, true} ->
            Time = calendar:universal_time(),
            gen_server:cast(?SERVER, {write_request, Time, Sender, Request});
        _ ->
            ok
    end.

-spec collect_meta(#radius_request{}) -> [{term(),term()}].
collect_meta(Request) ->
    Attrs = Request#radius_request.attrs,
    [collect_attr(Key, Val) || {Key, Val} <- Attrs].

-spec collect_message(sender(),#radius_request{}) -> iolist().
collect_message({NASIP, NASPort, ReqID}, Request) ->
    io_lib:format("~s:~p [~p]: ~s",[inet:ntoa(NASIP), NASPort, ReqID, format_cmd(Request#radius_request.cmd)]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(_) ->
    {ok, LogFile} = application:get_env(eradius, logfile),
    filelib:ensure_dir(LogFile),
    case file:open(LogFile, [append]) of
    {ok, Fd} ->
        {ok, Fd};
    {error, eacces} ->
        {stop, {file_permission_error, LogFile}};
    Error ->
        {stop, Error}
    end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({write_request, Time, Sender, Request}, State) ->
    try
	Msg = format_message(Time, Sender, Request),
	ok = io:put_chars(State, Msg)
    catch
	_:Error ->
	    lager:error("Failed to log RADIUS request: error: ~p, request: ~p, sender: ~p", 
            [Error, Request, Sender]),
	    ok
    end,
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, Fd) ->
    file:close(Fd),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% -- formatting
format_message(Time, Sender, Request) ->
    BinTStamp = radius_date(Time),
    BinSender = format_sender(Sender),
    BinCommand = format_cmd(Request#radius_request.cmd),
    BinPacket = format_packet(Request),
    <<BinTStamp/binary, " ", BinSender/binary, " ", BinCommand/binary, "\n", BinPacket/binary, "\n">>.

format_sender({NASIP, NASPort, ReqID}) ->
    <<(format_ip(NASIP))/binary, $:, (i2b(NASPort))/binary, " [", (i2b(ReqID))/binary, $]>>.

format_cmd(request)   -> <<"Access-Request">>;
format_cmd(accept)    -> <<"Access-Accept">>;
format_cmd(reject)    -> <<"Access-Reject">>;
format_cmd(challenge) -> <<"Access-Challenge">>;
format_cmd(accreq)    -> <<"Accounting-Request">>;
format_cmd(accresp)   -> <<"Accounting-Response">>;
format_cmd(coareq)    -> <<"Coa-Request">>;
format_cmd(coaack)    -> <<"Coa-Ack">>;
format_cmd(coanak)    -> <<"Coa-Nak">>;
format_cmd(discreq)   -> <<"Disconnect-Request">>;
format_cmd(discack)   -> <<"Disconnect-Ack">>;
format_cmd(discnak)   -> <<"Disconnect-Nak">>.

format_ip(IP) ->
    list_to_binary(inet_parse:ntoa(IP)).

format_packet(Request) ->
    Attrs = Request#radius_request.attrs,
    << <<(print_attr(Key, Val))/binary>> || {Key, Val} <- Attrs >>.

print_attr(Key = #attribute{name = Attr, type = Type}, InVal) ->
    FmtValUnquoted = printable_attr_value(Key, InVal),
    FmtVal         = case Type of
                         string -> <<$", FmtValUnquoted/binary, $">>;
                         _      -> FmtValUnquoted
                     end,
    <<"\t", (list_to_binary(Attr))/binary, " = ", FmtVal/binary, "\n">>;
print_attr(Id, Val) ->
    case eradius_dict:lookup(Id) of
        [Attr = #attribute{}] ->
            print_attr(Attr, Val);
        _ ->
            Name = format_unknown(Id),
            print_attr(#attribute{id = Id, name = Name, type = octets}, Val)
    end.

collect_attr(Key = #attribute{name = Attr, type = _Type}, InVal) ->
    FmtVal = collectable_attr_value(Key, InVal),
    {list_to_atom(Attr), FmtVal};
collect_attr(Id, Val) ->
    case eradius_dict:lookup(Id) of
        [Attr = #attribute{}] ->
            collect_attr(Attr, Val);
        _ ->
            Name = format_unknown(Id),
            collect_attr(#attribute{id = Id, name = Name, type = octets}, Val)
    end.

printable_attr_value(Attr = #attribute{type = {tagged, RealType}}, {Tag, RealVal}) ->
    ValBin = printable_attr_value(Attr#attribute{type = RealType}, RealVal),
    TagBin = case Tag of
                 undefined -> <<>>;
                 Int       -> <<(i2b(Int))/binary, ":">>
             end,
    <<TagBin/binary, ValBin/binary>>;
printable_attr_value(#attribute{type = string}, Value) when is_binary(Value) ->
    << <<(escape_char(C))/binary>> || <<C:8>> <= Value >>;
printable_attr_value(#attribute{type = string}, Value) when is_list(Value) ->
    << <<(escape_char(C))/binary>> || <<C:8>> <= iolist_to_binary(Value) >>;
printable_attr_value(#attribute{type = ipaddr}, {A, B, C, D}) ->
    <<(i2b(A))/binary, ".", (i2b(B))/binary, ".", (i2b(C))/binary, ".", (i2b(D))/binary>>;
printable_attr_value(#attribute{id = ID, type = integer}, Val) when is_integer(Val) ->
    case eradius_dict:lookup({ID, Val}) of
        [#value{name = VName}] -> list_to_binary(VName);
        _                      -> i2b(Val)
    end;
printable_attr_value(#attribute{type = date}, {{Y,Mo,D},{H,Min,S}}) ->
    list_to_binary(io_lib:fwrite("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B", [Y, Mo, D, H, Min, S]));
printable_attr_value(_Attr, <<Val/binary>>) ->
    <<"0x", (bin_to_hexstr(Val))/binary>>;
printable_attr_value(_Attr, Val) ->
    list_to_binary(io_lib:format("~p", [Val])).

collectable_attr_value(Attr = #attribute{type = {tagged, RealType}}, {Tag, RealVal}) ->
    ValCol = collectable_attr_value(Attr#attribute{type = RealType}, RealVal),
    TagCol = case Tag of
                 undefined -> empty;
                 Int       -> Int
             end,
    {TagCol, ValCol};
collectable_attr_value(#attribute{type = string}, Value) when is_binary(Value) ->
    binary_to_list(Value);
collectable_attr_value(#attribute{type = string}, Value) when is_list(Value) ->
    Value;
collectable_attr_value(#attribute{type = ipaddr}, IP) ->
    inet_parse:ntoa(IP);
collectable_attr_value(#attribute{id = ID, type = integer}, Val) when is_integer(Val) ->
    case eradius_dict:lookup({ID, Val}) of
        [#value{name = VName}] -> VName;
        _                      -> Val
    end;
collectable_attr_value(#attribute{type = date}, {{Y,Mo,D},{H,Min,S}}) ->
    io_lib:fwrite("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B", [Y, Mo, D, H, Min, S]);
collectable_attr_value(_Attr, <<Val/binary>>) ->
    "0x"++binary_to_list(bin_to_hexstr(Val));
collectable_attr_value(_Attr, Val) ->
    io_lib:format("~p", [Val]).

radius_date({{YYYY,MM,DD},{Hour,Min,Sec}}) ->
    DayNumber = calendar:day_of_the_week(YYYY, MM, DD),
    list_to_binary(
        io_lib:format("~s ~3.s ~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w ~4.4.0w",
            [day(DayNumber), month(MM), DD, Hour, Min, Sec, YYYY])).

format_unknown({VendId, Id}) ->
    case eradius_dict:lookup(VendId) of
        [#vendor{name = Name}] ->
            ["Unkown-", Name, $-, integer_to_list(Id)];
        _ ->
            ["Unkown-", integer_to_list(VendId), $-, integer_to_list(Id)]
    end;
format_unknown(Id) when is_integer(Id) ->
    ["Unkown-", integer_to_list(Id)].

escape_char($") -> <<"\\\"">>;
escape_char(C) when C >= 32, C < 127 -> <<C>>;
escape_char(C) -> <<"\\", (i2b(C))/binary>>.

day(1) -> "Mon";
day(2) -> "Tue";
day(3) -> "Wed";
day(4) -> "Thu";
day(5) -> "Fri";
day(6) -> "Sat";
day(7) -> "Sun".

month(1) -> "Jan";
month(2) -> "Feb";
month(3) -> "Mar";
month(4) -> "Apr";
month(5) -> "May";
month(6) -> "Jun";
month(7) -> "Jul";
month(8) -> "Aug";
month(9) -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".

-compile({inline, i2b/1}).
i2b(I) -> list_to_binary(integer_to_list(I)).

-compile({inline,hexchar/1}).
hexchar(X) when X >= 0, X < 10 ->
    X + $0;
hexchar(X) when X >= 10, X < 16 ->
    X + ($A - 10).

-compile({inline, bin_to_hexstr/1}).
bin_to_hexstr(Bin) ->
    << << (hexchar(X)) >> || <<X:4>> <= Bin >>.
