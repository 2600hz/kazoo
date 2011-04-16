%%==============================================================================
%% Copyright 2010 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-module(cecho).

-author('mazen.harake@erlang-solutions.com').

-behaviour(application).
-include("include/cecho.hrl").
-include("include/cecho_commands.hrl").

%% Behaviour Callbacks
-export([start/2, stop/1]).

%% Application API

-export([refresh/0, cbreak/0, nocbreak/0, echo/0, noecho/0, addch/1, addstr/1,
	 move/2, getyx/0, getmaxyx/0, curs_set/1, erase/0, has_colors/0,
	 start_color/0, init_pair/3, attron/1, attroff/1, nl/0, nonl/0,
	 scrollok/2, mvaddch/3, mvaddstr/3, newwin/4, delwin/1, wmove/3,
	 waddstr/2, waddch/2, mvwaddstr/4, mvwaddch/4, wrefresh/1, hline/2,
	 whline/3, vline/2, wvline/3, border/8, wborder/9, box/3, getyx/1,
	 getmaxyx/1, attron/2, attroff/2, keypad/2, getch/0]).

%% =============================================================================
%% Application API
%% =============================================================================
refresh() ->
    call(?REFRESH).

cbreak() -> 
    call(?CBREAK).

nocbreak() -> 
    call(?NOCBREAK).

echo() -> 
    call(?ECHO).

noecho() -> 
    call(?NOECHO).

addch(Char) when is_integer(Char) ->
    call(?ADDCH, Char).

addstr(String) when is_list(String) ->
    Str = lists:flatten(String),
    call(?ADDSTR, {erlang:iolist_size(Str), Str}).

move(Y, X) when is_integer(X) andalso is_integer(Y) ->
    call(?MOVE, {Y, X}).

getyx() ->
    getyx(?ceSTDSCR).

getyx(Window) when is_integer(Window) ->
    call(?GETYX, Window).

getmaxyx() ->
    getmaxyx(?ceSTDSCR).

getmaxyx(Window) when is_integer(Window) ->
    call(?GETMAXYX, Window).

curs_set(Flag) when is_integer(Flag) ->
    call(?CURS_SET, Flag).

erase() ->
    werase(?ceSTDSCR).

werase(Window) when is_integer(Window) ->
    call(?WERASE, Window).

has_colors() ->
    call(?HAS_COLORS).

start_color() ->
    call(?START_COLOR).

init_pair(N, FColor, BColor) when is_integer(N) andalso is_integer(FColor)
				  andalso is_integer(BColor) ->
    call(?INIT_PAIR, {N, FColor, BColor}).

attron(Mask) ->
    attron(?ceSTDSCR, Mask).

attron(Window, Mask) when is_integer(Mask) andalso is_integer(Window) ->
    call(?WATTRON, {Window, Mask}).

attroff(Mask) ->
    attroff(?ceSTDSCR, Mask).

attroff(Window, Mask) when is_integer(Mask) andalso is_integer(Window) ->
    call(?WATTROFF, {Window, Mask}).

nl() ->
    call(?NL).

nonl() ->
    call(?NONL).

scrollok(Window, BFlag) when is_integer(Window) andalso is_boolean(BFlag) ->
    call(?SCROLLOK, {Window, BFlag}).

mvaddch(Y, X, Char) when is_integer(Char) andalso is_integer(X) 
			 andalso is_integer(Y) ->
    call(?MVADDCH, {Y, X, Char}).

mvaddstr(Y, X, String) when is_list(String) andalso is_integer(X) andalso 
			    is_integer(Y) ->
    Str = lists:flatten(String),
    call(?MVADDSTR, {Y, X, erlang:iolist_size(Str), Str}).

newwin(Height, Width, StartY, StartX) when is_integer(Height) andalso 
					   is_integer(Width) andalso 
					   is_integer(StartY) andalso
					   is_integer(StartX) ->
    call(?NEWWIN, {Height, Width, StartY, StartX}).

delwin(Window) when is_integer(Window) ->
    call(?DELWIN, Window).

wmove(Window, Y, X) when is_integer(Window) andalso is_integer(Y) andalso 
			 is_integer(X) ->
    call(?WMOVE, {Window, Y, X}).

waddstr(Window, String) when is_integer(Window) andalso is_list(String) ->
    Str = lists:flatten(String),
    call(?WADDSTR, {Window, erlang:iolist_size(Str), Str}).

waddch(Window, Char) when is_integer(Window) andalso is_integer(Char) ->
    call(?WADDCH, {Window, Char}).

mvwaddstr(Window, Y, X, String) when is_integer(Window) andalso is_integer(Y)
				     andalso is_integer(X) andalso 
				     is_list(String) ->
    Str = lists:flatten(String),
    call(?MVWADDSTR, {Window, Y, X, erlang:iolist_size(Str), Str}).

mvwaddch(Window, Y, X, Char) when is_integer(Window) andalso is_integer(Y)
                                  andalso is_integer(X) ->
    call(?MVWADDCH, {Window, Y, X, Char}).

wrefresh(Window) when is_integer(Window) ->
    call(?WREFRESH, Window).

hline(Char, MaxN) ->
    whline(?ceSTDSCR, Char, MaxN).

whline(Window, Char, MaxN) when is_integer(Window) andalso is_integer(MaxN) ->
    call(?WHLINE, {Window, Char, MaxN}).

vline(Char, MaxN) ->
    wvline(?ceSTDSCR, Char, MaxN).

wvline(Window, Char, MaxN) when is_integer(Window) andalso is_integer(MaxN) ->
    call(?WVLINE, {Window, Char, MaxN}).

border(Ls, Rs, Ts, Bs, TLs, TRs, BLs, BRs) ->
    wborder(0, Ls, Rs, Ts, Bs, TLs, TRs, BLs, BRs).

wborder(Window, Ls, Rs, Ts, Bs, TLs, TRs, BLs, BRs) 
  when is_integer(Ls) andalso is_integer(Rs) andalso 
       is_integer(Ts) andalso is_integer(Bs) andalso 
       is_integer(TLs) andalso is_integer(TRs) andalso 
       is_integer(BLs) andalso is_integer(BRs) ->
    call(?WBORDER, {Window, Ls, Rs, Ts, Bs, TLs, TRs, BLs, BRs}).

box(Window, Vert, Horz) when is_integer(Window) andalso is_integer(Vert) andalso
			     is_integer(Horz) ->
    call(?BOX, {Window, Vert, Horz}).

keypad(Window, BFlag) when is_integer(Window) andalso is_boolean(BFlag) ->
    call(?KEYPAD, {Window, BFlag}).

getch() ->
    cecho_srv:getch().

%% =============================================================================
%% Behaviour Callbacks
%% =============================================================================
start(_, _) ->
    cecho_srv:start_link().

stop(_) ->
    ok.

%% =============================================================================
%% Internal Functions
%% =============================================================================
call(Cmd) ->
    call(Cmd, undefined).

call(Cmd, Args) ->
    cecho_srv:call(Cmd, Args).
