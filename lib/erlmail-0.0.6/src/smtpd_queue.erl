%%%---------------------------------------------------------------------------------------
%%% @author    Stuart Jackson <sjackson@simpleenigma.com> [http://erlsoft.org]
%%% @copyright 2006 - 2007 Simple Enigma, Inc. All Rights Reserved.
%%% @doc       SMTP Outgoing Mail Queue
%%% @reference See <a href="http://erlsoft.org/modules/erlmail" target="_top">Erlang Software Framework</a> for more information
%%% @reference See <a href="http://erlmail.googlecode.com" target="_top">ErlMail Google Code Repository</a> for more information
%%% @version   0.0.6
%%% @since     0.0.6
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Stuart Jackson, Simple Enigma, Inc. All Righs Reserved
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%%
%%%---------------------------------------------------------------------------------------
-module(smtpd_queue).
-author('sjackson@simpleenigma.com').
-include("../include/smtp.hrl").

-define(MAXIPV4,round(math:pow(2,32))).

-export([checkip/1]).
-export([ip_in_range/2,aton/1,ntoa/1,mtor/2]).
-export([mton/1,ntom/1,ctom/1,mtoc/1,cdir_to_number/1,number_to_cdir/1]).



%%-------------------------------------------------------------------------
%% @spec (IP::tuple()) -> true | false
%% @doc  Determines of IP address is in any of the ranges listed in the 
%%		server_smtp_relay_clients enviroment variable. Check configuration
%%		information on how to add more ranges into the user config file.
%% @end
%%-------------------------------------------------------------------------
checkip(IP) ->
	Ranges = erlmail_util:get_app_env(server_smtp_relay_clients,[]),
	checkip(IP,Ranges).

%%-------------------------------------------------------------------------
%% @spec (IP::tuple(),{Start::tuple(),End::tuple}) -> true | false
%% @doc
%% @hidden
%% @end
%%-------------------------------------------------------------------------
checkip(_IP,[]) -> false;
checkip(IP,[{Network,Mask}|T]) ->
	case ip_in_range(IP,mtor(Network,Mask)) of
		true -> true;
		false -> checkip(IP,T)
	end.

%%-------------------------------------------------------------------------
%% @spec (IP::tuple(),{Start::tuple(),End::tuple}) -> true | false
%% @doc  determines of IP address is between Start and End
%% @end
%%-------------------------------------------------------------------------
ip_in_range(IP,{Start,End}) -> 
	D = aton(IP),
	S = aton(Start),
	E = aton(End),
	if
		D >= S, D =< E -> true;
		true -> false
	end.

%%-------------------------------------------------------------------------
%% @spec (Network::tuple(),NetMask::tuple) -> {StartIP::tuple(),EndIP::tuple()}
%% @doc  Task a network number and a NetMask or CDIR and converts them into a
%%		starting IP address and an ending IP address in tuple form
%% @end
%%-------------------------------------------------------------------------
mtor(Network,CDIR) when is_integer(CDIR) -> mtor(Network,ctom(CDIR));
mtor(Network,Mask) when is_tuple(Mask) ->
	M = mton(Mask),
	D = aton(Network),
	Start = ntoa(D + 1),
	End = ntoa(D + M - 2),
	{Start,End}.


%%-------------------------------------------------------------------------
%% @spec (CIDR::integer()) -> Number::integer()
%% @doc  Converts a CIDR number into the number of IP addresses
%% @end
%%-------------------------------------------------------------------------
cdir_to_number(CDIR) -> round(math:pow(2,32-CDIR)).
%%-------------------------------------------------------------------------
%% @spec (Number::integet) -> CDIR::integer()
%% @doc  Converts the number of ip addresses into a CDIR number
%% @end
%%-------------------------------------------------------------------------
number_to_cdir(N) -> 
	C = number_to_cdir(N,0),
	32 - C.
%%-------------------------------------------------------------------------
%% @spec (N::integer(),C::integer()) -> C::intger()
%% @doc  
%% @hidden
%% @end
%%-------------------------------------------------------------------------
number_to_cdir(N,C) ->
	R = N rem 2,
	if
		R == 0 -> number_to_cdir(N div 2,C + 1);
		true -> C
	end.

%%-------------------------------------------------------------------------
%% @spec (CIDR::integer()) -> NetMask::tuple()
%% @doc  Converts a CIDR number into a NetMask tuple
%% @end
%%-------------------------------------------------------------------------
ctom(CDIR) -> ntoa(?MAXIPV4 - cdir_to_number(CDIR)).

%%-------------------------------------------------------------------------
%% @spec (NetMask::tuple()) -> CIDR::integer()
%% @doc  Converts a NetMask tuple into a CIDR number
%% @end
%%-------------------------------------------------------------------------
mtoc(Mask) -> number_to_cdir(?MAXIPV4 - aton(Mask)).

%%-------------------------------------------------------------------------
%% @spec (NetMask::tuple()) -> Dec::integer()
%% @doc  Converts a NetMask tuple into a integer
%% @end
%%-------------------------------------------------------------------------
mton(Mask) -> ?MAXIPV4 - aton(Mask).
%%-------------------------------------------------------------------------
%% @spec (Dec::integer()) -> NetMask::tuple()
%% @doc  Converts an integer into a NetMask tuple
%% @end
%%-------------------------------------------------------------------------
ntom(Dec) -> ntoa(?MAXIPV4 - Dec).

%%-------------------------------------------------------------------------
%% @spec (IPAddress::tuple()) -> Dec::integer()
%% @doc  Converts a IPAddress tuple into a integer
%% @end
%%-------------------------------------------------------------------------
aton({I1,I2,I3,I4}) ->
	<<Dec:32>> = <<I1:8,I2:8,I3:8,I4:8>>,
	Dec.
%%-------------------------------------------------------------------------
%% @spec (Dec::integer()) -> IPAddress::tuple()
%% @doc  Converts a integer into a IPAddress tuple
%% @end
%%-------------------------------------------------------------------------
ntoa(Dec) ->
	<<I1:8,I2:8,I3:8,I4:8>> = <<Dec:32>>,
	{I1,I2,I3,I4}.




