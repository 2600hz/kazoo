%% Copyright (c) 2011-2013, Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%% @doc Behaviour for short-lived HTTP handlers.
%%
%% <em>init/3</em> allows you to initialize a state for all subsequent
%% callbacks, and indicate to Cowboy whether you accept to handle the
%% request or want to shutdown without handling it, in which case the
%% <em>handle/2</em> call will simply be skipped.
%%
%% <em>handle/2</em> allows you to handle the request. It receives the
%% state previously defined.
%%
%% <em>terminate/3</em> allows you to clean up. It receives the
%% termination reason and the state previously defined.
%%
%% There is no required operation to perform in any of these callbacks
%% other than returning the proper values. Make sure you always return
%% the last modified Req so that Cowboy has the up to date information
%% about the request.
-module(cowboy_http_handler).

-type opts() :: any().
-type state() :: any().
-type terminate_reason() :: {normal, shutdown}
	| {normal, timeout} %% Only occurs in loop handlers.
	| {error, closed} %% Only occurs in loop handlers.
	| {error, overflow} %% Only occurs in loop handlers.
	| {error, atom()}.

-callback init({atom(), http}, Req, opts())
	-> {ok, Req, state()}
	| {loop, Req, state()}
	| {loop, Req, state(), hibernate}
	| {loop, Req, state(), timeout()}
	| {loop, Req, state(), timeout(), hibernate}
	| {shutdown, Req, state()}
	| {upgrade, protocol, module()}
	| {upgrade, protocol, module(), Req, opts()}
	when Req::cowboy_req:req().
-callback handle(Req, State) -> {ok, Req, State}
	when Req::cowboy_req:req(), State::state().
-callback terminate(terminate_reason(), cowboy_req:req(), state()) -> ok.
