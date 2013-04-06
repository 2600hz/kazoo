%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is RabbitMQ.
%%
%% The Initial Developer of the Original Code is VMware, Inc.
%% Copyright (c) 2007-2013 VMware, Inc.  All rights reserved.
%%

-module(rabbit_guid).

-behaviour(gen_server).

-export([start_link/0]).
-export([filename/0]).
-export([gen/0, gen_secure/0, string/2, binary/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(SERIAL_FILENAME, "rabbit_serial").

-record(state, {serial}).

%%----------------------------------------------------------------------------

-ifdef(use_specs).

-export_type([guid/0]).

-type(guid() :: binary()).

-spec(start_link/0 :: () -> rabbit_types:ok_pid_or_error()).
-spec(filename/0 :: () -> string()).
-spec(gen/0 :: () -> guid()).
-spec(gen_secure/0 :: () -> guid()).
-spec(string/2 :: (guid(), any()) -> string()).
-spec(binary/2 :: (guid(), any()) -> binary()).

-endif.

%%----------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE,
                          [update_disk_serial()], []).

%% We use this to detect a (possibly rather old) Mnesia directory,
%% since it has existed since at least 1.7.0 (as far back as I cared
%% to go).
filename() ->
    filename:join(rabbit_mnesia:dir(), ?SERIAL_FILENAME).

update_disk_serial() ->
    Filename = filename(),
    Serial = case rabbit_file:read_term_file(Filename) of
                 {ok, [Num]}     -> Num;
                 {error, enoent} -> 0;
                 {error, Reason} ->
                     throw({error, {cannot_read_serial_file, Filename, Reason}})
             end,
    case rabbit_file:write_term_file(Filename, [Serial + 1]) of
        ok -> ok;
        {error, Reason1} ->
            throw({error, {cannot_write_serial_file, Filename, Reason1}})
    end,
    Serial.

%% Generate an un-hashed guid.
fresh() ->
    %% We don't use erlang:now() here because a) it may return
    %% duplicates when the system clock has been rewound prior to a
    %% restart, or ids were generated at a high rate (which causes
    %% now() to move ahead of the system time), and b) it is really
    %% slow since it takes a global lock and makes a system call.
    %%
    %% A persisted serial number, the node, and a unique reference
    %% (per node incarnation) uniquely identifies a process in space
    %% and time.
    Serial = gen_server:call(?SERVER, serial, infinity),
    {Serial, node(), make_ref()}.

advance_blocks({B1, B2, B3, B4}, I) ->
    %% To produce a new set of blocks, we create a new 32bit block
    %% hashing {B5, I}. The new hash is used as last block, and the
    %% other three blocks are XORed with it.
    %%
    %% Doing this is convenient because it avoids cascading conflits,
    %% while being very fast. The conflicts are avoided by propagating
    %% the changes through all the blocks at each round by XORing, so
    %% the only occasion in which a collision will take place is when
    %% all 4 blocks are the same and the counter is the same.
    %%
    %% The range (2^32) is provided explicitly since phash uses 2^27
    %% by default.
    B5 = erlang:phash2({B1, I}, 4294967296),
    {{(B2 bxor B5), (B3 bxor B5), (B4 bxor B5), B5}, I+1}.

%% generate a GUID. This function should be used when performance is a
%% priority and predictability is not an issue. Otherwise use
%% gen_secure/0.
gen() ->
    %% We hash a fresh GUID with md5, split it in 4 blocks, and each
    %% time we need a new guid we rotate them producing a new hash
    %% with the aid of the counter. Look at the comments in
    %% advance_blocks/2 for details.
    case get(guid) of
        undefined -> <<B1:32, B2:32, B3:32, B4:32>> = Res =
                         erlang:md5(term_to_binary(fresh())),
                     put(guid, {{B1, B2, B3, B4}, 0}),
                     Res;
        {BS, I}   -> {{B1, B2, B3, B4}, _} = S = advance_blocks(BS, I),
                     put(guid, S),
                     <<B1:32, B2:32, B3:32, B4:32>>
    end.

%% generate a non-predictable GUID.
%%
%% The id is only unique within a single cluster and as long as the
%% serial store hasn't been deleted.
%%
%% If you are not concerned with predictability, gen/0 is faster.
gen_secure() ->
    %% Here instead of hashing once we hash the GUID and the counter
    %% each time, so that the GUID is not predictable.
    G = case get(guid_secure) of
            undefined -> {fresh(), 0};
            {S, I}    -> {S, I+1}
        end,
    put(guid_secure, G),
    erlang:md5(term_to_binary(G)).

%% generate a readable string representation of a GUID.
%%
%% employs base64url encoding, which is safer in more contexts than
%% plain base64.
string(G, Prefix) ->
    Prefix ++ "-" ++ rabbit_misc:base64url(G).

binary(G, Prefix) ->
    list_to_binary(string(G, Prefix)).

%%----------------------------------------------------------------------------

init([Serial]) ->
    {ok, #state{serial = Serial}}.

handle_call(serial, _From, State = #state{serial = Serial}) ->
    {reply, Serial, State};

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
