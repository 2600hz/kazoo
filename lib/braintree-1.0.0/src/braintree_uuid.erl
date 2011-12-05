%% -----------------------------------------------------------------------------
%% Copyright © 2010 Per Andersson
%%
%% Erlang UUID is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% Erlang UUID is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with erlang-uuid.  If not, see <http://www.gnu.org/licenses/>.
%% -----------------------------------------------------------------------------
%% @author Per Andersson <avtobiff@gmail.com>
%% @copyright 2010 Per Andersson
%% @doc
%% Erlang UUID
%%
%% Currently implements UUID v4, UUID generated with (pseudo) random number
%% generator.
%%
%% Example usage
%% <pre>
%%     1> uuid:to_string(uuid:uuid4()).
%%     "79f492f8-1337-4200-abcd-92bada1cacao"
%% </pre>
%% @end
%% -----------------------------------------------------------------------------

-module(braintree_uuid).
-author('Per Andersson').

-export([uuid4/0, to_string/0, to_string/1, to_binary/0, to_binary/1]).



%% @doc  Create a UUID v4 (random) as a binary
%% @spec () -> binary()
uuid4() ->
    {A1, A2, A3} = now(),
    _ = random:seed(A1, A2, A3),
    U0 = random:uniform((2 bsl 48) - 1),
    U1 = random:uniform((2 bsl 12) - 1),
    U2 = random:uniform((2 bsl 60) - 1),
    uuid4(U0, U1, U2).


%% @private
%% @doc  Create a 128 bit binary (UUID v4) from input
%% @spec (U0, U1, U2) -> binary()
%% where U0 = U1 = U2 = integer()
uuid4(U0, U1, U2) ->
     <<U0:48, 4:4, U1:12, 10:4, U2:60>>.


%% @doc  Format uuid string from binary
%% @spec (Uuid::binary()) -> string()
to_string() ->
    to_string(uuid4()).
to_string(<<U0:32, U1:16, U2:16, U3:16, U4:48>>) ->
    lists:flatten(io_lib:format(
        "~8.16.0b~4.16.0b~4.16.0b~4.16.0b~12.16.0b",
        [U0, U1, U2, U3, U4]));
to_string(_) ->
        erlang:error(badarg).

%% @doc  Format uuid binary from string
%% @spec (UuidStr::string()) -> binary()
to_binary() ->
    to_binary(uuid4()).
to_binary(UuidStr) when is_list(UuidStr) ->
    Parts = string:tokens(UuidStr, "$-"),
    [I0, I1, I2, I3, I4] = [hex_to_int(Part) || Part <- Parts],
    <<I0:32, I1:16, I2:16, I3:16, I4:48>>;
to_binary(Bin) when is_binary(Bin) ->
    Bin;
to_binary(_) ->
    erlang:error(badarg).

hex_to_int(Hex) ->
    {ok, [D], []} = io_lib:fread("~16u", Hex),
    D.
