%% Copyright ProcessOne 2006-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.


%% This record is used to pass received packets back to client.
%% The record is defined to make it easy to use pattern matching on
%% the most used data received.
-record(received_packet,
        {
          packet_type, % message, iq, presence
          type_attr,   % depend on packet. Example: set, get, subscribe, etc
          from,        % JID
          id,          % Packet ID
          queryns,     % IQ only: Namespace of the query
          raw_packet   % raw exmpp record
        }).
