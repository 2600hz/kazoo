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

-module(escalus_assert).

-export([is_chat_message/2,
         has_no_stanzas/1,
         is_iq/2,
         is_presence_stanza/1,
         is_presence_type/2,
         is_presence_with_show/2,
         is_presence_with_status/2,
         is_presence_with_priority/2,
         is_stanza_from/2,
         is_roster_set/1,
         is_roster_result/1,
         is_result/1,
         count_roster_items/2,
         roster_contains/2,
         is_privacy_query_result/1,
         is_privacy_query_result_with_active/1,
         is_privacy_query_result_with_active/2,
         is_privacy_query_result_with_default/1,
         is_privacy_query_result_with_default/2,
         is_privacy_list_nonexistent_error/1,
         is_error/3]).

-include_lib("exml/include/exml.hrl").
-include("escalus.hrl").

%%===================================================================
%% Special cases
%%===================================================================

% note argument order change (backwards compatibility hack)
-spec is_error(exml:element(), binary(), binary()) -> ok | no_return().
is_error(Stanza, Type, Condition) ->
    escalus:assert(is_error, [Type, Condition], Stanza).

% Assertion about client, not Stanza
-spec has_no_stanzas(#client{}) -> ok | no_return().
has_no_stanzas(Client) ->
    case escalus_client:peek_stanzas(Client) of
        [] ->
            ok;
        Stanzas ->
            escalus_utils:log_stanzas("following stanzas shouldn't be there", Stanzas),
            escalus_ct:fail({has_stanzas_but_shouldnt, Client, Stanzas})
    end.

%%===================================================================
%% Forward to new API
%%===================================================================

-define(USE_NEW_API_1_EXT(F, P),
    F(Stanza) ->
        escalus:assert(P, Stanza)).

-define(USE_NEW_API_2_EXT(F, P),
    F(Param, Stanza) ->
        escalus:assert(P, [Param], Stanza)).

-define(USE_NEW_API_1(F), ?USE_NEW_API_1_EXT(F, F)).
-define(USE_NEW_API_2(F), ?USE_NEW_API_2_EXT(F, F)).

?USE_NEW_API_2(is_chat_message).
?USE_NEW_API_2(is_iq).
?USE_NEW_API_1(is_presence_stanza).
?USE_NEW_API_2(is_presence_type).
?USE_NEW_API_2(is_presence_with_show).
?USE_NEW_API_2(is_presence_with_status).
?USE_NEW_API_2(is_presence_with_priority).
?USE_NEW_API_2(is_stanza_from).
?USE_NEW_API_1(is_roster_set).
?USE_NEW_API_1(is_roster_result).
?USE_NEW_API_1(is_result).
?USE_NEW_API_2(count_roster_items).
?USE_NEW_API_2(roster_contains).
?USE_NEW_API_1_EXT(is_privacy_query_result,
                   is_privacy_result).
?USE_NEW_API_1_EXT(is_privacy_query_result_with_active,
                   is_privacy_result_with_active).
?USE_NEW_API_2_EXT(is_privacy_query_result_with_active,
                   is_privacy_result_with_active).
?USE_NEW_API_1_EXT(is_privacy_query_result_with_default,
                   is_privacy_result_with_default).
?USE_NEW_API_2_EXT(is_privacy_query_result_with_default,
                   is_privacy_result_with_default).
?USE_NEW_API_1(is_privacy_list_nonexistent_error).
