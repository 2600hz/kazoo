%% @author Benoît Chesneau <benoitc@e-engura.org>
%% @copyright 2009 Benoît Chesneau.
%%
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.


-define(DEFAULT_TIMEOUT, 60000).

-type header() :: {string() | atom(), string()}.
-type headers() :: [header()].
%% In R13B bool() is now called boolean()
%% Uncomment if it's not compiling.
%% -type boolean() :: bool()



-type db_name() :: binary() | string().
-type docid() :: binary() | string().

-type ejson() :: ejson_object() | ejson_array().

-type ejson_array() :: [ejson_term()].
-type ejson_object() :: {[{ejson_key(), ejson_term()}]}.

-type ejson_key() :: binary() | atom().

-type ejson_term() :: ejson_array()
    | ejson_object()
    | ejson_string()
    | ejson_number()
    | true | false | null.

-type ejson_string() :: binary().

-type ejson_number() :: float() | integer().

-type changes_option() :: continuous | longpoll | normal
    | include_docs | {since, integer()}
    | {timeout, integer()}
    | heartbeat | {heartbeat, integer()}
    | {filter, string()} | {filter, string(), list({string(), string() | integer()}
)}
    | conflicts | {style, string()} | descending.
-type changes_options() :: list(changes_option()).

-type changes_option1() :: longpoll | normal
    | include_docs | {since, integer()}
    | {timeout, integer()}
    | heartbeat | {heartbeat, integer()}
    | {filter, string()} | {filter, string(), list({string(), string() | integer()}
)}
    | conflicts | {style, string()} | descending.
-type changes_options1() :: list(changes_option1()).

-type stale() :: ok | update_after.

-type view_option() :: {key, binary()}
                     | {keys, list(binary())}
                     | {startkey_docid, binary()}
                     | {endkey_docid, binary()}
                     | {startkey, binary()}
                     | {endkey, binary()}
                     | {limit, integer()}
                     | {stale, stale()}
                     | descending
                     | {skip, integer()}
                     | group
                     | {group, boolean()}
                     | {group_level, exact | integer()}
                     | reduce
                     | {reduce, boolean()}
                     | inclusive_end
                     | {inclusive_end, boolean()}
                     | include_docs
                     | {include_docs, boolean()}
                     | conflicts
                     | update_seq
                     | {list, binary()}
                     | {list(), binary()}
                     | {binary(), binary()}.

-type view_options() :: list(view_option()).

-record(view_query_args, {
        method = get :: atom(),
        options = [] :: view_options(),
        keys = [] :: list(binary())}).

-type view_query_args() :: #view_query_args{}.

-record(server, {
    host :: string(),
    port :: integer(),
    prefix :: string(),
    options = [] :: list()
}).

-type server() :: #server{}.

% record to keep database information
-record(db, {
    server :: server(),
    name :: string(),
    options = [] :: list()
}).

-type db() :: #db{}.

-record(server_uuids, {
    host_port,
    uuids
}).

-record(view, {
    db :: db(),
    name :: string(),
    options :: iolist(),
    method :: atom(),
    body :: iolist(),
    headers :: list(),
    url_parts :: list(),
    url :: string()
}).

-type view() :: #view{}.

-record(changes_args, {
        type = normal,
        since = 0,
        http_options = []}).
-type changes_args() :: #changes_args{}.

-record(gen_changes_state, {
    start_ref,
    changes_pid,
    last_seq=0,
    mod,
    modstate,
    db,
    options}).

-define(USER_AGENT, "couchbeam/0.7.0").

-define(DEPRECATED(Old, New, When),
    couchbeam_util:deprecated(Old, New, When)).
