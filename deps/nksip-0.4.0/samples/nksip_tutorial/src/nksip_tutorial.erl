%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Carlos Gonzalez Florido.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc Companion code for NkSIP Tutorial.


-module(nksip_tutorial).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([launch/0, trace/1, loglevel/1]).

%% @doc Launches the full tutorial.
launch() ->
    {ok, _} = nksip:start(server, nksip_tutorial_sipapp_server, [], 
        [
            {plugins, [nksip_registrar]},
            {transports, [{udp, all, 5060}, {tls, all, 5061}]}
         ]),
    {ok, _} = nksip:start(client1, nksip_tutorial_sipapp_client, [], 
        [
            {plugins, [nksip_uac_auto_auth]},
            {from, "sip:client1@nksip"},
            {transports, [{udp, {127,0,0,1}, 5070}, {tls, {127,0,0,1}, 5071}]}
        ]),
    {ok, _} = nksip:start(client2, nksip_tutorial_sipapp_client, [], 
        [
            {plugins, [nksip_uac_auto_auth]},
            {from, "sips:client2@nksip"},
            {transports, [udp, tls]}
        ]),

    nksip_registrar_util:clear(),


    {ok,200,[]} = nksip_uac:options(client2, "sip:127.0.0.1:5070", []),
    {ok,407,[{reason_phrase, <<"Proxy Authentication Required">>}]} =
        nksip_uac:options(client1, "sip:127.0.0.1", [{meta,[reason_phrase]}]),

    {ok,200,[]} = nksip_uac:options(client1, "sip:127.0.0.1", [{pass, "1234"}]),
    {ok,200,[]} = nksip_uac:options(client2, "<sip:127.0.0.1;transport=tls>", [{pass, "1234"}]),

    {ok,200,[{<<"contact">>, [<<"<sip:client1@127.0.0.1:5070>", _/binary>>]}]} = 
        nksip_uac:register(client1, "sip:127.0.0.1", 
                           [{pass, "1234"}, contact, {meta, [<<"contact">>]}]),

    {ok,200,[]} = nksip_uac:register(client2, "sips:127.0.0.1", [{pass, "1234"}, contact]),

    {ok,200,[{all_headers, _}]} = 
        nksip_uac:register(client2, "sips:127.0.0.1", [{pass, "1234"}, {meta, [all_headers]}]),

    {ok,200,[]} = nksip_uac:options(client1, "sip:127.0.0.1", []),
    {ok,200,[]} = nksip_uac:options(client2, "sips:127.0.0.1", []),

    {ok,407,[]} = nksip_uac:options(client1, "sips:client2@nksip", [{route, "<sip:127.0.0.1;lr>"}]),
    {ok,200,[{<<"x-nk-id">>, [<<"client2">>]}]} = 
        nksip_uac:options(client1, "sips:client2@nksip", 
                          [{route, "<sip:127.0.0.1;lr>"}, {pass, "1234"},
                           {meta, [<<"x-nk-id">>]}]),

    {ok,488,[]} = 
        nksip_uac:invite(client2, "sip:client1@nksip", [{route, "<sips:127.0.0.1;lr>"}]),

    {ok,200,[{dialog, DlgId}]}= 
        nksip_uac:invite(client2, "sip:client1@nksip", 
                        [{route, "<sips:127.0.0.1;lr>"}, {body, nksip_sdp:new()},
                          auto_2xx_ack]),

    {ok, confirmed} = nksip_dialog:meta(invite_status, DlgId),
    [_, _, _] = nksip_dialog:get_all_data(),

    {ok,200,[]} = nksip_uac:bye(DlgId, []),
    ok = nksip:stop_all().


%% ===================================================================
%% Utilities
%% ===================================================================

%% @doc Enables SIP trace messages to console.
-spec trace(Start::boolean()) -> 
    ok.

trace(true) ->  
    nksip_trace:start(),
    ok;
trace(false) -> 
    nksip_trace:stop(),
    ok.


%% @doc Changes console log level.
%% Availanle options are `debug' (maximum), `info' (medium) and `notice' (minimum).
-spec loglevel(debug|info|notice) -> 
    ok.

loglevel(Level) -> 
    lager:set_loglevel(lager_console_backend, Level),
    {ok, _} = nksip:update(server, [{log_level, Level}]),
    {ok, _} = nksip:update(client1, [{log_level, Level}]),
    {ok, _} = nksip:update(client2, [{log_level, Level}]),
    ok.





%% ===================================================================
%% EUnit tests
%% ===================================================================


% -ifdef(TEST).

% -include_lib("eunit/include/eunit.hrl").

% tutorial_test() ->
%     nksip_app:start(),
%     ok = launch().

% -endif.

