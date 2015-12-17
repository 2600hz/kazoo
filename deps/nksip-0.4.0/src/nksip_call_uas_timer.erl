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

%% @private Call UAS Timer anagement
-module(nksip_call_uas_timer).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([timer/3]).

-import(nksip_call_lib, [update/2]).

-include("nksip.hrl").
-include("nksip_call.hrl").



%% ===================================================================
%% Timers
%% ===================================================================

%% @private
-spec timer(nksip_call_lib:timer()|term(), nksip_call:trans_id(), nksip_call:call()) ->
    nksip_call:call().

timer(Tag, Id, #call{app_id=AppId, trans=Trans}=Call) ->
    case lists:keyfind(Id, #trans.id, Trans) of
        #trans{class=uas}=UAS ->
            case AppId:nkcb_uas_timer(Tag, UAS, Call) of
                {ok, Call1} ->
                    Call1;
                {continue, [Tag1, UAS1, Call1]} ->
                    do_timer(Tag1, UAS1, Call1)
            end;
        false ->
            ?call_warning("Call ignoring uas timer (~p, ~p)", [Tag, Id]),
            Call
    end.


%% @private
-spec do_timer(nksip_call_lib:timer(), nksip_call:trans(), nksip_call:call()) ->
    nksip_call:call().

do_timer(timer_c, #trans{id=Id, method=Method}=UAS, Call) ->
    ?call_notice("UAS ~p ~p Timer C fired", [Id, Method]),
    nksip_call_uas:do_reply({timeout, <<"Timer C Timeout">>}, UAS, Call);

do_timer(noinvite, #trans{id=Id, method=Method}=UAS, Call) ->
    ?call_notice("UAS ~p ~p No-INVITE timer fired", [Id, Method]),
    nksip_call_uas:do_reply({timeout, <<"No-INVITE Timeout">>}, UAS, Call);

% INVITE 3456xx retrans
do_timer(timer_g, #trans{id=Id, response=Resp}=UAS, Call) ->
    #sipmsg{class={resp, Code, _Reason}} = Resp,
    UAS2 = case nksip_call_uas_transp:resend_response(Resp, []) of
        {ok, _} ->
            ?call_info("UAS ~p retransmitting 'INVITE' ~p response", [Id, Code]),
            nksip_call_lib:retrans_timer(timer_g, UAS, Call);
        error -> 
            ?call_notice("UAS ~p could not retransmit 'INVITE' ~p response", [Id, Code]),
            UAS1 = UAS#trans{status=finished},
            nksip_call_lib:timeout_timer(cancel, UAS1, Call)
    end,
    update(UAS2, Call);

% INVITE accepted finished
do_timer(timer_l, #trans{id=Id}=UAS, Call) ->
    ?call_debug("UAS ~p 'INVITE' Timer L fired", [Id]),
    UAS1 = UAS#trans{status=finished},
    UAS2 = nksip_call_lib:timeout_timer(cancel, UAS1, Call),
    update(UAS2, Call);

% INVITE confirmed finished
do_timer(timer_i, #trans{id=Id}=UAS, Call) ->
    ?call_debug("UAS ~p 'INVITE' Timer I fired", [Id]),
    UAS1 = UAS#trans{status=finished},
    UAS2 = nksip_call_lib:timeout_timer(cancel, UAS1, Call),
    update(UAS2, Call);

% NoINVITE completed finished
do_timer(timer_j, #trans{id=Id, method=Method}=UAS, Call) ->
    ?call_debug("UAS ~p ~p Timer J fired", [Id, Method]),
    UAS1 = UAS#trans{status=finished},
    UAS2 = nksip_call_lib:timeout_timer(cancel, UAS1, Call),
    update(UAS2, Call);

% INVITE completed timeout
do_timer(timer_h, #trans{id=Id}=UAS, Call) ->
    ?call_notice("UAS ~p 'INVITE' timeout (Timer H) fired, no ACK received", [Id]),
    UAS1 = UAS#trans{status=finished},
    UAS2 = nksip_call_lib:timeout_timer(cancel, UAS1, Call),
    UAS3 = nksip_call_lib:retrans_timer(cancel, UAS2, Call),
    update(UAS3, Call);

do_timer(expire, #trans{id=Id, status=invite_proceeding, from=From}=UAS, Call) ->
    ?call_debug("UAS ~p 'INVITE' Timer Expire fired: sending 487",[Id]),
    case From of
        {fork, ForkId} -> 
            % We do not cancel our UAS request, we send it to the fork
            % Proxied remotes should send the 487 (ot not)
            nksip_call_fork:cancel(ForkId, Call);
        _ ->  
            UAS1 = UAS#trans{cancel=cancelled},
            Call1 = update(UAS1, Call),
            nksip_call_uas:do_reply(request_terminated, UAS1, Call1)
    end;

do_timer(expire, #trans{id=Id, status=Status}, Call) ->
    ?call_debug("UAS ~p 'INVITE' (~p) Timer Expire fired (ignored)", [Id, Status]),
    Call.