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

%% @doc NkSIP Registrar Plugin Callbacks
-module(nksip_registrar_callbacks).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-include("../include/nksip.hrl").
-include("../include/nksip_call.hrl").
-include("nksip_registrar.hrl").

-export([nkcb_sip_method/2, nkcb_authorize_data/3]).
-export([nkcb_nksip_registrar_request_opts/2, nkcb_nksip_registrar_request_reply/3,
         nkcb_nksip_registrar_get_index/2, nkcb_nksip_registrar_update_regcontact/4]).


%%%%%%%%%%%%%%%% Implemented core plugin callbacks %%%%%%%%%%%%%%%%%%%%%%%%%


%% @private This plugin callback is called when a call to one of the method specific
%% application-level SipApp callbacks is needed.
-spec nkcb_sip_method(nksip_call:trans(), nksip_call:call()) ->
    {reply, nksip:sipreply()} | noreply.


nkcb_sip_method(#trans{method='REGISTER', request=Req}, #call{app_id=AppId}) ->
    Module = AppId:module(),
    case 
        Module/=nksip_sipapp andalso
        erlang:function_exported(Module, sip_register, 2) 
    of
        true ->
            continue;
        false ->
            {reply, nksip_registrar:request(Req)}
    end;
nkcb_sip_method(_Trans, _Call) ->
    continue.


%% @private
nkcb_authorize_data(List, #trans{request=Req}=Trans, Call) ->
    case nksip_registrar:is_registered(Req) of
        true -> {continue, [[register|List], Trans, Call]};
        false -> continue
    end.



%%%%%%%%%%%%%%%% Published plugin callbacks %%%%%%%%%%%%%%%%%%%%%%%%%


%% @private
-spec nkcb_nksip_registrar_request_opts(nksip:request(), list()) ->
    {continue, list()}.

nkcb_nksip_registrar_request_opts(Req, List) ->
    {continue, [Req, List]}.


%% @private
-spec nkcb_nksip_registrar_request_reply(nksip:sipreply(), #reg_contact{}, list()) ->
    {continue, list()}.

nkcb_nksip_registrar_request_reply(Reply, Regs, Opts) ->
    {continue, [Reply, Regs, Opts]}.


%% @private
-spec nkcb_nksip_registrar_get_index(nksip:uri(), list()) ->
    {continue, list()}.

nkcb_nksip_registrar_get_index(Contact, Opts) ->
    {continue, [Contact, Opts]}.


%% @private
-spec nkcb_nksip_registrar_update_regcontact(#reg_contact{}, #reg_contact{}, 
                                             nksip:request(), list()) ->
    {continue, list()}.

nkcb_nksip_registrar_update_regcontact(RegContact, Base, Req, Opts) ->
    {continue, [RegContact, Base, Req, Opts]}.
