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

%% @doc NkSIP Event State Compositor Plugin User Callbacks
-module(nksip_event_compositor_sipapp).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([sip_event_compositor_store/2]).


% @doc Called when a operation database must be done on the compositor database.
%% This default implementation uses the built-in memory database.
-spec sip_event_compositor_store(StoreOp, AppId) ->
    [RegPublish] | ok | not_found when
        StoreOp :: {get, AOR, Tag} | {put, AOR, Tag, RegPublish, TTL} | 
                   {del, AOR, Tag} | del_all,
        AppId :: nksip:app_id(),
        AOR :: nksip:aor(),
        Tag :: binary(),
        RegPublish :: nksip_event_compositor:reg_publish(),
        TTL :: integer().

sip_event_compositor_store(Op, AppId) ->
    case Op of
        {get, AOR, Tag} ->
            nksip_store:get({nksip_event_compositor, AppId, AOR, Tag}, not_found);
        {put, AOR, Tag, Record, TTL} -> 
            nksip_store:put({nksip_event_compositor, AppId, AOR, Tag}, Record, [{ttl, TTL}]);
        {del, AOR, Tag} ->
            nksip_store:del({nksip_event_compositor, AppId, AOR, Tag});
        del_all ->
            FoldFun = fun(Key, _Value, Acc) ->
                case Key of
                    {nksip_event_compositor, AppId, AOR, Tag} -> 
                        nksip_store:del({nksip_event_compositor, AppId, AOR, Tag});
                    _ -> 
                        Acc
                end
            end,
            nksip_store:fold(FoldFun, none)
    end.
