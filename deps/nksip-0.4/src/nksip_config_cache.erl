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

%% @private NkSIP Config Cache
%%
%%
%% This module is hot compiled in run-time, after NkSIP application has started.
%% It maintains a number of functions to cache some parts of the configuration.

-module(nksip_config_cache).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-compile([export_all]).


%% This functions will never be called

global_id() -> nksip_config:get(global_id).

local_ips() -> nksip_config:get(local_ips).

main_ip() -> nksip_config:get(main_ip).

main_ip6() -> nksip_config:get(main_ip6).

sync_call_time() -> nksip_config:get(sync_call_time). 

msg_routers() -> nksip_config:get(msg_routers). 

dns_cache_ttl() -> nksip_config:get(dns_cache_ttl).

local_data_path() -> nksip_config:get(local_data_path).

global_max_connections() -> nksip_config:get(global_max_connections).

global_max_calls() -> nksip_config:get(global_max_calls).

app_config() -> nksip_config:get(app_config).

re_call_id() -> nksip_config:get(re_call_id). 

re_content_length() -> nksip_config:get(re_content_length). 

