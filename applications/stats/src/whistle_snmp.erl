%%%-------------------------------------------------------------------
%%% @author Stephen Gibberd <stephen.gibberd@2600hz.com>
%%% Created :  3 Jul 2013 by Stephen Gibberd <stephen.gibberd@2600hz.com>
%%% Implementation functions for SNMP support 
%%%-------------------------------------------------------------------
-module(whistle_snmp).

%% API
-export([
	 create_config/0
	 ,start/0
	 ,kazoo_ver/1
	 ,get_oid/4]).

%%% get_oid - implementation function to read from a table. 
-spec get_oid(get_next|get,integer(),integer(),term) -> any.
get_oid('get_next',RowIndex,Cols,Table) ->
    lager:debug("Table: ~p Row: ~p Cols: ~p~n",[Table,RowIndex, Cols]),
    Value = stats_handler:get_next(Table,RowIndex,Cols),
    lager:debug("Result ~p~n",[Value]),
    Value.
    
-spec kazoo_ver(get) -> {'value',string()}.
kazoo_ver('get') ->
    {'value', wh_util:to_list(wh_util:whistle_version())}.

%% @public
%% @doc
%% Create the directories and agent configuration files
%% @end

-spec start() -> ok.
start() ->
    application:start(snmp),
    lager:start(),
    snmpa:load_mibs(["KAZOO-MIB"]),
    'ok'.

%% Creates the directories and configuration files needs to start snmp. 
%% The important settings are the port and community string. It supports
%% SNMP version 2c. To test, try 
%% snmpwalk -v 2c -c public kazoo:4000 1.3.6.1.4.1.700001
-spec create_config() -> 'ok'.
create_config() ->
    {'ok',CWD} = file:get_cwd(),
    ADir = CWD ++ "/agent/conf",
    filelib:ensure_dir(ADir ++ "/agent"),
    DDir = CWD ++ "/db",
    filelib:ensure_dir(DDir ++ "/db"),
    write_conf('agent_entry','write_agent_config',ADir,
	       [['intAgentUDPPort', 4000]
		,['snmpEngineID',"Kazoo agent"]
		,['snmpEngineMaxMessageSize', 484]
		,['intAgentIpAddress',[0,0,0,0]]
	       ]),
    write_conf('standard_entry','write_standard_config',ADir,
	       [['sysDescr',"Erlang/OTP Agent"]
		,['sysName',"Kazoo agent"]
		,['sysContact',"info@2600hz.com"]
		,['sysLocation',"US"]
		,['sysObjectID',[3,6,1,4,1,193,19]]
		,['sysServices', 72]
		,['snmpEnableAuthenTraps', 'disabled']]),
    write_conf('community_entry','write_community_config',ADir,
	       [["public", "public", "initial", "", ""]]),
    write_conf('vacm_s2g_entry','write_vacm_config',ADir,
	       [['v2c',"initial","initial"],['usm',"initial","initial"]]),
    write_conf('vacm_acc_entry','append_vacm_config',ADir,
	       [["initial", "", 'any', 'noAuthNoPriv', 'exact', "restricted", 
		 "", "restricted"],
		["initial", "", 'usm', 'authNoPriv', 'exact', "internet", 
		 "internet", "internet"],
		["initial", "", 'usm', 'authPriv', 'exact', "internet", 
		 "internet", "internet"]]),
    write_conf('vacm_vtf_entry','append_vacm_config',ADir,
	       [["internet", [1,3,6,1], 'included', 'null'],
		["restricted", [1,3,6,1], 'included', 'null']]),

%%% SNMP Manager not needed yet.    
%%%   MDir = CWD ++ "/manager/conf",
%%%   filelib:ensure_dir(MDir ++ "/manager"),
%%%   MDDir = CWD ++ "/db",
%%%   filelib:ensure_dir(MDDir ++ "/db"),
%%%   write_conf(snmpm_conf,manager_entry,write_manager_config,MDir,%
%%%	       [[address,[0,0,0,0]],[port,5000],[engine_id,"Kazoo engine"],
%%%		[max_message_size, 484]
%%%	       ]),
%%%    write_conf(snmpm_conf,users_entry,write_users_config,MDir,%
%%%	       [ [simple_user] ]),
%%%    write_conf(snmpm_conf,agents_entry,write_agents_config,MDir,
%%%	       [[simple_user, "otp agent", "public", [127,0,0,1], 4000, 
%%%		 "agent's engine",infinity, 484, v2, v2c, "initial", 
%%%		 noAuthNoPriv]]),

    {ok,File} = file:open("snmp.config",['write']),
    Snmp = [{snmp,[{agent,
		    [{config, [{dir, ADir}]},
		     {db_dir, DDir}]}
%		   ,{manager,
%		    [{config, [{dir, MDir},
%			       {db_dir, MDDir}]}]}]}],
		   ]}],
    lager:debug(File,"~p.",[Snmp]),
    file:close(File),
    ok.
    
write_conf(EFun,WFun,Dir,EList) ->
    write_conf(snmpa_conf,EFun,WFun,Dir,EList).

write_conf(EMod,EFun,WFun,Dir,EList) ->
    Entries = [erlang:apply(EMod,EFun,Parm) || Parm <- EList],
    case erlang:function_exported(EMod,WFun,3) of
	true ->
	    lager:debug("Writing to ~s ~s~n~p~n",[WFun,Dir,Entries]),
	    EMod:WFun(Dir,"%%% Generated config",Entries);
	false ->
	    EMod:WFun(Dir,Entries)
    end.

