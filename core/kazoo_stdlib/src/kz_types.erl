%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_types).

-include_lib("xmerl/include/xmerl.hrl").
-include_lib("kazoo_stdlib/include/kz_records.hrl").

%% when using gen_smtp to send emails, it takes a 5-tuple for a message-body part
-type mail_message_body() :: {kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist(), kz_term:proplist(), kz_term:ne_binary() | iolist()}.

%% for setting types on dicts
-type dict(K,V) :: [{K, V}].

-type ip_list() :: kz_term:ne_binaries().

%% Recreate the non-exported types defined in the Erlang supervisor source
-type sup_child_spec() :: supervisor:child_spec().
-type sup_child_specs() :: [sup_child_spec()].
-type sup_start_flags() :: supervisor:sup_flags().

-type sup_init_ret() :: {'ok', {supervisor:sup_flags(), [supervisor:child_spec()]}} |
                        'ignore'.

-type sup_child_id() :: kz_term:api_pid().
-type sup_startchild_err() :: 'already_present' |
                              {'already_started', sup_child_id()} |
                              any().
-type sup_startchild_ret() :: {'ok', sup_child_id()} |
                              {'ok', sup_child_id(), any()} |
                              {'error', sup_startchild_err()}.
-type sup_deletechild_err() :: 'running' | 'restarting' | 'not_found' |
                               'simple_one_for_one'.
-type sup_deletechild_ret() :: 'ok' | {'error', sup_deletechild_err()}.

%% Recreate the non-exported types defined in the Erlang gen_server source
-type startlink_err() :: {'already_started', pid()} |
                         'shutdown' |
                         any().
-type startlink_ret() :: {'ok', pid()} |
                         'ignore' |
                         {'error', startlink_err()}.
-type startapp_ret() :: {'ok', pid()} |
                        {'ok', pid(), any()} |
                        {'error', startlink_err()}.

-type call_from() :: kz_term:pid_ref().
-type gen_server_timeout() :: 'hibernate' | non_neg_integer().
-type handle_call_ret() :: {'reply', any(), any()} |
                           {'reply', any(), any(), gen_server_timeout()} |
                           {'noreply', any()} |
                           {'noreply', any(), gen_server_timeout()} |
                           {'stop', any(), any()} |
                           {'stop', any(), any(), any()}.

-type handle_call_ret_state(State) :: {'reply', any(), State} |
                                      {'reply', any(), State, gen_server_timeout()} |
                                      {'noreply', State} |
                                      {'noreply', State, gen_server_timeout()} |
                                      {'stop', any(), State} |
                                      {'stop', any(), State, any()}.

-type handle_cast_ret() :: {'noreply', any()} |
                           {'noreply', any(), gen_server_timeout()} |
                           {'stop', any(), any()}.
-type handle_cast_ret_state(State) :: {'noreply', State} |
                                      {'noreply', State, gen_server_timeout()} |
                                      {'stop', any(), State}.

-type handle_info_ret() :: {'noreply', any()} |
                           {'noreply', any(), gen_server_timeout()} |
                           {'stop', any(), any()}.
-type handle_info_ret_state(State) :: {'noreply', State} |
                                      {'noreply', State, gen_server_timeout()} |
                                      {'stop', any(), State}.

-type handle_fsm_ret(State) :: {'next_state', atom(), State} |
                               {'next_state', atom(), State, timeout() | 'hibernate'} |
                               {'stop', any(), State}.

-type handle_sync_event_ret(State) :: handle_fsm_ret(State) |
                                      {'reply', any(), atom(), State} |
                                      {'reply', any(), atom(), State, timeout() | 'hibernate'} |
                                      {'stop', any(), any(), State}.

-type server_ref() :: atom() |
                      {atom(), atom()} |
                      {'global', any()} |
                      {'via', atom(), any()} |
                      pid().

-type gen_server_name() :: {'local', atom()} |
                           {'global', any()} |
                           {'via', atom(), any()}.
-type gen_server_option() :: {'debug', list()} |
                             {'timeout', non_neg_integer()} |
                             {'spawn_opt', list()}.
-type gen_server_options() :: [gen_server_option()].


%% XML types
-type xml_attrib_name() :: atom().
-type xml_attrib_value() :: kz_term:ne_binary() | nonempty_string() | iolist() | atom() | number().
-type xml_attrib() :: #xmlAttribute{}.
-type xml_attribs() :: [xml_attrib()].

-type xml_el() :: #xmlElement{}.
-type xml_els() :: [xml_el()].

-type xml_text() :: #xmlText{value :: iolist()}.
-type xml_texts() :: [xml_text()].


-type xml_thing() :: xml_el() | xml_text().
-type xml_things() :: xml_els() | xml_texts().

%% KZ_NODES types

-type whapp_info() :: #whapp_info{}.
-type kapps_info() :: [{binary(), whapp_info()}].

-type media_server() :: {kz_term:ne_binary(), kz_json:object()}.
-type media_servers() :: [media_server()].

-type kz_node() :: #kz_node{}.
-type kz_nodes() :: [kz_node()].

-export_type([mail_message_body/0
             ,dict/2
             ,ip_list/0
             ,sup_child_spec/0
             ,sup_child_specs/0
             ,sup_start_flags/0
             ,sup_init_ret/0
             ,sup_child_id/0
             ,sup_startchild_err/0
             ,sup_startchild_ret/0
             ,sup_deletechild_err/0
             ,sup_deletechild_ret/0
             ,startlink_err/0
             ,startlink_ret/0
             ,startapp_ret/0
             ,call_from/0
             ,gen_server_timeout/0
             ,handle_call_ret/0
             ,handle_call_ret_state/1
             ,handle_cast_ret/0
             ,handle_cast_ret_state/1
             ,handle_info_ret/0
             ,handle_info_ret_state/1
             ,handle_fsm_ret/1
             ,handle_sync_event_ret/1
             ,server_ref/0
             ,gen_server_name/0
             ,gen_server_option/0
             ,gen_server_options/0
             ,xml_attrib_name/0
             ,xml_attrib_value/0
             ,xml_attrib/0
             ,xml_attribs/0
             ,xml_el/0
             ,xml_els/0
             ,xml_text/0
             ,xml_texts/0
             ,xml_thing/0
             ,xml_things/0
             ,whapp_info/0
             ,kapps_info/0
             ,media_server/0
             ,media_servers/0
             ,kz_node/0
             ,kz_nodes/0
             ]).
