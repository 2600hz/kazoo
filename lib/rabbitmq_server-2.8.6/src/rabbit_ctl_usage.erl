%% Generated, do not edit!
-module(rabbit_ctl_usage).
-export([usage/0]).
usage() -> "Usage:
rabbitmqctl [-n <node>] [-q] <command> [<command options>] 

Options:
    -n node
    -q

Default node is \"rabbit@server\", where server is the local host. On a host 
named \"server.example.com\", the node name of the RabbitMQ Erlang node will 
usually be rabbit@server (unless RABBITMQ_NODENAME has been set to some 
non-default value at broker startup time). The output of hostname -s is usually 
the correct suffix to use after the \"@\" sign. See rabbitmq-server(1) for 
details of configuring the RabbitMQ broker.

Quiet output mode is selected with the \"-q\" flag. Informational messages are 
suppressed when quiet mode is in effect.

Commands:
    stop [<pid_file>]
    stop_app
    start_app
    wait <pid_file>
    reset
    force_reset
    rotate_logs <suffix>

    join_cluster <clusternode>[<--ram>]
    cluster_status
    change_cluster_node_type disc | ram
    forget_cluster_node [--offline]
    update_cluster_nodes clusternode
    sync_queue queue
    cancel_sync_queue queue

    add_user <username> <password>
    delete_user <username>
    change_password <username> <newpassword>
    clear_password <username>
    set_user_tags <username> <tag> ...
    list_users

    add_vhost <vhostpath>
    delete_vhost <vhostpath>
    list_vhosts [<vhostinfoitem> ...]
    set_permissions [-p <vhostpath>] <user> <conf> <write> <read>
    clear_permissions [-p <vhostpath>] <username>
    list_permissions [-p <vhostpath>]
    list_user_permissions <username>

    set_parameter [-p <vhostpath>] <component_name> <name> <value>
    clear_parameter [-p <vhostpath>] <component_name> <key>
    list_parameters [-p <vhostpath>]

    set_policy [-p <vhostpath>] <name> <pattern>  <definition> [<priority>] 
    clear_policy [-p <vhostpath>] <name>
    list_policies [-p <vhostpath>]

    list_queues [-p <vhostpath>] [<queueinfoitem> ...]
    list_exchanges [-p <vhostpath>] [<exchangeinfoitem> ...]
    list_bindings [-p <vhostpath>] [<bindinginfoitem> ...]
    list_connections [<connectioninfoitem> ...]
    list_channels [<channelinfoitem> ...]
    list_consumers [-p <vhostpath>]
    status
    environment
    report
    eval <expr>

    close_connection <connectionpid> <explanation>
    trace_on [-p <vhost>]
    trace_off [-p <vhost>]
    set_vm_memory_high_watermark <fraction>

<vhostinfoitem> must be a member of the list [name, tracing].

The list_queues, list_exchanges and list_bindings commands accept an optional 
virtual host parameter for which to display results. The default value is \"/\".

<queueinfoitem> must be a member of the list [name, durable, auto_delete, 
arguments, policy, pid, owner_pid, exclusive_consumer_pid, 
exclusive_consumer_tag, messages_ready, messages_unacknowledged, messages, 
consumers, active_consumers, memory, slave_pids, synchronised_slave_pids, 
status].

<exchangeinfoitem> must be a member of the list [name, type, durable, 
auto_delete, internal, arguments, policy].

<bindinginfoitem> must be a member of the list [source_name, source_kind, 
destination_name, destination_kind, routing_key, arguments].

<connectioninfoitem> must be a member of the list [pid, name, port, host, 
peer_port, peer_host, ssl, ssl_protocol, ssl_key_exchange, ssl_cipher, 
ssl_hash, peer_cert_subject, peer_cert_issuer, peer_cert_validity, 
last_blocked_by, last_blocked_age, state, channels, protocol, auth_mechanism, 
user, vhost, timeout, frame_max, client_properties, recv_oct, recv_cnt, 
send_oct, send_cnt, send_pend].

<channelinfoitem> must be a member of the list [pid, connection, name, number, 
user, vhost, transactional, confirm, consumer_count, messages_unacknowledged, 
messages_uncommitted, acks_uncommitted, messages_unconfirmed, prefetch_count, 
client_flow_blocked].


".
