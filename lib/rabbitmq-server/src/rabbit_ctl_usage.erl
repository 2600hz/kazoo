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
    stop
    stop_app
    start_app
    status
    reset
    force_reset
    rotate_logs <suffix>

    cluster <clusternode> ...
    force_cluster <clusternode> ...

    close_connection <connectionpid> <explanation>

    add_user <username> <password>
    delete_user <username>
    change_password <username> <newpassword>
    clear_password <username>
    set_admin <username>
    clear_admin <username>
    list_users

    add_vhost <vhostpath>
    delete_vhost <vhostpath>
    list_vhosts
    set_permissions [-p <vhostpath>] <user> <conf> <write> <read>
    clear_permissions [-p <vhostpath>] <username>
    list_permissions [-p <vhostpath>]
    list_user_permissions [-p <vhostpath>] <username>

    list_queues [-p <vhostpath>] [<queueinfoitem> ...]
    list_exchanges [-p <vhostpath>] [<exchangeinfoitem> ...]
    list_bindings [-p <vhostpath>] [<bindinginfoitem> ...]
    list_connections [<connectioninfoitem> ...]
    list_channels [<channelinfoitem> ...]
    list_consumers

The list_queues, list_exchanges and list_bindings commands accept an optional 
virtual host parameter for which to display results. The default value is \"/\".

<queueinfoitem> must be a member of the list [name, durable, auto_delete, 
arguments, pid, owner_pid, exclusive_consumer_pid, exclusive_consumer_tag, 
messages_ready, messages_unacknowledged, messages, consumers, memory].

<exchangeinfoitem> must be a member of the list [name, type, durable, 
auto_delete, internal, arguments].

<bindinginfoitem> must be a member of the list [source_name, source_kind, 
destination_name, destination_kind, routing_key, arguments].

<connectioninfoitem> must be a member of the list [pid, address, port, 
peer_address, peer_port, ssl, ssl_protocol, ssl_key_exchange, ssl_cipher, 
ssl_hash, peer_cert_subject, peer_cert_issuer, peer_cert_validity, state, 
channels, protocol, auth_mechanism, user, vhost, timeout, frame_max, 
client_properties, recv_oct, recv_cnt, send_oct, send_cnt, send_pend].

<channelinfoitem> must be a member of the list [pid, connection, number, user, 
vhost, transactional, consumer_count, messages_unacknowledged, 
acks_uncommitted, prefetch_count, client_flow_blocked, confirm, 
messages_unconfirmed].

The output format for \"list_consumers\" is a list of rows containing, in 
order, the queue name, channel process id, consumer tag, and a boolean 
indicating whether acknowledgements are expected from the consumer.


".
