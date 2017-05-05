# Kazoo Perf maintenance


## graphite_metrics

> sup kazoo_perf_maintenance graphite_metrics {ACCOUNT} {CLUSTER} {ZONE}

```
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.memory_total 89895592 1493666091
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.memory_processes 48596136 1493666091
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.memory_processes_used 48558824 1493666091
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.memory_system 41299456 1493666091
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.memory_atom 1025209 1493666091
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.memory_atom_used 1007380 1493666091
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.memory_binary 372464 1493666091
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.memory_code 24099303 1493666091
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.memory_ets 3228752 1493666091
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.processes 1821 1493666091
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.context_switches 20307121 1493666091
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.number_of_gcs 1676447 1493666091
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.words_reclaimed 6320721010 1493666091
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.input_io_bytes 425702424 1493666091
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.output_io_bytes 502870048 1493666091
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.reductions 1802365493 1493666091
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.run_queue 7 1493666091
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.tab_200809 3 1493666091
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.tab_ecallmgr_util_cache 3 1493666091
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.tab_timer_interval_tab 1 1493666091
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.tab_timer_tab 1 1493666091
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.tab_gproc 3 1493666091
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.tab_kz_nodes 1 1493666091
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.tab_kazoo_global_names 1 1493666091
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.tab_cowboy_clock 1 1493666091
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.tab_ranch_server 16 1493666091
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.tab_hackney_pool 1 1493666091
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.tab_kz_dataconnections 1 1493666091
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.tab_49184 4 1493666091
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.tab_kazoo_data_plan_cache 3 1493666091
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.tab_40989 80 1493666091
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.tab_kazoo_data_cache 80 1493666091
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.tab_kz_amqp_history 733 1493666091
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.tab_kz_amqp_assignments 230 1493666091
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.tab_kz_amqp_connections 2 1493666091
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.tab_kazoo_bindings 519 1493666091
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.tab_24596 3 1493666091
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.tab_lager_config 4 1493666091
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.tab_inet_db 29 1493666091
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.tab_4098 188 1493666091
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.tab_1 1070 1493666091
{ACCOUNT}.{CLUSTER}.{ZONE}.termina::tor.kazoo_apps.tab_ac_tab 265 1493666091
```


## json_metrics

> sup kazoo_perf_maintenance json_metrics

```json
{
    "context_switches": {
        "context_switches": 19398483
    },
    "ets_tables_sizes": {
        "tab_1": 1070,
        "tab_200809": 3,
        "tab_24596": 3,
        "tab_4098": 188,
        "tab_40989": 80,
        "tab_49184": 4,
        "tab_ac_tab": 265,
        "tab_cowboy_clock": 1,
        "tab_ecallmgr_util_cache": 3,
        "tab_gproc": 3,
        "tab_hackney_pool": 1,
        "tab_inet_db": 29,
        "tab_kazoo_bindings": 519,
        "tab_kazoo_data_cache": 80,
        "tab_kazoo_data_plan_cache": 3,
        "tab_kazoo_global_names": 1,
        "tab_kz_amqp_assignments": 230,
        "tab_kz_amqp_connections": 2,
        "tab_kz_amqp_history": 733,
        "tab_kz_dataconnections": 1,
        "tab_kz_nodes": 1,
        "tab_lager_config": 4,
        "tab_ranch_server": 16,
        "tab_timer_interval_tab": 1,
        "tab_timer_tab": 1
    },
    "garbage_collection": {
        "number_of_gcs": 1599861,
        "words_reclaimed": 6040198844
    },
    "io": {
        "input_io_bytes": 407605438,
        "output_io_bytes": 480484464
    },
    "memory_statistics": {
        "memory_atom": 1025209,
        "memory_atom_used": 1007380,
        "memory_binary": 357328,
        "memory_code": 24099055,
        "memory_ets": 3228752,
        "memory_processes": 48387648,
        "memory_processes_used": 48350680,
        "memory_system": 41104392,
        "memory_total": 89492040
    },
    "number_of_processes": {
        "processes": 1824
    },
    "processes_in_run_queue_of_each_schedulers": {
        "run_queue": 6
    },
    "scheduler_reductions": {
        "reductions": 1723025805
    },
    "timestamp": 1493665662
}
```
