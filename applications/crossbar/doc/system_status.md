# System Status

## About System Status

Helps to check cluster's health over crossbar

!!! note
    You must be `superduper_admin` to access this resource. Also make sure that the `system_status` crossbar module has been enabled. `sup crossbar_maintenance start_module cb_system_status`.

## Schema



## Fetch

> GET /v2/system_status

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/system_status
```

**Response**

2 zones (area7x and area5x) with 4 x kazoo_apps and 2 x kamailio nodes each can be seen.

```json
{
  "data": {
    "area7x": {
      "kazoo_apps": {
        "kazoo_apps@kz736.cluster.tld": {
          "node": "kazoo_apps@kz736.cluster.tld",
          "md5": "Wkh5QArMZBHy7sUpm57_4A",
          "version": "4.3.15 - 19",
          "used_memory": "67.46MB",
          "processes": 1658,
          "ports": 21,
          "zone": "area7x",
          "broker": "amqp://7.7.7.40:5672",
          "kapps": [
            "crossbar",
            "blackhole"
          ],
          "globals": {
            "remote": 4
          },
          "node_info": {
            "kz_amqp_pool": "150/0/0 (ready)"
          }
        },
        "kazoo_apps@kz735.cluster.tld": {
          "node": "kazoo_apps@kz735.cluster.tld",
          "md5": "3G3bhlSmI8iAmHdvT7lnTQ",
          "version": "4.3.15 - 19",
          "used_memory": "108.58MB",
          "processes": 1804,
          "ports": 24,
          "zone": "area7x",
          "broker": "amqp://7.7.7.40:5672",
          "kapps": [
            "callflow",
            "crossbar",
            "acdc",
            "blackhole"
          ],
          "globals": {
            "total": 0
          },
          "node_info": {
            "kz_amqp_pool": "150/0/0 (ready)"
          }
        },
        "kazoo_apps@kz734.cluster.tld": {
          "node": "kazoo_apps@kz734.cluster.tld",
          "md5": "_tjOTtd5C7938BPVnaQLXA",
          "version": "4.3.15 - 19",
          "used_memory": "130.53MB",
          "processes": 2389,
          "ports": 50,
          "zone": "area7x",
          "broker": "amqp://7.7.7.40:5672",
          "kapps": [
            "sysconf",
            "crossbar",
            "braintree",
            "cccp",
            "frontier",
            "hotornot",
            "jonny5",
            "konami",
            "spyvsspy",
            "tasks",
            "blackhole",
            "callflow",
            "cdr",
            "conference",
            "ecallmgr",
            "fax",
            "hangups",
            "media_mgr",
            "milliwatt",
            "omnipresence",
            "pivot",
            "registrar",
            "reorder",
            "stepswitch",
            "teletype",
            "trunkstore",
            "webhooks"
          ],
          "globals": {
            "remote": 4
          },
          "node_info": {
            "kz_amqp_pool": "150/0/0 (ready)"
          },
          "media_servers": [
            "freeswitch@kz738.cluster.tld",
            "freeswitch@kz739.cluster.tld"
          ],
          "registrations": 374
        },
        "kazoo_apps@kz733.cluster.tld": {
          "node": "kazoo_apps@kz733.cluster.tld",
          "md5": "9OOO3pQqL4i6C13iSE6c_w",
          "version": "4.3.15 - 19",
          "used_memory": "129.52MB",
          "processes": 2390,
          "ports": 51,
          "zone": "area7x",
          "broker": "amqp://7.7.7.40:5672",
          "kapps": [
            "sysconf",
            "crossbar",
            "braintree",
            "cccp",
            "frontier",
            "hotornot",
            "jonny5",
            "konami",
            "spyvsspy",
            "tasks",
            "blackhole",
            "callflow",
            "cdr",
            "conference",
            "ecallmgr",
            "fax",
            "hangups",
            "media_mgr",
            "milliwatt",
            "omnipresence",
            "pivot",
            "registrar",
            "reorder",
            "stepswitch",
            "teletype",
            "trunkstore",
            "webhooks"
          ],
          "globals": {
            "remote": 4
          },
          "node_info": {
            "kz_amqp_pool": "150/0/0 (ready)"
          },
          "media_servers": [
            "freeswitch@kz739.cluster.tld",
            "freeswitch@kz738.cluster.tld"
          ],
          "registrations": 374
        }
      },
      "kamailio": {
        "kamailio@kz732.cluster.tld": {
          "node": "kamailio@kz732.cluster.tld",
          "version": "5.1.5",
          "used_memory": "34.80MB",
          "zone": "area7x",
          "broker": "amqp://7.7.7.40:5672",
          "kapps": [
            "kamailio"
          ],
          "roles": {
            "Proxy": {
              "Listeners": {
                "udp:7.7.7.32:5060": {
                  "proto": "udp",
                  "address": "7.7.7.32",
                  "port": 5060
                },
                "udp:7.7.7.32:7000": {
                  "proto": "udp",
                  "address": "7.7.7.32",
                  "port": 7000
                },
                "udp:7.7.7.32:5064": {
                  "proto": "udp",
                  "address": "7.7.7.32",
                  "port": 5064
                },
                "udp:7.7.7.32:5065": {
                  "proto": "udp",
                  "address": "7.7.7.32",
                  "port": 5065
                },
                "tcp:7.7.7.32:5060": {
                  "proto": "tcp",
                  "address": "7.7.7.32",
                  "port": 5060
                },
                "tcp:7.7.7.32:7000": {
                  "proto": "tcp",
                  "address": "7.7.7.32",
                  "port": 7000
                },
                "tcp:7.7.7.32:5064": {
                  "proto": "tcp",
                  "address": "7.7.7.32",
                  "port": 5064
                },
                "tls:7.7.7.32:5065": {
                  "proto": "tls",
                  "address": "7.7.7.32",
                  "port": 5065
                },
                "tls:7.7.7.32:5061": {
                  "proto": "tls",
                  "address": "7.7.7.32",
                  "port": 5061
                },
                "tls:7.7.7.32:7001": {
                  "proto": "tls",
                  "address": "7.7.7.32",
                  "port": 7001
                }
              }
            },
            "Dispatcher": {
              "Groups": {
                "1": {
                  "sip:7.7.7.39:11000": {
                    "destination": "sip:7.7.7.39:11000",
                    "flags": "AP",
                    "priority": 0,
                    "attrs": ""
                  },
                  "sip:7.7.7.38:11000": {
                    "destination": "sip:7.7.7.38:11000",
                    "flags": "AP",
                    "priority": 0,
                    "attrs": ""
                  }
                },
                "2": {
                  "sip:5.5.5.39:11000": {
                    "destination": "sip:5.5.5.39:11000",
                    "flags": "AP",
                    "priority": 0,
                    "attrs": ""
                  },
                  "sip:5.5.5.38:11000": {
                    "destination": "sip:5.5.5.38:11000",
                    "flags": "IP",
                    "priority": 0,
                    "attrs": ""
                  }
                }
              }
            },
            "Presence": {
              "Subscribers": {},
              "Subscriptions": {},
              "Presentities": {
                "message-summary": 0,
                "dialog": 0,
                "presence": 0
              }
            },
            "Registrar": {
              "Registrations": 0
            }
          }
        },
        "kamailio@kz731.cluster.tld": {
          "node": "kamailio@kz731.cluster.tld",
          "version": "5.1.5",
          "used_memory": "34.66MB",
          "zone": "area7x",
          "broker": "amqp://7.7.7.40:5672",
          "kapps": [
            "kamailio"
          ],
          "roles": {
            "Proxy": {
              "Listeners": {
                "udp:7.7.7.31:5060": {
                  "proto": "udp",
                  "address": "7.7.7.31",
                  "port": 5060
                },
                "udp:7.7.7.31:7000": {
                  "proto": "udp",
                  "address": "7.7.7.31",
                  "port": 7000
                },
                "udp:7.7.7.31:5064": {
                  "proto": "udp",
                  "address": "7.7.7.31",
                  "port": 5064
                },
                "udp:7.7.7.31:5065": {
                  "proto": "udp",
                  "address": "7.7.7.31",
                  "port": 5065
                },
                "tcp:7.7.7.31:5060": {
                  "proto": "tcp",
                  "address": "7.7.7.31",
                  "port": 5060
                },
                "tcp:7.7.7.31:7000": {
                  "proto": "tcp",
                  "address": "7.7.7.31",
                  "port": 7000
                },
                "tcp:7.7.7.31:5064": {
                  "proto": "tcp",
                  "address": "7.7.7.31",
                  "port": 5064
                },
                "tls:7.7.7.31:5065": {
                  "proto": "tls",
                  "address": "7.7.7.31",
                  "port": 5065
                },
                "tls:7.7.7.31:5061": {
                  "proto": "tls",
                  "address": "7.7.7.31",
                  "port": 5061
                },
                "tls:7.7.7.31:7001": {
                  "proto": "tls",
                  "address": "7.7.7.31",
                  "port": 7001
                }
              }
            },
            "Dispatcher": {
              "Groups": {
                "1": {
                  "sip:7.7.7.39:11000": {
                    "destination": "sip:7.7.7.39:11000",
                    "flags": "AP",
                    "priority": 0,
                    "attrs": ""
                  },
                  "sip:7.7.7.38:11000": {
                    "destination": "sip:7.7.7.38:11000",
                    "flags": "AP",
                    "priority": 0,
                    "attrs": ""
                  }
                },
                "2": {
                  "sip:5.5.5.39:11000": {
                    "destination": "sip:5.5.5.39:11000",
                    "flags": "AP",
                    "priority": 0,
                    "attrs": ""
                  },
                  "sip:5.5.5.38:11000": {
                    "destination": "sip:5.5.5.38:11000",
                    "flags": "IP",
                    "priority": 0,
                    "attrs": ""
                  }
                }
              }
            },
            "Presence": {
              "Subscribers": {
                "dialog": 1
              },
              "Subscriptions": {
                "dialog": 2
              },
              "Presentities": {
                "message-summary": 0,
                "dialog": 0,
                "presence": 0
              }
            },
            "Registrar": {
              "Registrations": 1
            }
          }
        }
      }
    },
    "area5x": {
      "kazoo_apps": {
        "kazoo_apps@kz536.cluster.tld": {
          "node": "kazoo_apps@kz536.cluster.tld",
          "md5": "NPB0Fjlg6kjgURZ0FRY75Q",
          "version": "4.3.15 - 19",
          "used_memory": "137.59MB",
          "processes": 2605,
          "ports": 54,
          "zone": "area5x",
          "broker": "amqp://5.5.5.40:5672",
          "kapps": [
            "sysconf",
            "crossbar",
            "braintree",
            "cccp",
            "frontier",
            "hotornot",
            "jonny5",
            "konami",
            "spyvsspy",
            "tasks",
            "blackhole",
            "callflow",
            "cdr",
            "conference",
            "ecallmgr",
            "fax",
            "hangups",
            "media_mgr",
            "milliwatt",
            "omnipresence",
            "pivot",
            "registrar",
            "reorder",
            "stepswitch",
            "teletype",
            "trunkstore",
            "webhooks",
            "acdc"
          ],
          "globals": {
            "remote": 4
          },
          "node_info": {
            "kz_amqp_pool": "150/0/0 (ready)"
          },
          "media_servers": [
            "freeswitch@kz539.cluster.tld",
            "freeswitch@kz538.cluster.tld"
          ],
          "channels": 7,
          "registrations": 374
        },
        "kazoo_apps@kz535.cluster.tld": {
          "node": "kazoo_apps@kz535.cluster.tld",
          "md5": "nqpKUahBnNAuYketfUI-bQ",
          "version": "4.3.15 - 19",
          "used_memory": "140.08MB",
          "processes": 2627,
          "ports": 53,
          "zone": "area5x",
          "broker": "amqp://5.5.5.40:5672",
          "kapps": [
            "sysconf",
            "crossbar",
            "braintree",
            "cccp",
            "frontier",
            "hotornot",
            "jonny5",
            "konami",
            "spyvsspy",
            "tasks",
            "blackhole",
            "callflow",
            "cdr",
            "conference",
            "ecallmgr",
            "fax",
            "hangups",
            "media_mgr",
            "milliwatt",
            "omnipresence",
            "pivot",
            "registrar",
            "reorder",
            "stepswitch",
            "teletype",
            "trunkstore",
            "webhooks",
            "acdc"
          ],
          "globals": {
            "remote": 4
          },
          "node_info": {
            "kz_amqp_pool": "150/0/0 (ready)"
          },
          "media_servers": [
            "freeswitch@kz539.cluster.tld",
            "freeswitch@kz538.cluster.tld"
          ],
          "channels": 8,
          "registrations": 374
        },
        "kazoo_apps@kz534.cluster.tld": {
          "node": "kazoo_apps@kz534.cluster.tld",
          "md5": "8WRwH7N8noMyhmKp3h3Q1Q",
          "version": "4.3.15 - 19",
          "used_memory": "144.12MB",
          "processes": 2641,
          "ports": 53,
          "zone": "area5x",
          "broker": "amqp://5.5.5.40:5672",
          "kapps": [
            "sysconf",
            "crossbar",
            "braintree",
            "cccp",
            "frontier",
            "hotornot",
            "jonny5",
            "konami",
            "spyvsspy",
            "tasks",
            "blackhole",
            "callflow",
            "cdr",
            "conference",
            "ecallmgr",
            "fax",
            "hangups",
            "media_mgr",
            "milliwatt",
            "omnipresence",
            "pivot",
            "registrar",
            "reorder",
            "stepswitch",
            "teletype",
            "trunkstore",
            "webhooks",
            "acdc"
          ],
          "globals": {
            "local": 4
          },
          "node_info": {
            "kz_amqp_pool": "150/0/0 (ready)"
          },
          "media_servers": [
            "freeswitch@kz539.cluster.tld",
            "freeswitch@kz538.cluster.tld"
          ],
          "channels": 8,
          "registrations": 374
        },
        "kazoo_apps@kz533.cluster.tld": {
          "node": "kazoo_apps@kz533.cluster.tld",
          "md5": "kNeGNV-VHY5Z-GvUNvA3_w",
          "version": "4.3.15 - 19",
          "used_memory": "141.61MB",
          "processes": 2621,
          "ports": 53,
          "zone": "area5x",
          "broker": "amqp://5.5.5.40:5672",
          "kapps": [
            "sysconf",
            "crossbar",
            "braintree",
            "cccp",
            "frontier",
            "hotornot",
            "jonny5",
            "konami",
            "spyvsspy",
            "tasks",
            "blackhole",
            "callflow",
            "cdr",
            "conference",
            "ecallmgr",
            "fax",
            "hangups",
            "media_mgr",
            "milliwatt",
            "omnipresence",
            "pivot",
            "registrar",
            "reorder",
            "stepswitch",
            "teletype",
            "trunkstore",
            "webhooks",
            "acdc"
          ],
          "globals": {
            "remote": 4
          },
          "node_info": {
            "kz_amqp_pool": "150/0/0 (ready)"
          },
          "media_servers": [
            "freeswitch@kz539.cluster.tld",
            "freeswitch@kz538.cluster.tld"
          ],
          "channels": 8,
          "registrations": 374
        }
      },
      "kamailio": {
        "kamailio@kz532.cluster.tld": {
          "node": "kamailio@kz532.cluster.tld",
          "version": "5.1.5",
          "used_memory": "41.09MB",
          "zone": "area5x",
          "broker": "amqp://5.5.5.40:5672",
          "kapps": [
            "kamailio"
          ],
          "roles": {
            "Proxy": {
              "Listeners": {
                "udp:5.5.5.32:5060": {
                  "proto": "udp",
                  "address": "5.5.5.32",
                  "port": 5060
                },
                "udp:5.5.5.32:7000": {
                  "proto": "udp",
                  "address": "5.5.5.32",
                  "port": 7000
                },
                "udp:5.5.5.32:5064": {
                  "proto": "udp",
                  "address": "5.5.5.32",
                  "port": 5064
                },
                "udp:5.5.5.32:5065": {
                  "proto": "udp",
                  "address": "5.5.5.32",
                  "port": 5065
                },
                "tcp:5.5.5.32:5060": {
                  "proto": "tcp",
                  "address": "5.5.5.32",
                  "port": 5060
                },
                "tcp:5.5.5.32:7000": {
                  "proto": "tcp",
                  "address": "5.5.5.32",
                  "port": 7000
                },
                "tcp:5.5.5.32:5064": {
                  "proto": "tcp",
                  "address": "5.5.5.32",
                  "port": 5064
                },
                "tls:5.5.5.32:5065": {
                  "proto": "tls",
                  "address": "5.5.5.32",
                  "port": 5065
                },
                "tls:5.5.5.32:5061": {
                  "proto": "tls",
                  "address": "5.5.5.32",
                  "port": 5061
                },
                "tls:5.5.5.32:7001": {
                  "proto": "tls",
                  "address": "5.5.5.32",
                  "port": 7001
                }
              }
            },
            "Dispatcher": {
              "Groups": {
                "1": {
                  "sip:5.5.5.39:11000": {
                    "destination": "sip:5.5.5.39:11000",
                    "flags": "AP",
                    "priority": 0,
                    "attrs": ""
                  },
                  "sip:5.5.5.38:11000": {
                    "destination": "sip:5.5.5.38:11000",
                    "flags": "IP",
                    "priority": 0,
                    "attrs": ""
                  }
                },
                "2": {
                  "sip:7.7.7.39:11000": {
                    "destination": "sip:7.7.7.39:11000",
                    "flags": "AP",
                    "priority": 0,
                    "attrs": ""
                  },
                  "sip:7.7.7.38:11000": {
                    "destination": "sip:7.7.7.38:11000",
                    "flags": "AP",
                    "priority": 0,
                    "attrs": ""
                  }
                }
              }
            },
            "Presence": {
              "Subscribers": {},
              "Subscriptions": {},
              "Presentities": {
                "message-summary": 0,
                "dialog": 0,
                "presence": 0
              }
            },
            "Registrar": {
              "Registrations": 204
            }
          }
        },
        "kamailio@kz531.cluster.tld": {
          "node": "kamailio@kz531.cluster.tld",
          "version": "5.1.5",
          "used_memory": "37.91MB",
          "zone": "area5x",
          "broker": "amqp://5.5.5.40:5672",
          "kapps": [
            "kamailio"
          ],
          "roles": {
            "Proxy": {
              "Listeners": {
                "udp:5.5.5.31:5060": {
                  "proto": "udp",
                  "address": "5.5.5.31",
                  "port": 5060
                },
                "udp:5.5.5.31:7000": {
                  "proto": "udp",
                  "address": "5.5.5.31",
                  "port": 7000
                },
                "udp:5.5.5.31:5064": {
                  "proto": "udp",
                  "address": "5.5.5.31",
                  "port": 5064
                },
                "udp:5.5.5.31:5065": {
                  "proto": "udp",
                  "address": "5.5.5.31",
                  "port": 5065
                },
                "tcp:5.5.5.31:5060": {
                  "proto": "tcp",
                  "address": "5.5.5.31",
                  "port": 5060
                },
                "tcp:5.5.5.31:7000": {
                  "proto": "tcp",
                  "address": "5.5.5.31",
                  "port": 7000
                },
                "tcp:5.5.5.31:5064": {
                  "proto": "tcp",
                  "address": "5.5.5.31",
                  "port": 5064
                },
                "tls:5.5.5.31:5065": {
                  "proto": "tls",
                  "address": "5.5.5.31",
                  "port": 5065
                },
                "tls:5.5.5.31:5061": {
                  "proto": "tls",
                  "address": "5.5.5.31",
                  "port": 5061
                },
                "tls:5.5.5.31:7001": {
                  "proto": "tls",
                  "address": "5.5.5.31",
                  "port": 7001
                }
              }
            },
            "Dispatcher": {
              "Groups": {
                "1": {
                  "sip:5.5.5.39:11000": {
                    "destination": "sip:5.5.5.39:11000",
                    "flags": "AP",
                    "priority": 0,
                    "attrs": ""
                  },
                  "sip:5.5.5.38:11000": {
                    "destination": "sip:5.5.5.38:11000",
                    "flags": "IP",
                    "priority": 0,
                    "attrs": ""
                  }
                },
                "2": {
                  "sip:7.7.7.39:11000": {
                    "destination": "sip:7.7.7.39:11000",
                    "flags": "AP",
                    "priority": 0,
                    "attrs": ""
                  },
                  "sip:7.7.7.38:11000": {
                    "destination": "sip:7.7.7.38:11000",
                    "flags": "AP",
                    "priority": 0,
                    "attrs": ""
                  }
                }
              }
            },
            "Presence": {
              "Subscribers": {
                "dialog": 4,
                "message-summary": 3
              },
              "Subscriptions": {
                "dialog": 22,
                "message-summary": 4
              },
              "Presentities": {
                "message-summary": 0,
                "dialog": 0,
                "presence": 0
              }
            },
            "Registrar": {
              "Registrations": 181
            }
          }
        }
      }
    }
  },
  "timestamp": "2019-03-19T17:43:36Z",
  "version": "4.3.15",
  "node": "3G3bhlSmI8iAmHdvT7lnTQ",
  "request_id": "01e5f9c3b4352a90d60dfb5b0e9e78bc",
  "status": "success"
}
```
