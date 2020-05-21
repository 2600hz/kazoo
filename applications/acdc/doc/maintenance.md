## SUP-able functions

| Function | Arguments | Description |
| -------- | --------- | ----------- |
| `agent_detail/2` | `(AccountId,AgentId)` | |
| `agent_login/2` | `(AccountId,AgentId)` | |
| `agent_logout/2` | `(AccountId,AgentId)` | |
| `agent_pause/2` | `(AccountId,AgentId)` | |
| `agent_pause/3` | `(AccountId,AgentId,Timeout)` | |
| `agent_presence_id/2` | `(AccountId,AgentId)` | |
| `agent_queue_login/3` | `(AccountId,AgentId,QueueId)` | |
| `agent_queue_logout/3` | `(AccountId,AgentId,QueueId)` | |
| `agent_resume/2` | `(AccountId,AgentId)` | |
| `agent_summary/2` | `(AccountId,AgentId)` | |
| `agents_detail/0` |  | |
| `agents_detail/1` | `(AccountId)` | |
| `agents_summary/0` |  | |
| `agents_summary/1` | `(AccountId)` | |
| `current_agents/1` | `(AccountId)` | |
| `current_calls/1` | `(AccountId)` | |
| `current_calls/2` | `(AccountId,Props) | (AccountId,QueueId)` | |
| `current_queues/1` | `(AccountId)` | |
| `current_statuses/1` | `(AccountId)` | |
| `flush_call_stat/1` | `(CallId)` | |
| `logout_agent/2` | `(AccountId,AgentId)` | |
| `logout_agents/1` | `(AccountId)` | |
| `migrate/0` |  | |
| `migrate_to_acdc_db/0` |  | |
| `queue_detail/2` | `(AccountId,QueueId)` | |
| `queue_restart/2` | `(AccountId,QueueId)` | |
| `queue_summary/2` | `(AccountId,QueueId)` | |
| `queues_detail/0` |  | |
| `queues_detail/1` | `(AccountId)` | |
| `queues_restart/1` | `(AccountId)` | |
| `queues_summary/0` |  | |
| `queues_summary/1` | `(AccountId)` | |
| `refresh/0` |  | |
| `refresh_account/1` | `(Account)` | |
| `register_views/0` |  | |
