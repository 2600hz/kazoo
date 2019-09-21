## SUP-able functions

| Function | Arguments | Description |
| -------- | --------- | ----------- |
| `account_jobs/1` | `(AccountId)` | |
| `account_jobs/2` | `(AccountId,State)` | |
| `active_jobs/0` |  | |
| `faxbox_jobs/1` | `(FaxboxId)` | |
| `faxbox_jobs/2` | `(FaxboxId,State)` | |
| `flush/0` |  | |
| `load_smtp_attachment/2` | `(DocId,Filename)` | |
| `migrate/0` |  | |
| `migrate/1` | `(Account) | ([]) | (_)` | |
| `migrate/2` | `(Account,Options) | (Accounts,Option) | (Accounts,_) | ([],_) | (_,Options)` | |
| `migrate_outbound_faxes/0` |  | |
| `migrate_outbound_faxes/1` | `(Number) | (Options)` | |
| `pending_jobs/0` |  | |
| `refresh_views/0` |  | |
| `restart_job/1` | `(JobID)` | |
| `update_job/2` | `(JobID,State)` | |
