### Fax cleanup

This task is used to cleanup the faxes global database. This is a headless task which runs on a daily timer. Sometimes fax documents are left behind which are stale. Whenever possible these stale fax documents are moved to the appropriate account modb. When the appropriate account MODB or account associated with the document is not available, the documents are deleted from the faxes db.

Since this task only operates on docs that are stale, it does not generate any notifications when it runs.

### Final state assignment
Documents are moved to a final state prior to being copied to the MODB. If a document is missing attachments, is in a provisional state (status is: `start`, `prepare`, etc), or missing an account ID, it is set to a status of `failed`. If an inbound fax is still in a provisional state but the attachment is present, it is set to a `completed` state.

#### Configuration

All configuration are in `system_config/tasks.fax_cleanup`.

Key | Description | Type | Default
--- | ----------- | ---- | -------
`enabled` | Should the cluster run the fax cleanup task | `boolean` | `false`
`page_size` | The number of documents to process in one fetch operation | `integer` | `32`
`per_page_pause_ms` | How many milliseconds to pause between chunk fetches | `integer` | `1000`
`stale_after_s` | The number of seconds old a document is before it is considered to be stale | `integer` | `604800`

#### Logs

The task currently logs status and changes it makes to the kazoo log file. Logs can be gathered by grepping for `kt_fax_cleanup`.

### Fill DB script

A python script was written to test this. The script fills the faxes db of the target couchdb with dummy fax docs containing randomized dates and other values.

This script can be found in [community-scripts](https://github.com/2600hz/community-scripts/tree/master/fill_fax_db).


