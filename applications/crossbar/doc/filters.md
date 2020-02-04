# Query String Filters

## Overview

Query string filters allow the API results to be filtered by additional criteria to limit the result set. This is especially useful when querying a collection that could be massive (like CDRs) but you're only interested in results that match certain criteria.

## Available Filters

Filter | Operates On | Description
------ | ----------- | -----------
`filter_not_{KEY}` | `{VALUE}` | Doc include if `{KEY}` is *not* `{VALUE}`
`filter_{KEY}` | `{VALUE}` | Doc included if `{KEY}` is `{VALUE}`
`has_key` | `{KEY}` | Doc included if `{KEY}` is present on the doc
`key_missing` | `{KEY}` | Doc included if `{KEY}` is *not* present on the doc
`has_value` | `{KEY}` | Doc included if `{KEY}` exists *and* the `{VALUE}` is non-empty
`missing_value` | `{KEY}` | Doc included if `{KEY}` is not present *or* the `{VALUE}` is empty
`created_from` | `{VALUE}` | Doc included if the created time is greater than or equal to `{VALUE}` (in Gregorian seconds)
`created_to` | `{VALUE}` | Doc included if the created time is less than or equal to `{VALUE}` (in Gregorian seconds)
`modified_from` | `{VALUE}` | Doc included if the last-modified time is greater than or equal to `{VALUE}` (in Gregorian seconds)
`modified_to` | `{VALUE}` | Doc included if the last-modified time is less than or equal to `{VALUE}` (in Gregorian seconds)

## Keys

Filters can be used on validated keys (those appearing in the schema) and on custom keys (those included by the caller).

`{KEY}` can be a dot-delimited string representing a JSON key path. So `filter_foo.bar.baz=1` would match a doc that had `{"foo":{"bar":{"baz":1}}}` in it.

## Multiple Filters

Filters can be chained together on a query string and will be applied as a boolean `AND` operation. For example, `?filter_foo=1&has_key=bar` will look for docs where `foo=1` and the key `bar` exists on the doc.
