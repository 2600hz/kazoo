%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license.
%%% See the NOTICE for more information.

{application, couchbeam,
 [{description, "Erlang CouchDB kit"},
  {vsn, "0.8.0"},
  {modules, [couchbeam_app, couchbeam_attachments, couchbeam_changes, couchbeam_deps, couchbeam_doc, couchbeam, couchbeam_httpc, couchbeam_json_stream, couchbeam_sup, couchbeam_util, couchbeam_uuids, couchbeam_view, gen_changes]},
  {registered, [
    couchbeam_sup
  ]},
  {applications, [kernel, stdlib, crypto, public_key, ssl, sasl,
        ibrowse]},
  {env, []},
  {mod, { couchbeam_app, []}}
 ]
}.
