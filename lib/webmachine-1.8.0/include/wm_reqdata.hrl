-record(wm_reqdata, {method, version, peer, wm_state,
                     disp_path, path, raw_path, path_info, path_tokens,
                     app_root,response_code,max_recv_body, max_recv_hunk,
                     req_cookie, req_qs, req_headers, req_body,
                     resp_redirect, resp_headers, resp_body,
                     host_tokens, port, notes
                    }).

