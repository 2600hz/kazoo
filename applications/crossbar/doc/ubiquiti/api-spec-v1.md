SSO REST API Spec v1
====================

URI prefix: `/api/sso/v1`


self = "self" | own_user_id

I = Implementation
T = Test


|  I  |  T  | Endpoint                                 | Method | Session In            | Body In                       | Semantic Code  | HTTP Code | Session Out  | Body Out             | Issues |
|-----|-----|------------------------------------------|--------|---------------------- |-------------------------------|----------------|-----------|--------------|----------------------|--------|
| [ ] | [ ] | `user`                                   | POST   | NONE                  | SOME _known-fields-unique_    | OK             | 201       | NONE         | NONE                 |        |
| [ ] | [ ] | `user`                                   | POST   | NONE                  | SOME _known-fields-nonunique_ | ERROR          | 409       | NONE         | SOME _all-fields_    |[#49][] |
| [ ] | [ ] | `user`                                   | POST   | NONE                  | SOME _unknown-fields_         | ERROR          | 400       | NONE         | NONE                 |        |
| [ ] | [ ] | `user`                                   | _      | NONE                  | _                             | ERROR          | 405       | NONE         | NONE                 |        |
| [ ] | [ ] | `user/`_any_                             | _      | NONE                  | _                             | ERROR          | 403       | NONE         | NONE                 |        |
| [ ] | [ ] | `user/self`                              | POST   | SOME _uid_            | _                             | OK             | 201       | NONE         | NONE                 |        |
| [ ] | [ ] | `user/self`                              | GET    | SOME _uid_            | _                             | OK             | 200       | NONE         | SOME _all-fields_    |        |
| [ ] | [ ] | `user/self`                              | PUT    | SOME _uid_            | SOME _known-fields-unique_    | OK             | 204       | NONE         | NONE                 |        |
| [ ] | [ ] | `user/self`                              | PUT    | SOME _uid_            | SOME _known-fields-nonunique_ | ERROR          | 409       | NONE         | NONE                 |        |
| [ ] | [ ] | `user/self`                              | PUT    | SOME _uid_            | SOME _unknown-fields_         | ERROR          | 422       | NONE         | NONE?                |[#51][] |
| [ ] | [ ] | `user/self`                              | _      | SOME _uid_            | _                             | ERROR          | 405       | NONE         | NONE                 |        |
| [ ] | [ ] | `user/`_any_                             | _      | SOME _uid_            | _                             | ERROR          | 403       | NONE         | NONE                 |        |
| [x] | [x] | `group`                                  | _      | NONE                  | _                             | ERROR          | 401       |              | NONE                 |        |
| [x] | [x] | `group`                                  | POST   | SOME _uid_            | SOME _group-name-unique_      | OK             | 201       |              | SOME _gid_           |        |
| [x] | [x] | `group`                                  | POST   | SOME _uid_            | SOME _group-name-nonunique_   | ERROR          | 409       |              | NONE                 |        |
| [x] | [x] | `group`                                  | POST   | SOME _uid_            | SOME _invalid_                | ERROR          | 422       |              | NONE                 |        |
| [x] | [x] | `group`                                  | _      | SOME _uid_            | _                             | ERROR          | 405       |              | NONE                 |        |
| [x] | [x] | `group/`_unknown_                        | _      | _                     | _                             | ERROR          | 404       |              | NONE                 |        |
| [x] | [x] | `group/`_gid_                            | _      | SOME _uid-non-member_ | _                             | ERROR          | 403       |              | NONE                 |        |
| [x] | [x] | `group/`_gid_                            | GET    | SOME _uid-member_     | _                             | OK             | 200       |              | SOME _group-members_ |        |
| [x] | [x] | `group/`_gid_                            | PUT    | SOME _uid-member_     | SOME _known-fields-unique_    | OK             | 204       | NONE         | NONE                 |        |
| [x] | [x] | `group/`_gid_                            | PUT    | SOME _uid-member_     | SOME _known-fields-nonunique_ | ERROR          | 409       | NONE         | NONE                 |        |
| [x] | [x] | `group/`_gid_                            | PUT    | SOME _uid-member_     | SOME _unknown-fields_         | ERROR          | 422       | NONE         | NONE                 |        |
| [x] | [x] | `group/`_gid_                            | _      | SOME _uid-member_     | _                             | ERROR          | 405       |              | NONE                 |        |
| [ ] | [ ] | `group/`_gid_`/`_user-name_              | PUT    | SOME _uid-member_     | _                             | OK             | 204       |              | NONE                 |        |
| [ ] | [ ] | `group/`_gid_`/`_user-name-existent_     | HEAD   | SOME _uid-member_     | _                             | OK             | 204       |              | NONE                 |        |
| [ ] | [ ] | `group/`_gid_`/`_user-name-non-existent_ | HEAD   | SOME _uid-member_     | _                             | ERROR          | 404       |              | NONE                 |        |
| [ ] | [ ] | `group/`_gid_`/`_user-name_              | DELETE | SOME _uid-member_     | _                             | OK             | 204       |              | NONE                 |        |
| [ ] | [ ] | `group/`_gid_`/`_user-name_              | _      | _                     | _                             | ERROR          | 405       |              | NONE                 |        |
| [x] | [x] | `group_id`                               | GET    | NONE                  | _                             | ERROR          | 401       |              | NONE                 |        |
| [x] | [x] | `group_id`                               | GET    | SOME _                | _                             | ERROR          | 404       |              | NONE                 |        |
| [x] | [x] | `group_id`                               | _      | _                     | _                             | ERROR          | 405       |              | NONE                 |        |
| [x] | [x] | `group_id/`_any_                         | _      | NONE                  | _                             | ERROR          | 401       |              | NONE                 |        |
| [x] | [x] | `group_id/`_unknown_                     | _      | SOME _                | _                             | ERROR          | 404       |              | NONE                 |        |
| [x] | [x] | `group_id/`_group-name_                  |        | SOME _uid-non-member_ | _                             | ERROR          | 403       |              | NONE                 |        |
| [x] | [x] | `group_id/`_group-name_                  | GET    | SOME _uid-member_     | _                             | OK             | 200       |              | SOME _group-id_      |        |
| [x] | [x] | `group_id/`_group-name_                  | _      | SOME _uid-member_     | _                             | ERROR          | 405       |              | NONE                 |        |
| [x] | [ ] | `user_id`                                | GET    | NONE                  | _                             | ERROR          | 401       |              | NONE                 |        |
| [x] | [ ] | `user_id`                                | GET    | SOME _                | _                             | ERROR          | 404       |              | NONE                 |        |
| [x] | [ ] | `user_id`                                | _      | _                     | _                             | ERROR          | 405       |              | NONE                 |        |
| [x] | [ ] | `user_id/`_any_                          | _      | NONE                  | _                             | ERROR          | 401       |              | NONE                 |        |
| [x] | [ ] | `user_id/`_unknown_                      | _      | SOME _                | _                             | ERROR          | 404       |              | NONE                 |        |
| [x] | [ ] | `user_id/`_user-name_                    | GET    | SOME _uid_            | _                             | OK             | 200       |              | SOME _user-id_       |        |
| [x] | [ ] | `user_id/`_user-name_                    | _      | SOME _uid_            | _                             | ERROR          | 405       |              | NONE                 |        |
| [ ] | [ ] | `login`                                  | POST   | _                     | SOME _credentials-good_       | OK             | 200       | SOME _uid_   | SOME _all-fields_    |        |
| [ ] | [ ] | `login`                                  | POST   | _                     | SOME _credentials-bad_        | ERROR          | 403       | NONE         | NONE                 |        |
| [ ] | [ ] | `login`                                  | _      | _                     | _                             | ERROR          | 405       | NONE         | NONE                 |        |
| [ ] | [ ] | `login/`_any_                            | _      | _                     | _                             | ERROR          | 404       | NONE         | NONE                 |        |
| [ ] | [ ] | `logout`                                 | POST   | _                     | _                             | OK             | 204       | SOME _junk_  | NONE                 |        |
| [ ] | [ ] | `logout`                                 | _      | _                     | _                             | ERROR          | 405       | NONE         | NONE                 |        |
| [ ] | [ ] | `logout/`_any_                           | _      | _                     | _                             | ERROR          | 404       | NONE         | NONE                 |        |
| [ ] | [ ] | `verify`                                 | _      | _                     | _                             | ERROR          | 404       | NONE         | NONE                 |        |
| [ ] | [ ] | `verify/`_code-valid_                    | POST   | _                     | _                             | OK             | 204       | NONE         | NONE                 |        |
| [ ] | [ ] | `verify/`_code-invalid_                  | POST   | _                     | _                             | ERROR          | 404       | NONE         | NONE                 |        |
| [ ] | [ ] | `verify/`_any_                           | _      | _                     | _                             | ERROR          | 405       | NONE         | NONE                 |        |
| [ ] | [ ] | `search/email`                           | _      | _                     | _                             | ERROR          | 404       | NONE         | NONE                 |        |
| [ ] | [ ] | `search/email/`_existent_                | HEAD   | _                     | _                             | OK             | 204       | NONE         | NONE                 |        |
| [ ] | [ ] | `search/email/`_nonexistent_             | HEAD   | _                     | _                             | ERROR          | 404       | NONE         | NONE                 |        |
| [ ] | [ ] | `search/email/`_any_                     | _      | _                     | _                             | ERROR          | 405       | NONE         | NONE                 |        |
| [ ] | [ ] | `search/username`                        | _      | _                     | _                             | ERROR          | 404       | NONE         | NONE                 |        |
| [ ] | [ ] | `search/username/`_existent_             | HEAD   | _                     | _                             | OK             | 204       | NONE         | NONE                 |        |
| [ ] | [ ] | `search/username/`_nonexistent_          | HEAD   | _                     | _                             | ERROR          | 404       | NONE         | NONE                 |        |
| [ ] | [ ] | `search/username/`_any_                  | _      | _                     | _                             | ERROR          | 405       | NONE         | NONE                 |        |
| [ ] | [ ] | _                                        | _      | _                     | _                             | ERROR          | 406       | NONE         | NONE                 |        |


[#49]: https://github.com/Ubiquiti-Cloud/ubic/issues/49
[#51]: https://github.com/Ubiquiti-Cloud/ubic/issues/51
