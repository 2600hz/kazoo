Base16 encoding and decoding
============================

API
---

Both `encode/1` and `decode/1` functions are of `(binary()) -> binary()` type.

The usage looks like:

```erlang
B = crypto:rand_bytes(10).
H = base16:encode(B).
B = base16:decode(H).
```

Please, note:

   * `base16:encode/1` uses upper-case latters.
   * `base16:decode/1` requires the argument to be a binary of odd-number size.


License
-------

The library itself is licensed under the Simplified (2-clause) BSD license.

The tests, due to dependency on [PropEr], are licensed under the GPLv3 license.

[PropEr]:http://proper.softlab.ntua.gr/index.html
