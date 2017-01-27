# DNS configuration for smtp-to-fax

## generated addresses

simple bind configuration for kazoo generated addresses.
assuming that fax.kazoo.io is the domain configured for faxboxes.

```
$ORIGIN kazoo.io.
	MX	0 mail-01.kazoo.io.
        MX  0 mail-01.kazoo.io.
        TXT "v=spf1 mx ptr -all"
mail-01     A   x.x.x.x
mail-02     A   x.x.x.x
$ORIGIN fax.kazoo.io.
*			MX	5 mail-01.kazoo.io.
*           MX  5 mail-02.kazoo.io.
```

## custom addresses

When a user customizes the SMTP address of the faxbox, the user must
also add a DNS entry to the domain. For example, if the user
configured the SMTP address to be `myfax.mydomain.tld`, then the user
would have to add the following DNS entry:

```
$ORIGIN mydomain.tld.
myfax       CNAME   kazoo.io.
```

if user has many faxboxes he can also add wildcards

```
$ORIGIN fax.mydomain.tld.
*           CNAME   kazoo.io.
```

* Important : add SPF policy to dns to prevent email address spoofing
