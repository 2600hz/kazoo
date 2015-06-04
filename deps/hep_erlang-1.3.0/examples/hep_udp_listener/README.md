hep_udp_listener example
========================

To try this example, you need GNU `make` and `git` in your path.

To build this example, run the following command:

``` bash
$ make
```
Start the example in the foreground, as you want to look at the console for
messages:

``` bash
$ ./_rel/bin/hep_udp_listener console
```

Then point some HEP generating source to your IP on port 9060/udp and do some
calls. You should see some HEP messages appear on your console.

Enable HEPv1 in FreeSWITCH
--------------------------

``` xml
<!--
  ***
  *** PLEASE NOTE:
  ***
  *** This configuration only shows the relevant settings to
  *** turn capturing of SIP messages on.
  ***
-->

<document type="freeswitch/xml">
  <section name="configuration">
    <configuration name="sofia.conf">
      <global_settings>
        <param name="capture-server" value="udp:1.2.3.4:9060"/>
      </global_settings>
      <profiles>
        <profile name="internal">
          <settings>
            <param name="sip-capture" value="yes"/>
          </settings>
        </profile>
      </profiles>
    </configuration>
  </section>
</document>
```

Enable HEPv1 or HEPv2 in OpenSIPS
---------------------------------

TBD

Enable HEPv1

---

Copyright &#169; 2013, Matthias Endler. All rights reserved.
