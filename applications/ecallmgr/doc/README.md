# ecallmgr *Erlang Call Manager*

Ecallmgr connects to and manages the FreeSWITCH servers and the interactions between higher-level applications and the FreeSWITCH layer.

## Overview

Higher-level applications don't need to know which FreeSWITCH server the call they're processing is located on. Ecallmgr manages the bookkeeping of where calls and conferences are, as well as providing an abstraction layer over the particulars of the FreeSWITCH commands.
