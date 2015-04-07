eradius
=======

A generic RADIUS client and server.

Version 0.6.1 - 05 Mar 2015
---------------------------
* switching to lager logging
* using systemds journal for informational eradius logging

Version 0.6.0 - 11 Feb 2015
---------------------------

* fix Message-Authenitcatior validation in access replies (API change)
* fix case of CAPWAP Power Travelping attributes

Version 0.5.2 - 17 Dec 2014
---------------------------

* change radius.log to a append only log file
* added wtp version attributes
* implement resend

Version 0.5.0 - 20 Jun 2014
---------------------------

* added new vendor Travelping attributes
* add generic nas id

Version 0.3.2 - 25 Sep 2013
---------------------------

* SNMP support moved to seperate application

Version 0.3.1 - 19 Sep 2013
---------------------------

* Add Travelping TLS and CAPWAP Attributes to dictionary
* Add CoA and Disconnect protocol support
* Fix RCF 2868 Tunnel-Type attributes
