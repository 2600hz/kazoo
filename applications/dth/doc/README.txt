Getting started:

Get the WSDL file from your DTH server:

wget http://IP.OF.DTH.SERVER/DTHSOAPAPI_v3/dthSOAP.asmx?WSDL

Adjust URL above as necessary. Save this file to "dth/priv/dthsoap.wsdl"

Starting up the whapps container, ensure the DTH whapp has been started and a
new file should be written to "dth/include/dthsoap.hrl" containing the records
for the WSDL. (You will need to rename the file if it exists already, and re-
start the DTH whapp to create a new version of the .hrl file).
