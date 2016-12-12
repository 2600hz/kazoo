
# Internet fax for Office 2010
* Create a fax.reg file with the contents below replacing xxx.fax.kazoo.io with the smtp address of faxbox.
* double click to merge into registry.
* open winword, text a bit
* File | Save & Send | Send as Internet Fax

this may work on other office versions, try by changing the version 14.0
```
Windows Registry Editor Version 5.00

[HKEY_CURRENT_USER\Software\Microsoft\Office\14.0\Common\Services\Fax]
"FSPName"="kazoo.io"
"FSPHome"="kazoo.io"
"FaxAddress"="%D <%F@xxx.fax.kazoo.io>"
"NoFax"=dword:00000000
"TicketOptions"=dword:00000001
"CostButton"=dword:00000000
"DoNotConvertMailBody"=dword:00000000
"UseCoverSheet"=dword:00000000

```
