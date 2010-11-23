From: "${voicemail_caller_id_name}" <${voicemail_caller_id_number}@gocentrix.com>
To: <${voicemail_email}>
Subject: Voicemail from ${voicemail_caller_id_name} ${voicemail_caller_id_number}
X-Priority: ${voicemail_priority}
X-Mailer: 2600hz.net

Content-Type: multipart/alternative; 
	boundary="000XXX000"

--000XXX000
Content-Type: text/plain; charset=ISO-8859-1; Format=Flowed
Content-Disposition: inline
Content-Transfer-Encoding: 7bit

Mailbox: ${voicemail_account}
Created: ${voicemail_time}
From: ${voicemail_caller_id_number}
Name: ${voicemail_caller_id_name}
Duration: ${voicemail_message_len}

--000XXX000
Content-Type: text/html; charset=ISO-8859-1
Content-Disposition: inline
Content-Transfer-Encoding: 7bit

<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<META http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
<title>Voicemail from "${voicemail_caller_id_name}" <${voicemail_caller_id_number}></title>
<meta content="text/html; charset=iso-8859-1" http-equiv="content-type"/>
</head>
<body>

<font face=arial>
<b>Message From ${voicemail_caller_id_name} <A HREF="tel:${voicemail_caller_id_number}">${voicemail_caller_id_number}</A></b><br>
<hr noshade size=1>
Mailbox: ${voicemail_account}<br>
Created: ${voicemail_time}<br>
From: ${voicemail_caller_id_number}<br>
Name: ${voicemail_caller_id_name}<br>
Duration: ${voicemail_message_len}<br>

</body>
</html>
--000XXX000--
