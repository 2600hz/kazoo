#!/bin/bash

cd `dirname $0`

export email_address="$1"
export first_name="$2"
export last_name="$3"
export from_user="$4"
export recv_timestamp="$5"

template_file="./vm_email.tmpl"

sed "s|{email_address}|${email_address}|g;s|{first_name}|${first_name}|g;s|{last_name}|${last_name}|g;s|{from_user}|${from_user}|g;s|{recv_timestamp}|${recv_timestamp}|g" ${template_file} | mail -s "You've received a new voicemail!" ${email_address}
