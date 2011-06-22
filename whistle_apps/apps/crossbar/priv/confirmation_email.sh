#!/bin/bash

cd `dirname $0`

export email_address="$1"
export first_name="$2"
export last_name="$3"
export base_url="$4"
export segment_url="$5"
export activation_key="$6"

export registration_link="${base_url}${segment_url}${activation_key}"

template_file="./signup_email.tmpl"

sed "s|{email_address}|${email_address}|g;s|{first_name}|${first_name}|g;s|{last_name}|${last_name}|g;s|{registration_link}|${registration_link}|g" ${template_file} | mail -s "2600hz Project New Account Registration" ${email_address}
