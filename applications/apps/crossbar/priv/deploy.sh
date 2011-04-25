#!/bin/bash

cd `dirname $0`

export ip_address="$1"
export roles="$2"
export password="$3"
export node_name="$4"
export operating_system="$5"
export server_id="$6"
export account_id="$7"
export erlang_cookie="$8"
export db_pwd="$9"


eval "echo \"$(cat ./deploy_data.tmpl)\"" > /tmp/${server_id}.json

echo "$(date +%s) server[${server_id}] account[$account_id]: knife bootstrap ${ip_address} '${roles}' -x root -P ${password} -N ${node_name} -d ${operating_system}" >> run_log.txt

knife bootstrap ${ip_address} '${roles}' -x root -P ${password} -N ${node_name} -d ${operating_system}
