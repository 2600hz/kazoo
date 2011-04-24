#!/bin/bash

cd `dirname $0`

export ip_address="$1"
export roles="$2"
export password="$3"
export node_name="$4"
export operating_system="$5"

echo "$(date +%s): knife bootstrap ${ip_address} '${roles}' -x root -P ${password} -N ${node_name} -d ${operating_system}" >> run_log.txt
knife bootstrap ${ip_address} '${roles}' -x root -P ${password} -N ${node_name} -d ${operating_system}

sleep 10
