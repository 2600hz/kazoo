#!/bin/sh
CTL=$1
USER="O=client,CN=$(hostname)"

# Test direct connections
$CTL add_user "$USER" ''
$CTL set_permissions -p / "$USER" ".*" ".*" ".*"
