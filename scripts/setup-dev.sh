#!/bin/bash

ln -s /opt/kazoo/utils/sup/sup /usr/bin/sup
ln -s /opt/kazoo/utils/media_importer/media_importer /usr/bin/media_importer
ln -s /opt/kazoo/scripts/rabbitmq-server.init /etc/init.d/rabbitmq-server
ln -s /opt/kazoo/scripts/kz-generic.init /etc/init.d/kz-ecallmgr
ln -s /opt/kazoo/scripts/kz-generic.init /etc/init.d/kz-whistle_apps
ln -s /opt/kazoo/scripts/rabbitmq-generic.sh /usr/bin/rabbitmq-server
ln -s /opt/kazoo/scripts/rabbitmq-generic.sh /usr/bin/rabbitmq-plugins
ln -s /opt/kazoo/scripts/rabbitmq-generic.sh /usr/bin/rabbitmqctl
rm -rf /var/lib/rabbitmq /var/log/rabbitmq /usr/lib/rabbitmq
mkdir -p /var/lib/rabbitmq
chown rabbitmq:daemon /var/lib/rabbitmq
ln -sfn /opt/kazoo/deps/rabbitmq_server-3.1.3/scripts /var/lib/rabbitmq/bin
chmod +x /etc/init.d/rabbitmq*
chmod +x /etc/init.d/kz-*
chmod +x /usr/bin/rabbitmq*
if ! getent passwd kazoo >/dev/null 2>&1; then
    useradd -r -g daemon -M -d /opt/kazoo -s /sbin/nologin kazoo
else
    usermod -g daemon -d /opt/kazoo -s /sbin/nologin kazoo
fi
if ! getent passwd rabbitmq >/dev/null 2>&1; then
    useradd -r -g daemon -M -d /var/lib/rabbitmq -s /sbin/nologin rabbitmq
else
    usermod -g daemon -d /var/lib/rabbitmq -s /sbin/nologin rabbitmq                                                                                                                
fi

