ear
    echo "======================================================="
    echo " _  _  _ _     _ _____    _    _______ _       _______ "
    echo "| || || | |   | (_____)  | |  (_______) |     (_______)"
    echo "| || || | |__ | |  _      \ \  _      | |      _____   "
    echo "| ||_|| |  __)| | | |      \ \| |     | |     |  ___)  "
    echo "| |___| | |   | |_| |_ _____) ) |_____| |_____| |_____ "
    echo " \______|_|   |_(_____|______/ \______)_______)_______)"
    echo " - - - Signaling the start of next generation telephony"
    echo "======================================================="
    echo
}

fWelcome

cd `dirname $0`

if [ -f $1/whistle_apps/lib/whistle_couch-1.0.0/priv/startup.config ];
then
    echo "# \cp -f $1/whistle_apps/lib/whistle_couch-1.0.0/priv/startup.config ../whistle_apps/lib/whistle_couch-1.0.0/priv/"
    \cp -f $1/whistle_apps/lib/whistle_couch-1.0.0/priv/startup.config ../whistle_apps/lib/whistle_couch-1.0.0/priv/
elif [ -f $1/lib/whistle_couch-1.0.0/priv/startup.config ];
then
    echo "# \cp -f $1/lib/whistle_couch-1.0.0/priv/startup.config ../whistle_apps/lib/whistle_couch-1.0.0/priv/"
    \cp -f $1/lib/whistle_couch-1.0.0/priv/startup.config ../whistle_apps/lib/whistle_couch-1.0.0/priv/
fi

if [ -f $1/lib/whistle_amqp-1.0.0/priv/startup.config ];
then
    echo "# \cp -f $1/lib/whistle_amqp-1.0.0/priv/startup.config ../lib/whistle_amqp-1.0.0/priv/"
    \cp -f $1/lib/whistle_amqp-1.0.0/priv/startup.config ../lib/whistle_amqp-1.0.0/priv/
fi

if [ -f $1/whistle_apps/conf/vm.args ];
then
    echo "# \cp -f $1/whistle_apps/conf/vm.args ../whistle_apps/conf"
    \cp -f $1/whistle_apps/conf/vm.args ../whistle_apps/conf
fi

if [ -f $1/ecallmgr/conf/vm.args ];
then
    echo "# \cp -f $1/ecallmgr/conf/vm.args ../ecallmgr/conf"
    \cp -f $1/ecallmgr/conf/vm.args ../ecallmgr/conf
fi

exit 0
