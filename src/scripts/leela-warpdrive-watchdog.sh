#!/bin/sh

. /etc/default/leela-warpdrive

env LEELA_WARPDRIVE_WATCHDOG_USER=$LEELA_WARPDRIVE_WATCHDOG_USER \
    LEELA_WARPDRIVE_WATCHDOG_PASS=$LEELA_WARPDRIVE_WATCHDOG_PASS \
    LEELA_WARPDRIVE_ENDPOINT=$LEELA_WARPDRIVE_ENDPOINT python2.7 <<EOF
import os
import sys
import time
from pyleela import lql

user     = os.environ["LEELA_WARPDRIVE_WATCHDOG_USER"]
secret   = os.environ["LEELA_WARPDRIVE_WATCHDOG_PASS"]
cluster  = [os.environ["LEELA_WARPDRIVE_ENDPOINT"]]

def health_check ():
  for at in range(5):
    try:
      with lql.with_context(cluster, user, secret, 10000): pass
      return(True)
    except: time.sleep(1 << at)

ok = health_check()
sys.exit(0 if ok else 1)
EOF
