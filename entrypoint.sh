#!/usr/bin/env bash
set -x

pid=0

# SIGTERM-handler
term_handler() {
  if [ $pid -ne 0 ]; then
    kill -SIGTERM "$pid"
    wait "$pid"
  fi
  exit 143; # 128 + 15 -- SIGTERM
}

# setup handlers
# on callback, kill the last background process, which is `tail -f /dev/null` and execute the specified handler
trap 'kill ${!}; term_handler' SIGTERM

# Start PostgreSQL service
/etc/init.d/postgresql start

# wait forever
while true
do
  ls -d **/*.hs | entr -d -r stack run
done

