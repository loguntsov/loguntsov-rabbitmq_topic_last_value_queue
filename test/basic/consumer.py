#!/usr/bin/env python3
import asyncio
import sys
from consume import consume
from exchanges import get_exchange_from_cmdline

sleep_seconds = None

if __name__ == "__main__":
    if len(sys.argv) < 5:
        print("Usage: consumer.py <queue_name> <exchange_type> <sleep_seconds> <routing_key, routing_key, routing_key>")
        sys.exit(1)
    queue_name = sys.argv[1]
    exchange_type, exchange_name = get_exchange_from_cmdline(sys.argv[2])

    last_value = False
    sleep_seconds = float(sys.argv[3])
    routing_keys = sys.argv[4:]
    
    print("Binding queue to routing keys: " + str(routing_keys))

    print("Sleeping for " + str(sleep_seconds) + " seconds between sends")
    
    loop = asyncio.get_event_loop()
    loop.create_task(consume(loop, queue_name, exchange_name, exchange_type, sleep_seconds, routing_keys, last_value))
    loop.run_forever()
