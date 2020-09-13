#!/usr/bin/env python3
import asyncio
from aio_pika import connect_robust, ExchangeType, Message
import time
from datetime import datetime
import sys
import random
from exchanges import get_exchange_from_cmdline


async def main(loop, exchange_name, exchange_type, sleep_seconds, routing_keys):
    connection = await connect_robust(
        "amqp://localhost/", loop=loop
    )

    channel = await connection.channel()
    exchange = await channel.declare_exchange(exchange_name, exchange_type)

    while True:
        now = datetime.now()
        current_time = now.strftime("%H:%M:%S.%f")
        routing_key = random.choice(routing_keys)
        body = "[" + routing_key +  "] Time = " + current_time
        await exchange.publish(
            Message(body=body.encode()),
            routing_key=routing_key
        )
        print(" Sent '", body, "'")
        await asyncio.sleep(sleep_seconds)

    await connection.close()


if __name__ == "__main__":
    if len(sys.argv) < 4:
        print("Usage: producer.py <exchange_type> <sleep_seconds> <routing_key, routing_key, routing_key>")
        sys.exit(1)
  
    exchange_type, exchange_name = get_exchange_from_cmdline(sys.argv[1])

    sleep_seconds = float(sys.argv[2])
    
    routing_keys = sys.argv[3:]
    
    print("Producing randomly to topics/routing keys: " + str(routing_keys))

    print("Sleeping for " + str(sleep_seconds) + " seconds between sends")
    
    loop = asyncio.get_event_loop()
    loop.run_until_complete(main(loop, exchange_name, exchange_type, sleep_seconds, routing_keys))
