# RabbitMQ last key value queue

This code provides new type of queue, which is replaces elements in queue by key.
It is helpful to update queue by newest elements (some ticks, real-time data and etc).

Example of work:

Presentation of queue is ``[ Head, SecondElement, ... LastElememt ]``

Head is going to as next data for consumers

Initial state:
```
{key_1, value_1},{key_2, value_2},
```
Step 1: Add new non existed element {key_3, value_3} :
```
{key_1, value_1}, {key_2, value_2}, {key_3, value_3}
```
Step 2: Updating element {key_1, new_value_1}:
```
{key_2, value_2}, {key_3, value_3}, {key_1, new_value_1}
```

# Usage

## Initialisation

Before using you must define this queue. Queue will be created by this module if you will provide specific key for argument (options) of creation command:
``x-last-value-per-topic = 'enabled'``

## Put element

You must provide key of element in routing_key options within puttin operation.

## Examples of code

All examples of python's code for usage this queue (and tests) are available in test folder. 

# Prerequisites

### Version
You want to match the erlang version that's in use on our servers). So you will want to go on to the our servers and issue:

erl

and see what version the erlang shell prints out to tell you what erlang version you want to install locally and use.


### Remove your existing erlang:

sudo apt remove erlang-*

### Install erlang:

Follow instructions at the bottom of the page here for installing via apt:

https://www.erlang-solutions.com/resources/download.html

At the time of writing they were:

wget https://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb
sudo dpkg -i erlang-solutions_2.0_all.deb

Now install. In my case it was OTP 21 that i wanted, so:

sudo apt install esl-erlang=1:21.3.8.12-1 elixir

You'll want to stop whatever rabbitmq is running on your server. So:
sudo /etc/init.d/rabbitmq-server stop
(and perhaps brew stop on OSX)


# Launching of test

1. ```make run-broker``` <- start RMQ
2. go to python folder and start ```consumer.py``` and ```producer.py```


# Installation instructions

Easiest thing to do is to:
- make run-broker from above, check that you see "started with 1 plugins", then stop the process
- copy plugins/rabbit_topic_last_value_queue-[version].ez in to the plugins directory for your installation (usually /usr/lib/rabbitmq/plugins/)
- you should now see the plugin when you do rabbitmq-plugins list and can enable/disable it using that tool

If you need any more info in installing plugins and/or troubleshooting then refer to:
https://www.rabbitmq.com/plugins.html
https://www.rabbitmq.com/community-plugins.html


# Testing

See test/TEST_PLAN.md

# License

MIT

# Author

Sergey Loguntsov <loguntsov@gmail.com>

