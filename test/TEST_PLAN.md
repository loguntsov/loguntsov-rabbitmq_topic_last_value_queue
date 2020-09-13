# Test plan for the Topic Last Value Queue 


## Basic tests
You can use the utlilities in the python/ directory of this repo to complete these tests. They use aio_pika

### Aim:

Basic tests verifying that:

- slow consumers using a NORMAL QUEUE will see each value published for that topic
- fast consumers using a NORMAL QUEUE will see each value published for that topic
- slow consumers using a TLV QUEUE will skip 'intermediate' values for a topic (a routing key)
- fast consumers using a TLV QUEUE will see each value published for that topic

### Tests:

./producer.py topic 1 a  # 1 second delay on producing
./producer.py topic 2 b  # 2 second delay

./consumer.py queue1 topic 5 a b         # slow consumer, NORMAL queue, verify it prints all values (ie printed timestamps should slowly drift out of line)
./consumer.py queue2 topic 0.01 a b      # fast consumer, NORMAL queue, should see all values

./consumer_tlv.py queue3 topic 5 b     # slow consumer, TLV queue, verify it only prints new values (ie printed timestamps should roughly match)
./consumer_tlv.py queue4 topic 5 a     # same as above, different routing key
./consumer_tlv.py queue5 topic 0.04 a  # fast consumer, TLV queue, verify it prints ALL values


### Aim:

Test that consumers running faster than the producer receive all of the to up date information in virtually real time (ie no delay)

### Tests:

./producer.py topic 1 a b c     

./consumer_tlv.py queue1 topic 0.01 a b c
./consumer_tlv.py queue2 topic 0.01 a b c



### Aim:

Test that many values on one topic don't cause starvation to another topic, or vice versa. Ordering should be basically round robin delivery of topic values 

### Tests:

#### 1
./producer.py topic 0.01 a      #fast  
./producer.py topic 1 b      #medium
./producer.py topic 3 c      #slow

./consumer_tlv.py queue1 topic 5 a b c  # even slower!


#### 2

./producer.py topic 0.01 a b c d   # producer goes flat chat producing across lots of topics
./consumer_tlv.py queue1 topic 1 a b c d  # consumer fairly slow

Start and stop the producer -- letting the consumer drain the queue and then starting producer again


#### 3

 # producers go flat chat producing across lots of topics
./producer.py topic 0.01 a        
./producer.py topic 0.01 b
./producer.py topic 0.01 c
./producer.py topic 0.01 d

./consumer_tlv.py queue1 topic 1 a b c d  # consumer fairly slow

Start and stop individual producers, letting each topic drain from the queue then start the producer again


### Confirm
Ordering of values delivered / printed at the consumer should be basically round robin delivery of topic values
Values should always be 'latest' values -- ie the now and received timestamps should be super close



### Aim:

Test advanced topic routing features work ok


### Tests

./producer.py topic 0.01 a.1.x a.2.x a.3.y a.3.z 
./producer.py topic 1 b.1.x b.3.z b.5.z

./consumer_tlv.py queue1 topic 1 *.1.* 
./consumer_tlv.py queue2 topic 5 a.# 
./consumer_tlv.py queue3 topic 5 \#.z



### Aim:

Ensure TLV is working with prefetch count higher than 1


### Tests

To test different timing conditions and message flows for acks etc there's one test using a normal pika consumer, one using aio_pika

#### Normal pika

./producer.py topic 0.01 a b c d e f g h i j k l m n o p q r

./prefetch_pika_consumer.py

#### Confirm

Consumer uses prefetch of 10 and processes messages one per second. So after the first 10 print similar timestamps you should expect from there it will print timestamps that stay about 10 seconds in the past
Should observe a vaguely round robin / random looking delivery of topic keys


#### aio_pika

The tester for this can be found in test/real

./producer.py topic 0.01 1.0.value.OKEX.1 1.0.value.OKEX.2 1.0.value.OKEX.3 1.0.value.OKEX.4 1.0.value.OKEX.5 1.0.value.OKEX.6 1.0.value.OKEX.7 1.0.value.OKEX.8 1.0.value.OKEX.9 1.0.value.OKEX.10 1.0.value.OKEX.11 1.0.value.OKEX.12 1.0.value.OKEX.13 1.0.value.OKEX.14 1.0.value.OKEX.15 1.0.value.OKEX.16 1.0.value.OKEX.17 1.0.value.OKEX.18 1.0.value.OKEX.19 1.0.value.OKEX.20

./value_consumer.py q1 topic-routing-test 1 --last-value


#### Confirm

This used to deadlock the plugin queue, so just confirming you still get messages is a good start. Apart from that inspect and check messages received do not experience any time drift.


### Aim:

Ensure compatibility with direct, fanout and topic exchanges. 

### Tests

Topic exchanges were covered by all the above tests, not extra needed
Retest all the above (except advanced topic routing) using direct exchange
Retest all the above (except advanced topic routing) using fanout exchange. Tests will be basically valid but the 'binding' to routing keys will be ignored and all consumers will receive all types a, b, c etc



## 'Real data' tests (setup and run on a live dev server)
Connect a publisher or more than one that is publishing a lot of 'values' topics (a reference pricer is a good example of this so the example uses this)

Upload test/real/value_consumer.py and run. It prints consumed values topics. Manually inspect that the values that are bring printed are appropriate based on what was produced in the reference pricer logs (old values thrown away, the most recent to when the consume was done is the value that got printed)

Upload test/real/value_consumer_high_cpu.py and run. Same as above but output might differ


Connect a real slow consumer, ie. venus


This final test you might want to combine with the infrastructure tests (1 - 3 node cluster) by doing in 'real' dev:
Do the above tests but connected to a rabbitmq that is also publishing a lot of other data (and the exchange you're connected to also has lots of other queues) -- ie. Lots of things using normal queues, attached to the same exchange as the new consumer using this special queue


## 'Infrastructure' tests

After installing the plugin, start and stop the entire rabbitmq server (we had issues with it crashing on startup at one stage)

Check the plugin can be enabled and disabled, enabled again

Retest the above tests on single and multi-node rabbit clusters (1&3 nodes is sufficient)

Simulate a server crash, eg. by running one of the above 'basic' tests and force restarting the computer, check that rabbitmq boots up ok after (we had some issues with this, recovering from something like the Write Ahead Log (WAL))


==== THIS DOESNT WORK - LOCKS UP THE PLUGIN ON UPGRADE ====

Upgrade downgrade upgrade. At every stage check that the plugin works, can be enabled and disabled
Eg. 3.8.0 -> 3.8.2 -> 3.8.0
Might want to sudo apt install synaptic and google how to downgrade a package with that tool

(Note plugin does not work on rabbitmq 3.7, crashes because of change of internal apis. If you are using 3.7 then please upgrade to any 3.8 version. Don't bother testing downgrade to any version of 3.7)

