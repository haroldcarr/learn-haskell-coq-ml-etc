#!/bin/bash
stack exec peer   -- 127.0.0.1 8080 8081 8082 8083 8084 > /tmp/$$-peer-8080.log 2>&1 &
x8080=$!
stack exec peer   -- 127.0.0.1 8081 8082 8083 8084 8080 > /tmp/$$-peer-8081.log 2>&1 &
x8081=$!
stack exec peer   -- 127.0.0.1 8082 8083 8084 8080 8081 > /tmp/$$-peer-8082.log 2>&1 &
x8082=$!
stack exec peer   -- 127.0.0.1 8083 8084 8080 8081 8082 > /tmp/$$-peer-8083.log 2>&1 &
x8083=$!
stack exec peer   -- 127.0.0.1 8084 8080 8081 8082 8083 > /tmp/$$-peer-8084.log 2>&1 &
x8084=$!
sleep 2
stack exec client -- 127.0.0.1 8085 8080 8081 8082 8083 > /tmp/$$-client-8085.log 2>&1 &
x8085=$!
echo kill ${x8080} ${x8081} ${x8082} ${x8083} ${x8084} ${x8085}
