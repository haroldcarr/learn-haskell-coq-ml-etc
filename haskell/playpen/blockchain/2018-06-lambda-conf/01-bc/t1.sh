#!/bin/bash

. ./base.sh

section "this shows how a TX might get dropped"


doone 0 tx?TX1
doone 0 tx-no-forward?TX2
doall mine
sleep 2
section "after doall mine"
doall env

doone 1 tx-no-forward?TX3
doone 1 mine
sleep 2
section "at this point the heights should be 0:2 1:3"
section "0 should have TX1 TX2 in its chain"
section "1 should have TX1 TX3 in its chain"
doall env

doone 0 resolve
section "0 should have 1's chain; *but it should TX2 in its pool*"
doall env

