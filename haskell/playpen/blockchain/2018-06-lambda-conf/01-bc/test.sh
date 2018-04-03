#!/bin/bash

function section {
    echo "--------------------------------------------------"
    echo ${1}
    echo
}

function doone {
    echo
    curl localhost:300${1}/${2}
    echo
    echo
}

function doall {
    for p in 0 1 2
    do
        doone ${p} ${1}
    done
}

section "initial"
doall env

section "0 MY-TX-0"
doone 0 tx?MY-TX-0

section "after MY-TX-0"
doall env

section "0 mine"
doone 0 mine
sleep 2

section "after mine"
doall env

section "2 resolve"
doone 2 resolve

section "after resolve"
doall env

