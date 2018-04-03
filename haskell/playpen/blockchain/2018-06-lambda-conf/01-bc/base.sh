section () {
    echo "--------------------------------------------------"
    echo ${1}
    echo
}

doone () {
    echo
    curl localhost:300${1}/${2}
    echo
    echo
}

doall () {
    for p in 0 1 2
    do
        doone ${p} ${1}
    done
}
