msg () {
    echo ${1}
    echo
}



doone () {
    echo
    curl --silent --write-out "\n" localhost:300${1}/${2}
    echo
    echo
}

doall () {
    for p in 1 2 3
    do
        doone ${p} ${1}
    done
}
