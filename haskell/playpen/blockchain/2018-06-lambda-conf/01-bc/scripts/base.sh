msgN () {
    echo
    printf '%s\n' "${1}"
}

lbl () {
    printf '>: %s\n' "${1}"
}

msg () {
    printf '%s\n' "${1}"
}

doone () {
    echo
    curl --silent --write-out "\n" localhost:300${1}/${2}
}

doall () {
    for p in 1 2 3
    do
        doone ${p} ${1}
    done
}
