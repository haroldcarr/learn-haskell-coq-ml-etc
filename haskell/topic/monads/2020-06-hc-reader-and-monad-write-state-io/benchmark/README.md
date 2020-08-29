# run

    stack bench

# run with graphical output

    stack bench --ba "--output <file>"

where `<file>` should be replaced with the desired output path/location

# criterion options

    stack bench --ba --help

examples

    stack bench --ba --list                   # list benchmarks that can be run
    stack bench --ba <name>                   # run a single benchmark
    stack bench --ba "<name1> <name2>"        # run two benchmarks
    stack bench --ba "--time-limit <seconds>" # time limit to run benchmark
    stack bench --ba "--time-limit <seconds> --output <file>"
    stack bench --ba "--time-limit <seconds> --output <file> <name1> ..."
