#!/bin/bash

# Created       : 2017 Mar 10 (Fri) 14:26:51 by Harold Carr.
# Last Modified : 2017 Mar 10 (Fri) 14:33:36 by Harold Carr.

rm -f /tmp/BC916*.out

stack exec bc -- 9160 224.0.0.99 9160 &> /tmp/BC9160.out &
stack exec bc -- 9161 224.0.0.99 9160 &> /tmp/BC9161.out &
stack exec bc -- 9162 224.0.0.99 9160 &> /tmp/BC9162.out &
stack exec bc -- 9163 224.0.0.99 9160 &> /tmp/BC9163.out &

