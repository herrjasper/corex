#!/bin/bash

set -ue

paste <(cut -f 2-4 $1) <(cut -f 1 $1) <(cut -f 5 $1)  <(cut -f 6 $1|cut -f 2 -d " ") <(cut -f 7 $1|cut -f 1 -d " ") <(cut -f 7 $1|cut -f 2 -d " ")
