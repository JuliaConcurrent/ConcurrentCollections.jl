#!/bin/sh

if lscpu --parse >/dev/null 2>/dev/null
then
    lscpu --parse | grep -v '^#' | sort --unique --field-separator=, --key=2 | wc -l
elif nproc >/dev/null 2>/dev/null
then
    exec nproc
else
    echo auto
fi
