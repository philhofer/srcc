#!/bin/sh -e

compiledir="./compiledir"

[ -d ${compiledir} ] && rm -r ${compiledir}

for src in *.scm; do
	svcdir=$(mktemp -d -p .)
	../ss6c -d ${svcdir} -c ${compiledir} ${src} || {
	    echo "compiling ${src} failed"
	    exit 1
	}
	s6-rc-db -c ${compiledir} check || {
	    echo "s6-rc-db check for ${src} failed"
	    exit 1
	}
	echo "${src} ok"
	rm -r ${svcdir} ${compiledir}
done
