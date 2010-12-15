#!/bin/sh

echo '(load "exo5") (test-between 150 200 3)' | sbcl > /tmp/$$-stat
cat /tmp/$$-stat | grep -v '^ek' | grep -v '^di' | tail -n +10 | grep -v '^  \[' | head -n -1 > /tmp/$$-stat2
cat /tmp/$$-stat2 | while read ab; do read ab; ab="${ab#  }" echo -n "${ab%% *} "; read ab; read ab; read ab; read ab; ab="${ab#  }" echo "${ab%% *}"; read ab; done > /tmp/$$-stat3
cat /tmp/$$-stat3 | while read ab; do read xy; echo "$ab $xy"; done
