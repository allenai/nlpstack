#!/bin/bash

CLASS_NAME="org.allenai.aitk.nlpweb.Nlpweb"
JVM_ARGS="-Xmx4g"

SCRIPT_DIR=`dirname $0`
SHORT_NAME=`basename $0 .sh`
. "${SCRIPT_DIR}/run-class.sh" "$CLASS_NAME" "$SHORT_NAME" "$@"
