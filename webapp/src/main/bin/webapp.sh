#!/bin/bash

CLASS_NAME="org.allenai.nlpstack.webapp.Nlpweb"
JVM_ARGS="-Xmx3g"

SCRIPT_DIR=`dirname $0`
SHORT_NAME=`basename $0 .sh`
. "${SCRIPT_DIR}/run-class.sh" "$CLASS_NAME" "$SHORT_NAME" "$@"
