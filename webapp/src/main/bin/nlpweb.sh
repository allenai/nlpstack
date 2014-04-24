#!/bin/bash

CLASS_NAME="org.allenai.aitk.nlpweb.Nlpweb"

SCRIPT_DIR=`dirname $0`
SHORT_NAME=`basename $0 .sh`
. "${SCRIPT_DIR}/run-class.sh" "$CLASS_NAME" "$SHORT_NAME" "$@"
