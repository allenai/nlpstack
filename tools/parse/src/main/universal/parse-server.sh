#!/bin/bash

CLASS_NAME="edu.knowitall.tool.parse.ClearDependencyParserMain"

SCRIPT_DIR=`dirname $0`
SHORT_NAME=`basename $0 .sh`
APP_ROOT="$SCRIPT_DIR/.."
JVM_ARGS="-Xmx3G"

. "${SCRIPT_DIR}/run-class.sh" "$CLASS_NAME" "$SHORT_NAME" "$@"
