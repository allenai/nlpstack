#!/bin/bash

CLASS_NAME="edu.knowitall.tool.lemmatize.MorphaStemmer"

SCRIPT_DIR=`dirname $0`
SHORT_NAME=`basename $0 .sh`
APP_ROOT="$SCRIPT_DIR/.."
JVM_ARGS="-Xmx128M"

. "${SCRIPT_DIR}/run-class.sh" "$CLASS_NAME" "$SHORT_NAME" "$@"
