#!/bin/bash

lintPath=""
path="$TM_BUNDLE_SUPPORT"/bin/capp_lint.py

if [[ -x "$path" ]]; then
    lintPath="$path"
fi

if [[ -z "$lintPath" ]]; then
    path="$TM_CAPP_LINT"

    if [[ -n "$path" && -x "$path" ]]; then
        lintPath="$path"
    fi
fi

if [[ -z "$lintPath" && ! `which -s capp_lint.py` ]]; then
    lintPath="`which capp_lint.py`"
fi

if [[ -z "$lintPath" && ! `which -s capp_lint` ]]; then
    lintPath="`which capp_lint`"
fi

if [[ -z "$lintPath" ]]; then
    path="$TM_PROJECT_DIRECTORY/.git/hooks/capp_lint.py"

    if [[ -x "$path" ]]; then
        lintPath="$path"
    fi
fi

echo -n "$lintPath"
