#!/usr/bin/env bash
# Take python code from text input, and analyze the code.
# Need to have "tmp" dir
TMP_FILE=./tmp/t.py
TMP_AST=./tmp/t.py.ast
cat > $TMP_FILE
../python_pprint/pprint.py -l $TMP_FILE > $TMP_AST
echo "---------python ast--------------"
cat $TMP_AST
echo "---------analysis result---------"
./main.native $TMP_AST
