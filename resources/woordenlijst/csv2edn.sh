#!/bin/sh

echo "[" ; \
    echo $(cat <&0 | sed "s/\"//g"  |\
           awk -F';' '{print "{:nl \"" $1"\" :en \"" $2 "\" :hoofdstuk \"" $4 "\"  }\\n"}' ); \
echo "]"

    
    
