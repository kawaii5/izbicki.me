#!/bin/bash

#for i in `find img`; do
    #if [ -f "$i" ]; then 
        #echo "$i"
        #convert "$i" -resize 700x10000\> "$i"
    #fi
#done

for i in `find posts`; do
    if [ -f "$i" ]; then
        echo "$i"
        
        # this command replaces wordpress style latex with markdown latex
        set 

        # this command removes image links
        #sed -i "s/\[\!\[\(.*\)\](\(.*\)).*\](.*)/\!\[\1\](\2)/" "$i"
    fi
done
