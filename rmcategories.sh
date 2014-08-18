#/bin/bash

for f in _posts/*; do
    grep -v -e "^- " -e "^categories:" -e "^series:" $f > posts/$(basename $f)
done
