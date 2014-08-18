#!/bin/bash

ghc site.hs &&\
./site rebuild &&\
rsync -e "/usr/bin/ssh" -av _site/ jackpirate@rockingham.dreamhost.com:izbicki.me
#rsync -e "/usr/bin/ssh" --bwlimit=2000 -av _site/ jackpirate@rockingham.dreamhost.com:izbicki.me
