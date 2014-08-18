#!/bin/bash

ghc site.hs &&\
./site rebuild &&\
echo "\n\n\n-------------------------------------------------" &&\
echo "pushing to github" &&\
git add . &&\
git commit -m "automatic publishing script" &&\
git push origin &&\
echo "\n\n\n-------------------------------------------------" &&\
echo "pushing to dreamhost" &&\
rsync -e "/usr/bin/ssh" -av _site/ jackpirate@rockingham.dreamhost.com:izbicki.me
#rsync -e "/usr/bin/ssh" --bwlimit=2000 -av _site/ jackpirate@rockingham.dreamhost.com:izbicki.me
