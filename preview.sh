#!/bin/bash

ghc --make site.hs && ./site rebuild && ./site preview -p 8000
