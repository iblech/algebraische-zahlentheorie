#!/bin/bash
# Benötigt leaderboard.txt auf der Standardeingabe.

ssh speicherleck.de "
  cd /var/www/iblech/stuff/tutor-algebraische-zahlentheorie/leaderboard || exit
  git pull --rebase
  cabal build
  cd ..
  ./leaderboard/dist/build/Main/Main > index.html
  pdfjoin --outfile alle.pdf uebung??.pdf
"
