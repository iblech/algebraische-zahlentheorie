#!/bin/bash
# Benötigt leaderboard.txt auf der Standardeingabe.

ssh speicherleck.de "
  cd /var/www/iblech/stuff/tutor-algebraische-zahlentheorie/leaderboard || exit
  git pull --rebase
  nix-shell --run 'cabal build' -p 'haskellPackages.ghcWithPackages (pkgs: with pkgs; [blaze-html shakespeare])'
  cd ..
  ./leaderboard/dist/build/Main/Main > index.html
  pdfjoin --outfile alle.pdf uebung??.pdf
"
