#!/bin/bash

elm-make Main.elm --output game.js
scp game.js index.html \
  'playspace:/home/sseefried/sites/seanseefried.com/public/games/gyruss-elm'