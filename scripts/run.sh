#!/bin/bash

echo "[*] Building notybot app:"

stack build || { echo "Build failed!!!"; exit 1; }

echo "[*] Running notybot app:"

stack exec notybot-exe
