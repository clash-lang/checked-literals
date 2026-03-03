#!/usr/bin/env python3
import json
import subprocess

def main():
    with open("ghc-versions.json", "r") as f:
        ghcs = json.load(f)

    for ghc in ghcs:
        subprocess.run(["ghcup", "install", "ghc", ghc])

if __name__ == "__main__":
    main()
