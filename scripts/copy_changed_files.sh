#!/bin/bash

# Copies modified files to an identical copy of this project in another directory.
# Current version does nothing with deleted, added, and untracked files.
#
# Intended for synching files to a RAMdisk for faster building.
# 
# Run this from git project root.
#
# Usage: <name of script> target_dir


changed_files=$(git status --porcelain | grep "^ M" | sed 's/ M //')

for f in $changed_files
do
    echo "Coping $f"
    cp $f "$1/$f"
done


