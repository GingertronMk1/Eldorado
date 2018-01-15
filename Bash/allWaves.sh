#!/bin/bash

SAVEIFS=$IFS
IFS=$(echo -en "\n\b")
set -m

folder="/Users/Jack/Music/Music"

files=$(find $folder -name "*.flac" | sort)

waveformConvert() {
  newFile=$(echo $1 | sed -e 's/flac/png/g; s/Music\/Music/Desktop\/Scratch/g')
  echo $newFile
  mkdir -p $(dirname $newFile)
  ffmpeg -i $1 -filter_complex "showwavespic=s=3840x2400" -frames:v 1 $newFile
}

for line in $files; do
  waveformConvert $line
done

IFS=$SAVEIFS
