#!/bin/bash

SAVEIFS=$IFS
IFS=$(echo -en "\n\b")
set -m

threads=4   # Increase/decrease as necessary for how many threads you want of this

folder="/Users/Jack/Music/Music" # Where the file things are (that you want copying)
#destFolder="/Users/Jack/Desktop/SDBACK/Music"
destFolder="/Users/Jack/Desktop/Scratch"
#destFolder="/Volumes/BISHOP/Music"  # Where you want the files to go

foldEsc=$(echo $folder | sed -e 's/\//\\\//g')          # These two variables are for the `sed` command later
destFoldEsc=$(echo $destFolder | sed -e 's/\//\\\//g')

files=$(find $folder -name "*.flac" | sort)

mp3Convert() {
  baseFile=$1
  newFile=$(echo $baseFile | sed -e 's/.flac/.mp3/g' -e "s/$foldEsc/$destFoldEsc/g")
  echo $newFile | sed -e "s/$destFoldEsc//g"
  mkdir -p $(dirname $newFile)
  avconv -v quiet -i $baseFile -ab 320k -ac 2 -ar 44100 $newFile -y
}

for line in $files; do
  while [ $(jobs | wc -l) -ge $threads ] ; do sleep 1 ; done
  #echo "$(echo $(find $destFolder -name "*mp3" | wc -l) | sed -e 's/ //g')/$(echo $(find $folder -name "*flac" | wc -l) | sed -e 's/ //g')"
  #mp3Convert $line &
  baseFile=$line
  newFile=$(echo $baseFile | sed -e "s/.flac/.mp3/g; s/$foldEsc/$destFoldEsc/g")
  echo $newFile | sed -e "s/$destFoldEsc//g"
  mkdir -p $(dirname $newfile)
  avconv -v quiet -i $baseFile -ab 320k -ac 2 -ar 44100 $newFile -y &
done

IFS=$SAVEIFS

wait

echo "Done!"

# After writing this I'm not sure if sed's my favourite or least favourite function.
