#!/bin/bash


USER=schnecki
PCS="c437-pc169" # " c437-pc161 c437-pc147"
DIR=`pwd`/

if [ ${PWD##*/} == "test" ]; then
    DIR=$DIR../
fi

# Sync  & Run
for PC in $PCS; do
  echo "SYNCING TO $USER@$PC:$DIR"
  rsync -tar -zz --del --force --exclude=.git --exclude=results --exclude=.stack-work --exclude=.stack-work.prof --exclude=state* --exclude=costs --exclude=plts --exclude=.state* --exclude=psiValues --exclude=.psiValues  --exclude=episodeLength --exclude=queueLength --exclude=reward $DIR $USER@$PC:$DIR
  if [ $? -ne 0 ]; then
      ssh -t $USER@$PC "mkdir -p $DIR 1>/dev/null"
      rsync -tar -zz --del --force --exclude=.git --exclude=.stack-work --exclude=state*  --exclude=costs --exclude=plts --exclude=.state* --exclude=psiValues --exclude=.psiValues --exclude=episodeLength --exclude=queueLength --exclude=reward $DIR $USER@$PC:$DIR/
  fi
  echo "Synced data via rsync. Result $?"
  if [ $? -eq 0 ]; then
      printf "Synced to $PC"
  else
      echo "Something went wrong while syncing to $PC. Check the connection"
  fi

done
