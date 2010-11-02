#!/bin/sh

OCBUILD="/opt/godi/bin/ocamlbuild";
TARGETS="todolist.cma";


cleanup() {
  $OCBUILD -clean;
  rm -f *.annot;
}

produce() {
   $OCBUILD $TARGETS;
   for fname in _build/*.annot
  do
    bname=$(basename $fname);
    if [ ! -e $bname ] 
    then
      ln -s $fname $bname;
    fi
  done
}

rule() {
  case $1 in
    clean)  cleanup;;
#    native) produce native;;
#   byte)   produce byte;;
    all)    produce native byte;;
    depend) echo "Not needed.";;
    *)      echo "Unknown action $1";;
  esac;
}

if [ $# -eq 0 ]; then
  rule all
else
  while [ $# -gt 0 ]; do
    rule $1;
    shift
  done
fi
	
