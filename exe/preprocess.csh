#!/bin/csh

# Script to process all BUFR tar files located in "bufr_configdecode/bufrobs"
# If a file is gzipped, it will gunzip it before untarring.
#

# !! Edit procdir directory definition to match the location where you
# !! put the software tar file before untarring it

set EXE=`pwd`
set OBS=$EXE/../bufrobs

cd $OBS
foreach file (gdassfcobs.*????.tar*)

# gdassfcobs.yyyymmdd.tar.gz
  if ("$file" =~ *.gz)  then
      tar -xvzf $file  
  else
      tar -xvf $file
  endif
end

foreach dir (sfcobs.*)
    cp $dir/gdas.* .
end

cd $EXE
