#!/bin/csh

# Script to process all BUFR tar files located in "bufr_configdecode/bufrobs"
# If a file is gzipped, it will gunzip it before untarring.
#

# !! Edit procdir directory definition to reflect your local system !!

# set procdir=$HOME/bufr_configdecode
set procdir=$MY_WORK/datasets/ds461.0/bufr_configdecode
set CPLAT=other

# !!! Uncomment the following for linux !!!
set CPLAT=linux

if($CPLAT =~ "linux") then 
 set compiler=gfortran    ## set compiler=compiler_name, eg xlf for ibm-sp
 cd $procdir/grabbufr
 $compiler -w -o grabbufr grabbufr.f spbufr.f
endif

cd $procdir/bufrobs

set z=0

foreach file (gdassfcobs.*????.tar*)

# gdassfcobs.yyyymmdd.tar.gz
  set fntar = `echo "$file" | cut -c1-23`
  if ("$file" =~ *.gz)  then
    if (! -e $fntar)  then
      gunzip $file   # gunzip before untarring  # gunzip removes the .gz file
    endif
  endif
  tar -xvf $fntar    # tar does not remove the .tar file
end

foreach dir (sfcobs.*????)

  set date=`echo $dir | awk -F. '{print $2}'`

  foreach hh ("00" "06" "12" "18") 

    set datehh=$date$hh
    set hour=$hh"z"

    echo $datehh
    echo $hour
    cd $dir
    foreach type (adpsfc sfcshp)
      if ($CPLAT =~ "linux") then
        cp $procdir/grabbufr/grabbufr . 
#
#       we want to save the original data file to avoid not knowing whether it 
#       is flipped, because the flipped version has the same name
#         
        if (-e gdas.$type.t$hour.$date.bufr)  then
          if (-e gdas.$type.t$hour.$date.bufr_save)  then
            cp -p gdas.$type.t$hour.$date.bufr_save gdas.$type.t$hour.$date.bufr
          else
            cp -p gdas.$type.t$hour.$date.bufr gdas.$type.t$hour.$date.bufr_save
          endif
          wc -c gdas.$type.t$hour.$date.bufr | ./grabbufr gdas.$type.t$hour.$date.bufr $type.t$hour.le
          mv ${type}.t$hour.le ../gdas.${type}.t$hour.$date.bufr
        endif
      else
        if (! -e ../gdas.${type}.t$hour.$date.bufr)  then
          cp -p gdas.$type.t$hour.$date.bufr ..
        endif
      endif
    end
    cd ..
  end
end
