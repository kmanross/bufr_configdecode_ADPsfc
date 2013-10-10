#!/bin/sh
#  ------------------------------------------------------------------------
#  This script will make bufrsfc.x and dumpbufr.x which extract data
#  from ADP BUFR input files, and place the data into a basic text file.
#  bufrsurface.x:  used to extract data from gdas.adpsfc.tHHz.YYYYMMDD.bufr 
#              and gdas.sfcshp.tHHz.YYYYMMDD.bufr files.
#  dumpbufr.x:        used to dump all contents of a BUFR file.
#  ** Make sure the "ar" command location has been set in your path
#  environment variable.  Type "which ar" to check if this is done. **
#  ------------------------------------------------------------------------
 
set -eua
 
#  ------------------------------------------------------------------------
#  CPLAT - platform type (linux,sgi,aix,sun)
#  ------------------------------------------------------------------------
 
CPLAT=linux
SRC=../src
LIB=../lib
EXE=../exe

#  different platforms use different link name protocols
#  -----------------------------------------------------

# if using linux, BUFR files must be run through the "grabbufr/grabbufr.sh" script
# with the resulting output used as input for the decoders.  Set appropriate compiler
# in grabbufr.sh, and exe/convert.csh
 
cflag=""
fflag=""

if [ $CPLAT = linux ]
then
   openrb=openrb_
   openwb=openwb_
   crdbfr=crdbufr_
   cwrbfr=cwrbufr_
   lenmsg=lenm_
#
#  ff=g77
   ff=g95
#  ff=gfortran
#
#  fflag="-fno-second-underscore -fsloppy-char"
   fflag="-fno-second-underscore -w -fbounds-check"
#  fflag="-fno-second-underscore -w"
#
   cc=gcc
   cflag="-DUNDERSCORE -w"

elif [ $CPLAT = sgi ]
then
   openrb=openrb_
   openwb=openwb_
   crdbfr=crdbufr_
   cwrbfr=cwrbufr_
   lenmsg=lenm_
   ff=f77
   cc=cc
   cflag=-DUNDERSCORE
elif [ $CPLAT = aix ]
then
   openrb=openrb
   openwb=openwb
   crdbfr=crdbufr
   cwrbfr=cwrbufr
   lenmsg=lenm
   ff=f77
   cc=cc
elif [ $CPLAT = sun ]
then
   openrb=openrb_
   openwb=openwb_
   crdbfr=crdbufr_
   cwrbfr=cwrbufr_
   lenmsg=lenm_
   ff=f77
   cc=cc
   cflag=-DUNDERSCORE 
fi

#  Compile and archive the Bufr Library
#  ------------------------------------

$cc $cflag -c $cflag $LIB/*.c
$ff $fflag -c $LIB/*.f

ar crv $LIB/bufrlib.a *.o

rm *.o
 
#  Compile the decode programs
#  ---------------------------------------
 
$ff $fflag -c $SRC/bufrsurface.f
$ff $fflag -c $SRC/dumpbufr.f
 
#  link and load the executables
#  -----------------------------


$ff $fflag -o $EXE/bufrsurface.x bufrsurface.o $LIB/bufrlib.a
$ff $fflag -o $EXE/dumpbufr.x dumpbufr.o $LIB/bufrlib.a

#  clean up
#  --------

rm -f *.o
