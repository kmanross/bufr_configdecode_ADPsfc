#!/bin/sh
#  ------------------------------------------------------------------------
#  This script will make bufrland.x and bufrship.x which extract data
#  from ADP BUFR input files, and place the data into a basic text file.
#  bufradpsfc.x:  used to extract data from gdas.adpsfc.tHHz.YYYYMMDD.bufr files.
#  bufrsfcship.x  used to extract data from gdas.sfcshp.tHHz.YYYYMMDD.bufr files.
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
   cflag="-DUNDERSCORE -w "
   fflag="-DUNDERSCORE -fno-second-underscore -w "
#   fflag="-DUNDERSCORE -ftrace=frame"
#  fflag="-fno-second-underscore -fsloppy-char"     # deactivated 2011.04.29, in conjunction with other changes this day
#   cc=gcc; ff=g95   # deactivated 2011.04.21
#   cc=icc; ff=ifort  # added/activated 2011.04.21
# uncomment following if ff=gfortran #
#   fflag="-fno-second-underscore"
   cc=gcc; ff=gfortran
elif [ $CPLAT = sgi ]
then
   openrb=openrb_
   openwb=openwb_
   crdbfr=crdbufr_
   cwrbfr=cwrbufr_
   lenmsg=lenm_
   cflag=-DUNDERSCORE
   cc=cc; ff=f77
elif [ $CPLAT = aix ]
then
   openrb=openrb
   openwb=openwb
   crdbfr=crdbufr
   cwrbfr=cwrbufr
   lenmsg=lenm
   cc=cc; ff=f77
elif [ $CPLAT = sun ]
then
   openrb=openrb_
   openwb=openwb_
   crdbfr=crdbufr_
   cwrbfr=cwrbufr_
   lenmsg=lenm_
   cflag=-DUNDERSCORE 
   cc=cc; ff=f77
fi

#  Compile and archive the Bufr Library
#  ------------------------------------

 $cc $cflag -c $cflag $LIB/*.c        # as of 2011.04.29, 
# $cc $cflag -c $cflag $LIB/libbufr.a  # already have the bufrlib.a file, which includes both C & Fortran routines
 $ff $fflag -c $LIB/*.f

 ar crv $LIB/bufrlib.a *.o   # as of 2011.04.29, already have the bufrlib.a file
 rm *.o

#LIB=/glade/home/dss/lib/i_compiler_lib
 
#  Compile the decode programs
#  ---------------------------------------
 
# $ff $fflag -c $SRC/bufradpsfc.f
# $ff $fflag -c $SRC/bufrsfcship.f 
# $ff $fflag -c $SRC/dumpbufr.f
 
$ff $fflag -c $SRC/bufrsurface.f    # switched to this 2011.04.29

#  link and load the executables
#  -----------------------------


# $ff $fflag -o $EXE/bufradpsfc.x bufradpsfc.o $LIB/bufrlib.a
# $ff $fflag -o $EXE/bufrsfcship.x bufrsfcship.o $LIB/bufrlib.a
# $ff $fflag -o $EXE/dumpbufr.x dumpbufr.o $LIB/bufrlib.a

$ff $fflag -o $EXE/bufrsurface.x bufrsurface.o $LIB/bufrlib.a    # switched to this 2011.04.29 

#  clean up
#  --------

rm -f *.o
