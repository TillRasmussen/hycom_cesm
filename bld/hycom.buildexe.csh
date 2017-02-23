#! /bin/csh -f 

set objdir = $OBJROOT/ocn/obj
set cfgdir = $OBJROOT/ocn/config
set my_path = $CASEROOT/SourceMods/src.hycom
mkdir -p $cfgdir
mkdir -p $objdir
if ( -d $CODEROOT/ocn/hycom/sorc ) then
#  cd $CODEROOT/ocn/hycom/sorc
    cd $objdir
    cp $CODEROOT/ocn/hycom/sorc/Makefile .
    cp $CODEROOT/ocn/hycom/sorc/*.F .
    cp $CODEROOT/ocn/hycom/sorc/*.F90 .
    cp $CODEROOT/ocn/hycom/sorc/*.h .
    cp $CODEROOT/ocn/hycom/sorc/*.c .
    cp $CODEROOT/ocn/hycom/sorc/*.f .
    cp $CODEROOT/ocn/hycom/config/* $cfgdir/.
else
  echo "error changing directory to $CODEROOT/ocn/hycom/sorc"
  exit 1
endif

if (-d $my_path ) then
    cp -fp $my_path/*.F90 $objdir/.
    cp -fp $my_path/*.F   $objdir/.
    cp -fp $my_path/*.f   $objdir/.
    cp -fp $my_path/*.h   $objdir/.
    cp -fp $my_path/*.c   $objdir/.
    cp -fp $my_path/Makefile   $objdir/.
endif

if ($OCN_GRID == "gh72"  || $OCN_GRID == "tx0.1v2" || $OCN_GRID == "gh08") then
  gmake ARCH=Aintelrelo TYPE=nuopc USER_DEFS="-DHYCOM_IN_CESM" nuopc
  if ($status) then
     echo "error executing gmake ARCH=Aintelrelo TYPE=nuopc USER_DEFS=-DHYCOM_IN_CESM nuopc"
     exit 2
  endif
else
  gmake ARCH=intelrelo TYPE=nuopc USER_DEFS="-DHYCOM_IN_CESM" nuopc
  if ($status) then
     echo "error executing gmake ARCH=intelrelo TYPE=nuopc USER_DEFS=-DHYCOM_IN_CESM nuopc"
     exit 2
  endif
endif

#cp -p *.mod $objdir
cp -p *.mod $LIBROOT/include
cp -p libhycom_nuopc.a $LIBROOT/libocn.a
