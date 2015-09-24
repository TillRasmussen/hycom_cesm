#! /bin/csh -f 

set objdir = $OBJROOT/ocn/obj
set srcdir = $OBJROOT/ocn/src
mkdir -p $srcdir
mkdir -p $objdir
if ( -d $CODEROOT/ocn/hycom/sorc ) then
  cd $CODEROOT/ocn/hycom/sorc
else
  echo "error changing directory to $CODEROOT/ocn/hycom/sorc"
  exit 1
endif

if ($OCN_GRID == "gh72") then
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

cp -p *.mod $objdir
cp -p *.mod $LIBROOT/include
cp -p libhycom_nuopc.a $LIBROOT/libocn.a
