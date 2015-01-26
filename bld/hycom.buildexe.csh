#! /bin/csh -f 

set objdir = $OBJROOT/ocn/obj
set srcdir = $OBJROOT/ocn/src
mkdir -p $srcdir
mkdir -p $objdir
cd $srcdir
cp -p -r $CODEROOT/ocn/hycom .
cd ./hycom/sorc
if ( -d $CODEROOT/ocn/hycom/sorc ) then
  cd $CODEROOT/ocn/hycom/sorc
else
  echo "error changing directory to $CODEROOT/ocn/hycom/sorc"
  exit 1
endif

gmake ARCH=intelIFC TYPE=nuopc USER_DEFS="-DHYCOM_IN_CESM" nuopc
if ($status) then
   echo "error executing gmake ARCH=intelIFC TYPE=nuopc USER_DEFS=-DHYCOM_IN_CESM nuopc"
   exit 2
endif

cp -p *.mod $objdir
cp -p *.mod $LIBROOT/include
cp -p libhycom_nuopc.a $LIBROOT/libocn.a
